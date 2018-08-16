package mill.scalalib.worker

import java.io.File
import java.util.Optional

import ammonite.ops.{Path, exists, ls, mkdir}
import ammonite.util.Colors
import mill.Agg
import mill.eval.PathRef
import mill.scalalib.{CompilationResult, Lib, TestRunner}
import xsbti.compile.{CompilerCache => _, FileAnalysisStore => _, ScalaInstance => _, _}
import mill.scalalib.Dep.isDotty
import mill.scalalib.Lib.{grepJar, scalaBinaryVersion}
import mill.util.{Ctx, Diagnostic, Location, Position, PrintLogger, Range}
import sbt.internal.inc._
import sbt.internal.util.{ConsoleOut, MainAppender}
import sbt.util.LogExchange

import scala.collection.mutable

case class MockedLookup(am: File => Optional[CompileAnalysis]) extends PerClasspathEntryLookup {
  override def analysis(classpathEntry: File): Optional[CompileAnalysis] =
    am(classpathEntry)

  override def definesClass(classpathEntry: File): DefinesClass =
    Locate.definesClass(classpathEntry)
}

class ScalaWorker(ctx0: mill.util.Ctx,
                  compilerBridgeClasspath: Array[String]) extends mill.scalalib.ScalaWorkerApi{
  @volatile var compilersCache = Option.empty[(Long, Compilers)]

  /** Compile the bridge if it doesn't exist yet and return the output directory.
   *  TODO: Proper invalidation, see #389
   */
  def compileZincBridgeIfNeeded(scalaVersion: String,
                                sourcesJar: Path,
                                compilerJars: Array[File]): Path = {
    val workingDir = ctx0.dest / scalaVersion
    val compiledDest = workingDir / 'compiled
    if (!exists(workingDir)) {

      ctx0.log.info("Compiling compiler interface...")

      mkdir(workingDir)
      mkdir(compiledDest)

      val sourceFolder = mill.modules.Util.unpackZip(sourcesJar)(workingDir)
      val classloader = mill.util.ClassLoader.create(compilerJars.map(_.toURI.toURL), null)(ctx0)
      val compilerMain = classloader.loadClass(
        if (isDotty(scalaVersion))
          "dotty.tools.dotc.Main"
        else
          "scala.tools.nsc.Main"
      )
      val argsArray = Array[String](
        "-d", compiledDest.toString,
        "-classpath", (compilerJars ++ compilerBridgeClasspath).mkString(File.pathSeparator)
      ) ++ ls.rec(sourceFolder.path).filter(_.ext == "scala").map(_.toString)

      compilerMain.getMethod("process", classOf[Array[String]])
        .invoke(null, argsArray)
    }
    compiledDest
  }



  def discoverMainClasses(compilationResult: CompilationResult)(implicit ctx: mill.util.Ctx): Seq[String] = {
    def toScala[A](o: Optional[A]): Option[A] = if (o.isPresent) Some(o.get) else None

    toScala(FileAnalysisStore.binary(compilationResult.analysisFile.toIO).get())
      .map(_.getAnalysis)
      .flatMap{
        case analysis: Analysis =>
          Some(analysis.infos.allInfos.values.map(_.getMainClasses).flatten.toSeq.sorted)
        case _ =>
          None
      }
      .getOrElse(Seq.empty[String])
  }


  def compileScala(scalaVersion: String,
                   sources: Agg[Path],
                   compilerBridgeSources: Path,
                   compileClasspath: Agg[Path],
                   compilerClasspath: Agg[Path],
                   scalacOptions: Seq[String],
                   scalacPluginClasspath: Agg[Path],
                   javacOptions: Seq[String],
                   upstreamCompileOutput: Seq[CompilationResult])
                  (implicit ctx: mill.util.Ctx): mill.eval.Result[CompilationResult] = {
    val compileClasspathFiles = compileClasspath.map(_.toIO).toArray
    val compilerJars = compilerClasspath.toArray.map(_.toIO)

    val compilerBridge = compileZincBridgeIfNeeded(scalaVersion, compilerBridgeSources, compilerJars)

    val ic = new sbt.internal.inc.IncrementalCompilerImpl()

    val compilerBridgeSig = compilerBridge.mtime.toMillis
    val compilersSig = compilerBridgeSig + compilerClasspath.map(p => p.toString().hashCode + p.mtime.toMillis).sum
    val compilers = compilersCache match {
      case Some((k, v)) if k == compilersSig => v
      case _ =>
        val compilerName =
          if (isDotty(scalaVersion))
            s"dotty-compiler_${scalaBinaryVersion(scalaVersion)}"
          else
            "scala-compiler"
        val scalaInstance = new ScalaInstance(
          version = scalaVersion,
          loader = mill.util.ClassLoader.create(compilerJars.map(_.toURI.toURL), null),
          libraryJar = grepJar(compilerClasspath, "scala-library", scalaVersion).toIO,
          compilerJar = grepJar(compilerClasspath, compilerName, scalaVersion).toIO,
          allJars = compilerJars,
          explicitActual = None
        )
        val compilers = ic.compilers(
          scalaInstance,
          ClasspathOptionsUtil.boot,
          None,
          ZincUtil.scalaCompiler(scalaInstance, compilerBridge.toIO)
        )
        compilersCache = Some((compilersSig, compilers))
        compilers
    }

    mkdir(ctx.dest)

    val logger = {
      val consoleAppender = MainAppender.defaultScreen(ConsoleOut.printStreamOut(
        ctx.log.outputStream
      ))
      val l = LogExchange.logger("Hello")
      LogExchange.unbindLoggerAppenders("Hello")
      LogExchange.bindLoggerAppenders("Hello", (consoleAppender -> sbt.util.Level.Info) :: Nil)
      l
    }

    def analysisMap(f: File): Optional[CompileAnalysis] = {
      if (f.isFile) {
        Optional.empty[CompileAnalysis]
      } else {
        upstreamCompileOutput.collectFirst {
          case CompilationResult(zincPath, classFiles) if classFiles.path.toNIO == f.toPath =>
            FileAnalysisStore.binary(zincPath.toIO).get().map[CompileAnalysis](_.getAnalysis)
        }.getOrElse(Optional.empty[CompileAnalysis])
      }
    }

    val lookup = MockedLookup(analysisMap)

    val zincFile = ctx.dest / 'zinc
    val classesDir = ctx.dest / 'classes

    val zincIOFile = zincFile.toIO
    val classesIODir = classesDir.toIO

    val store = FileAnalysisStore.binary(zincIOFile)

    try {
      val newResult = ic.compile(
        ic.inputs(
          classpath = classesIODir +: compileClasspathFiles,
          sources = sources.toArray.map(_.toIO),
          classesDirectory = classesIODir,
          scalacOptions = (scalacPluginClasspath.map(jar => s"-Xplugin:${jar}") ++ scalacOptions).toArray,
          javacOptions = javacOptions.toArray,
          maxErrors = 10,
          sourcePositionMappers = Array(),
          order = CompileOrder.Mixed,
          compilers = compilers,
          setup = ic.setup(
            lookup,
            skip = false,
            zincIOFile,
            new FreshCompilerCache,
            IncOptions.of(),
            new MillReporter(ctx.log),
            None,
            Array()
          ),
          pr = {
            val prev = store.get()
            PreviousResult.of(prev.map(_.getAnalysis), prev.map(_.getMiniSetup))
          }
        ),
        logger = logger
      )

      store.set(
        AnalysisContents.create(
          newResult.analysis(),
          newResult.setup()
        )
      )

      mill.eval.Result.Success(CompilationResult(zincFile, PathRef(classesDir)))
    }catch{case e: CompileFailed => mill.eval.Result.Failure(e.toString)}
  }
}

class MillReporter(logger: mill.util.Logger)
    extends xsbti.Reporter {

  private[this] var errorCount = 0
  private[this] var warningCount = 0
  private[this] val allProblems = new mutable.ArrayBuffer[xsbti.Problem]

  override def reset(): Unit = {
    errorCount = 0
    warningCount = 0
  }

  override def hasErrors: Boolean = errorCount > 0
  override def hasWarnings: Boolean = warningCount > 0

  override def printSummary: Unit = {
    if (warningCount > 0)
      logger.info(s"$warningCount warning(s) found")
    if (errorCount > 0)
      logger.error(s"$errorCount error(s) found")
  }

  override def problems: Array[xsbti.Problem] = allProblems.toArray

  override def log(problem: xsbti.Problem): Unit = {
    val sbtPos = problem.position
    val path = toOption(sbtPos.sourcePath).map(Path(_))

    val pos = toOption(sbtPos.line).flatMap(l =>
      toOption(sbtPos.pointer).map(c => Position(l, c)))
    // Zinc does not record the start/end of a message currently, just a point.
    val range = pos.map(p => Range(p, p))
    val location = path.map(Location(_, range))

    val severity = {
      import xsbti.{Severity => S}
      import mill.util.{DiagnosticSeverity => DS}
      problem.severity match {
        case S.Error => DS.Error
        case S.Warn  => DS.Warning
        case S.Info  => DS.Information
      }
    }

    // Zinc does not record diagnostic codes currently.
    val code = None

    val diagnostic = Diagnostic(location, severity, code, problem.message)

    logger.report(diagnostic)
  }

  // TODO: what is this for ?
  override def comment(pos: xsbti.Position, msg: String): Unit = {}

  private def toOption[T](x: Optional[T]): Option[T] =
    if (x.isPresent) Some(x.get) else None
}
