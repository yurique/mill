package mill.scalaworker
import java.net.URLClassLoader

import org.objectweb.asm.{ClassReader, ClassWriter}
import utest._

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.ClassPath


object IJarTests extends TestSuite{
  def makeFile(src: Array[Byte], name: String) = {
    assert(!name.contains('/'))
    val singleFile = new scala.tools.nsc.io.VirtualFile(name)
    val output = singleFile.output
    output.write(src)
    output.close()
    singleFile
  }

  def compile(code: String) = {
    val vd = new VirtualDirectory("(memory)", None)
    lazy val settings = new Settings
    val loader = getClass.getClassLoader.asInstanceOf[URLClassLoader]
    val entries = loader.getURLs map(_.getPath)
    settings.outputDirs.setSingleOutput(vd)

    // annoyingly, the Scala library is not in our classpath, so we have to add it manually
    val sclpath = entries.map(
      _.replaceAll("scala-compiler.jar", "scala-library.jar")
    )

    settings.classpath.value = ClassPath.join(entries ++ sclpath : _*)
    val compiler = new Global(settings, new ConsoleReporter(settings))
    val run = new compiler.Run()
    val abstractFile = makeFile(code.getBytes, "foo.scala")
    run.compileFiles(List(abstractFile))

    vd.map(f => (f.name, f.toByteArray)).toList
  }
  def ijarify(bytes: Array[Byte]) = {
    val cr = new ClassReader(bytes)
    val cw = new ClassWriter(0)
    val cv = new MethodReplacer(cw)
    cr.accept(cv, 0)
    cw.toByteArray.toSeq
  }
  def compileStrip(source: String) = {
    val res = compile(source).map{case (k, v) => (k, ijarify(v))}
    for((k, v) <- res) ammonite.ops.write.over(
      ammonite.ops.pwd/math.abs(source.hashCode).toString/k,
      v.toArray
    )
    res
  }
  def tests = Tests{
    'pos - {
      'literal - {
        val i1 = compileStrip("class Test{ def x = 1 }")
        val i2 = compileStrip("class Test{ def x = 2 }")
        assert(i1 == i2)
      }
      'statements - {
        val i1 = compileStrip("class Test{ def x = 1 }")
        val i2 = compileStrip("class Test{ def x = {println(1); 2} }")
        assert(i1 == i2)
      }
      'privateMethods - {
        val i1 = compileStrip("class Test{ def x = 1 }")
        val i2 = compileStrip("class Test{ private[this] def y = 1; def x = 1 }")
        assert(i1 == i2)
      }
      'privateFields - {
        val i1 = compileStrip("class Test(y: Int){ def x = 1 }")
        val i2 = compileStrip("class Test(y: Int){ def x = y }")
        assert(i1 == i2)
      }
    }
    'neg - {
      'name - {
        val i1 = compileStrip("class Test{ def x = 1 }")
        val i2 = compileStrip("class Test{ def y = 1 }")
        assert(i1 != i2)
      }
      'type - {
        val i1 = compileStrip("class Test{ def x = 1 }")
        val i2 = compileStrip("class Test{ def x = 1L }")
        assert(i1 != i2)
      }
      'args - {
        val i1 = compileStrip("class Test{ def x = 1 }")
        val i2 = compileStrip("class Test{ def x(i: Int) = 1 }")
        assert(i1 != i2)
      }
      'superclass - {
        val i1 = compileStrip("class Test{ def x = 1 }")
        val i2 = compileStrip("class Test extends Serializable{ def x = 1 }")
        assert(i1 != i2)
      }
    }
  }
}
