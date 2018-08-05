package mill.bspserver

import java.io._
import java.net.Socket

import scala.collection.JavaConverters._

import mill.eval.Evaluator
import mill.util.DummyInputStream

import io.circe.Json        // to construct Json values
import scala.meta.jsonrpc._ // for JSON-RPC APIs
// import scala.meta.lsp._     // for LSP endpoints
import scribe._             // for logging

import ch.epfl.scala.bsp._
import ch.epfl.scala.bsp.endpoints._

import monix.eval.Task

object BspServerMain {
  def main(args0: Array[String]): Unit = {
    val server = new Server
    server.run()
  }
}

class Server {
  def myServices(logger: LoggerSupport, client: LanguageClient): Services = {
    Services
      .empty(logger)
      .request(Build.initialize) { params =>
        logger.info(params.toString)
        InitializeBuildResult(BuildServerCapabilities(
          compileProvider = CompileProvider(List("scala")),
          testProvider = TestProvider(Nil),
          runProvider = RunProvider(Nil),
          textDocumentBuildTargetsProvider = false,
          dependencySourcesProvider = false,
          resourcesProvider = false,
          buildTargetChangedProvider = false
        ))
      }
      .notification(Build.initialized) { _ =>
        logger.info("Client is initialized")
      }
      .request(Build.shutdown) { _ =>
        logger.info("Client is about to call exit soon")
      }
      .notification(Build.exit) { _ =>
        logger.info("Goodbye!")
        System.exit(0)
      }
  }

  // def testClient(logger: LoggerSupport): Services = {
  //   Services
  //     .empty(logger)

  // }

  def run() = {
    // TODO: pool size
    implicit val scheduler =
      monix.execution.Scheduler(java.util.concurrent.Executors.newFixedThreadPool(2))

    val io = new InputOutput(System.in, System.out)

    // val clientLogger = Logger("client-logger").withHandler(writer = scribe.writer.FileWriter.simple())

    val serverLogger = Logger("bsp-server").withHandler(writer = scribe.writer.FileWriter.simple())
    val clientLogger = Logger("bsp-client").withHandler(writer = scribe.writer.FileWriter.simple())

    val source = new PipedOutputStream
    val sink = new PipedInputStream
    sink.connect(source)

    implicit val client =
      LanguageClient.fromOutputStream(/*io.out*/ source, clientLogger)

    val messages =
      BaseProtocolMessage.fromInputStream(/*io.in*/ sink, serverLogger)
    val server =
      new LanguageServer(messages, client, myServices(serverLogger, client), scheduler, serverLogger)

    val connection = Connection(client, server.startTask.executeWithFork.runAsync)

    Build.shutdown.request(Shutdown())
    for {
      res <- Build.initialize.request(InitializeBuildParams(Uri("file:///bla"), BuildClientCapabilities(languageIds = List("scala"), providesFileWatching = false))).runAsync
      _ <- Task(println(res))
      // _ <- Task(Build.initialized.notify()).runAsync
      _ <- Task(Build.shutdown.request(Shutdown())).runAsync
    } {}

    // scala.concurrent.Await.result(
    //   connection.server,
    //   scala.concurrent.duration.Duration.Inf
    // )
  }
}
