package mill.bspserver

import ammonite.ops._
import ammonite.util.Colors

import coursier.Cache
import mill._
import mill.util.{Ctx, TestEvaluator, TestUtil}
import mill.util.PrintLogger

import utest._
import utest.framework.TestPath

object BspServerTests extends TestSuite {

  // val millSourcePath = pwd / 'target / 'workspace / "bspServer"
  val resourcePath = pwd / 'bspserver / 'test / 'resources / "main"

  def workspaceTest[T, M <: TestUtil.BaseModule](m: M, resourcePath: Path = resourcePath)
                                                (t: TestEvaluator[M] => T)
                                                (implicit tp: TestPath): T = {
    val eval = new TestEvaluator(m)
    rm(m.millSourcePath)
    rm(eval.outPath)
    mkdir(m.millSourcePath / up)
    cp(resourcePath, m.millSourcePath)
    t(eval)
  }

  object Base extends TestUtil.BaseModule {
    def millSourcePath =  TestUtil.getSrcPathBase() / millOuterCtx.enclosing.split('.')
    // def millSourcePath = BspServerTests.millSourcePath

    object bspTestCore extends scalalib.ScalaModule {
      def scalaVersion = "2.12.4"

      object test extends super.Tests {
        def testFrameworks = Seq("utest.runner.Framework")
      }
    }
  }

  // val helloWorldEvaluator = TestEvaluator.static(HelloWorld)

  def tests: Tests = Tests {
    'bspServerTests - {
      // val homeStr = "/home/smarter"
      // val colored = "true"
      // val ctx = new Ctx.Log with Ctx.Home {
      //   val log = PrintLogger(
      //     colored == "true",
      //     true,
      //     if(colored == "true") Colors.Default
      //     else Colors.BlackWhite,
      //     System.out,
      //     System.err,
      //     System.err,
      //     System.in
      //   )
      //   val home = Path(homeStr)
      // }
      // val server = new Server(ctx, Base)

      workspaceTest(Base) { eval =>
        val in = new java.io.PipedInputStream
        val out = new java.io.PipedOutputStream
        out.connect(in)
        val io = new scala.meta.jsonrpc.InputOutput(in, out)

        val server = new Server(eval.evaluator, io)
        server.run()
      }
    }
  }
}
