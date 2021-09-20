package tests

import scala.util.Failure
import scala.util.Success

import scala.meta.internal.metals.ServerCommands

class BspBuildChangedSuite
    extends BaseLspSuite("bsp-build-changed", ScalaCliBuildInitializer) {

  private def isInterruptedException(t: Throwable): Boolean =
    t != null && (
      t.isInstanceOf[InterruptedException] ||
        isInterruptedException(t.getCause)
    )

  test("build changed") {
    cleanWorkspace()
    for {
      _ <- initialize(
        """/MyTests.scala
          |import $ivy.`com.lihaoyi::utest::0.7.10`, utest._
          |object MyTests extends TestSuite {
          |  pprint.log(2)
          |  val tests = Tests {
          |    test("foo") {
          |      assert(2 + 2 == 4)
          |    }
          |    test("nope") {
          |      assert(2 + 2 == 5)
          |    }
          |  }
          |}
          |""".stripMargin
      )
      _ <- server.executeCommand(ServerCommands.ConnectBuildServer.id)
      _ <- server.didOpen("MyTests.scala")
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        """
          |MyTests.scala:3:3: error: Not found: pprint
          |  pprint.log(2)
          |  ^^^^^^
        """.stripMargin
      )
      _ <- server
        .didSave("MyTests.scala")(_ =>
          """import $ivy.`com.lihaoyi::utest::0.7.10`, utest._
            |import $ivy.`com.lihaoyi::pprint::0.6.6`
            |object MyTests extends TestSuite {
            |  pprint.log(2)
            |  val tests = Tests {
            |    test("foo") {
            |      assert(2 + 2 == 4)
            |    }
            |    test("nope") {
            |      assert(2 + 2 == 5)
            |    }
            |  }
            |}
            |""".stripMargin
        )
        .transform {
          case Success(()) => Success(())
          case Failure(t) =>
            // Seems build change triggers reloading, which interrupts things. We ignore that here.
            if (isInterruptedException(t)) Success(())
            else Failure(t)
        }
      _ = assertNoDiff(client.workspaceDiagnostics, "")
    } yield ()
  }

}
