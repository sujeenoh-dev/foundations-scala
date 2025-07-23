/**
 * 함수형 스칼라에서 프로그램은 외부 세계와 직접 상호작용하지 않습니다.
 * 대신, 외부 세계에서 입력을 처리하고 출력을 생성하는 방법을 기술하는
 * "청사진"을 구성합니다. _무엇을_ 할 것인지를 기술하는 모델과 _어떻게_ 할 것인지를
 * 기술하는 실행 사이의 이러한 분리는 표현력을 증가시키며, 이것이 함수형
 * 프로그래밍이 제대로 구축하기 어려운 리액티브 애플리케이션의 설계를
 * 혁신하는 이유입니다.
 */
package net.degoes

import zio._
import zio.test._
import zio.test.TestAspect._
import java.io.IOException

object Effects extends ZIOSpecDefault {
  def spec =
    suite("Effects") {
      suite("constructors") {

        /**
         * 연습문제
         *
         * `ZIO.succeed`를 사용하여 값 `42`로 성공하는 이팩트를
         * 구성하세요.
         */
        test("succeed") {
          def effect: ZIO[Any, Nothing, Int] = ???

          for {
            result <- effect
          } yield assertTrue(result == 42)
        } @@ ignore +
          /**
           * 연습문제
           *
           * `ZIO.fromEither`를 사용하여 제공된 either 값으로부터
           * 이팩트를 구성하세요.
           */
          test("fromEither (success)") {
            val either: Either[String, Int] = Right(42)

            val _ = either

            def effect: ZIO[Any, String, Int] = ???

            for {
              result <- effect
            } yield assertTrue(result == 42)
          } @@ ignore +
          /**
           * 연습문제
           *
           * `ZIO.fromOption`을 사용하여 제공된 option 값으로부터
           * 이팩트를 구성하세요.
           */
          test("fromOption (success)") {
            val option: Option[Int] = Some(42)

            val _ = option

            def effect: ZIO[Any, Option[Nothing], Int] = ???

            for {
              result <- effect
            } yield assertTrue(result == 42)
          } @@ ignore
      } +
        suite("operators") {

          /**
           * 연습문제
           *
           * `ZIO#map`을 사용하여 `Console.readLine` 이팩트의 성공 값을
           * 콘솔에서 읽은 텍스트 라인의 길이를 나타내는 정수로 맵핑하세요.
           */
          test("map") {
            val readInt: IO[IOException, Int] = ???

            for {
              _   <- TestConsole.feedLines("Sherlock")
              int <- readInt
            } yield assertTrue(int == 8)
          } @@ ignore +
            /**
             * 연습문제
             *
             * `ZIO#mapError`를 사용하여 제공된 이팩트의 정수 실패를
             * 정수의 문자열 표현으로 변환하세요.
             */
            test("mapError") {
              val errorCode = 42

              val failure = ZIO.fail(errorCode)

              val mappedFailure: IO[String, Nothing] = failure.mapError(???)

              for {
                value <- mappedFailure.flip
              } yield assertTrue(value == "42")
            } @@ ignore +
            /**
             * 연습문제
             *
             * `ZIO#zip`을 사용하여 제공된 두 이팩트를 순차적으로 결합하세요.
             */
            test("zip") {
              val first = Console.printLine("Sherlock")
              val last  = Console.printLine("Holmes")

              val _ = first
              val _ = last

              val zipped: ZIO[Any, IOException, Unit] = ???

              for {
                _      <- zipped
                output <- TestConsole.output
              } yield assertTrue(output == Vector("Sherlock\n", "Holmes\n"))
            } @@ ignore +
            /**
             * 연습문제
             *
             * `ZIO.*>`를 사용하여 제공된 두 이팩트를 순차적으로 연결하되,
             * 우측의 성공 값을 반환하세요.
             */
            test("*>") {
              val first  = ZIO.succeed(42)
              val second = ZIO.succeed("Roger Rabbit")

              val _ = first
              val _ = second

              val zipLeft: UIO[String] = ???

              for {
                result <- zipLeft
              } yield assertTrue(result == "Roger Rabbit")
            } @@ ignore +
            /**
             * 연습문제
             *
             * `ZIO.<*`를 사용하여 제공된 두 이팩트를 순차적으로 연결하되,
             * 좌측의 성공 값을 반환하세요.
             */
            test("<*") {
              val first  = ZIO.succeed(42)
              val second = ZIO.succeed("Roger Rabbit")

              val _ = first
              val _ = second

              val zipLeft: UIO[Int] = ???

              for {
                result <- zipLeft
              } yield assertTrue(result == 42)
            } @@ ignore +
            /**
             * 연습문제
             *
             * `ZIO#flatMap`, `Console.printLine`, `Console.readLine`을 사용하여
             * "이름이 무엇인가요?"를 출력하고, 사용자의 이름을 읽은 다음,
             * 마지막으로 "Hello, <이름>!"을 출력하는 프로그램을 작성하세요.
             * 여기서 <이름>은 사용자의 이름입니다.
             */
            test("flatMap") {

              def program: ZIO[Any, IOException, Unit] = ???

              val expected =
                Vector("What is your name?\n", "Sherlock\n", "Hello, Sherlock!\n")

              for {
                _      <- TestConsole.feedLines("Sherlock")
                _      <- program
                output <- TestConsole.output
              } yield assertTrue(output == expected)
            } @@ ignore +
            /**
             * 연습문제
             *
             * `ZIO.catchAll`과 `ZIO.succeed`를 사용하여 실패한 이팩트로부터
             * 복구하여 문자열 "Recovered!"로 성공하도록 하세요.
             */
            test("catchAll") {
              def effect: ZIO[Any, Nothing, String] =
                ZIO.fail("Uh oh!").FIXME

              for {
                value <- effect
              } yield assertTrue(value == "Recovered!")
            } @@ ignore +
            /**
             * 연습문제
             *
             * `ZIO#foldZIO`를 사용하여 제공된 이팩트의 오류와 성공 경우 모두를
             * 처리하여 상수 문자열 "Did it!"을 생성하세요.
             */
            test("foldZIO") {
              def effect: ZIO[Any, Nothing, String] =
                ZIO.fail("Failure").FIXME

              for {
                value <- effect
              } yield assertTrue(value == "Did it!")
            } @@ ignore +
            /**
             * 연습문제
             *
             * 재귀를 사용하여 조건(predicate)이 true를 반환하는 동안만
             * (그보다 더 오래 말고) 상태 값에 대해 반복하는 `iterate` 함수를 구현하세요.
             */
            test("recursion") {
              def iterate[R, E, S](start: S)(pred: S => Boolean)(f: S => ZIO[R, E, S]): ZIO[R, E, S] = ???

              val iterationResult =
                iterate(List.empty[String])(_.length < 3) { list =>
                  ZIO.succeed("a" :: list)
                }

              for {
                list <- iterationResult
              } yield assertTrue(list == List("a", "a", "a"))
            } @@ ignore +
            /**
             * 연습문제
             *
             * ZIO를 사용하여 다양한 이팩트 타입들을 결합할 수 있도록 하세요.
             */
            test("mixed") {
              import scala.util._

              final case class UnknownUserError(message: String) extends Exception(message)
              case class NoPreferencesError()                    extends Exception("Preferences could not be loaded")

              type User  = String
              type Docs  = List[String]
              type Prefs = Map[String, Boolean]

              def getUser(): Either[String, User] = Right("sherlock@holmes.com")
              def getDocs(): Try[Docs]            = Try(List("Doc 1", "Doc 2"))
              def getPrefs(): Option[Prefs]       = Some(Map("autosave" -> true))

              def getUserZIO: IO[UnknownUserError, User] = {
                getUser()
                ???
              }

              def getDocsZIO: IO[Throwable, Docs] = {
                getDocs()
                ???
              }

              def getPrefsZIO: IO[NoPreferencesError, Prefs] = {
                getPrefs()
                ???
              }

              for {
                user  <- getUserZIO
                docs  <- getDocsZIO
                prefs <- getPrefsZIO
              } yield assertTrue(
                user == "sherlock@holmes.com" && docs == List("Doc 1", "Doc 2") && prefs == Map("autosave" -> true)
              )

            } @@ ignore
        } +
        suite("control flow") {

          /**
           * 연습문제
           *
           * `ZIO#forever`를 사용하여 워커가 영원히 실행되도록 하여
           * 중단될 때까지 결과를 계속 누적하도록 하세요.
           */
          test("forever") {

            def makeWorker(ref: Ref[List[String]]) =
              ref.update("All work and no play makes Jack a dull boy" :: _)

            for {
              accum  <- Ref.make[List[String]](Nil)
              worker = makeWorker(accum)
              fiber  <- worker.fork
              _      <- accum.get.repeatUntil(_.length > 10) *> fiber.interrupt
              result <- accum.get
            } yield assertTrue(result.length > 10)
          } @@ ignore +
            /**
             * 연습문제
             *
             * `ZIO#eventually`를 사용하여 워커가 성공할 때까지
             * 반복하도록 하세요.
             */
            test("eventually") {
              def makeWorker(ref: Ref[Int]) =
                for {
                  count <- ref.updateAndGet(_ + 1)
                  _     <- if (count < 10) ZIO.fail("Uh oh!") else ZIO.succeed(())
                } yield "Success!"

              for {
                ref    <- Ref.make(0)
                worker = makeWorker(ref)
                result <- worker
              } yield assertTrue(result == "Success!")
            } @@ ignore +
            /**
             * 연습문제
             *
             * `ZIO#repeatN`을 사용하여 제공된 이팩트를 5번 반복하세요.
             */
            test("repeatN") {

              for {
                ref    <- Ref.make(0)
                effect = ref.update(_ + 1)
                _      <- effect
                result <- ref.get
              } yield assertTrue(result == 6)
            } @@ ignore +
            /**
             * 연습문제
             *
             * `ZIO.whenZIO`를 사용하여 `continue` 이팩트가 `true`로 성공할 때마다
             * 제공된 ref를 `true`로 설정하세요.
             */
            test("whenZIO") {
              def isYes(line: String): Boolean =
                line.toLowerCase match {
                  case "y" | "yes" => true
                  case _           => false
                }

              val continue =
                for {
                  _    <- Console.printLine("Do you want to continue (y/n)?")
                  line <- Console.readLine
                } yield isYes(line)

              for {
                ref    <- Ref.make(false)
                _      <- TestConsole.feedLines("y")
                _      <- continue
                result <- ref.get
              } yield assertTrue(result)
            } @@ ignore
        }
    }
}
