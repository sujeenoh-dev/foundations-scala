/**
 * 많은 프로그래밍 언어들은 예외를 정상적인 반환 과정을 중단시키고 
 * 애플리케이션의 상위 레벨에 실패를 알리는 방법으로 사용합니다.
 * 함수형 스칼라는 다른 선택지를 제공합니다: 타입화된 반환 값을 사용하여 
 * 컴파일러가 예상 가능한 오류 상황을 처리할 수 있도록 도와주며,
 * 클라우드 환경의 복잡성을 더 잘 다루는 훨씬 견고하고 복원력 있는 
 * 코드를 만들 수 있게 합니다.
 *
 * 이 모듈에서는 애플리케이션에서 예외를 제거하고 타입화된 반환 값을 
 * 사용하여 프로그래밍하는 방법을 배우게 됩니다.
 */
package net.degoes

import zio.test._
import zio.test.TestAspect._

object Exceptions extends ZIOSpecDefault {
  def spec =
    suite("Exceptions") {
      suite("constructors") {

        /**
         * 연습문제
         *
         * `parseInt`가 `Option`을 반환하도록 수정하세요.
         */
        test("Option") {
          def parseInt(s: String) = s.toInt

          def test = (parseInt(""): Any) match {
            case None => "None"
            case _    => "Some"
          }

          assertTrue(test == "None")
        } @@ ignore
          /**
           * 연습문제
           *
           * `parseInt`가 `Try`를 반환하도록 수정하세요.
           */
          test("Try") {
            import scala.util._

            def parseInt(s: String) = s.toInt

            def test = (parseInt(""): Any) match {
              case Failure(_) => "Failure"
              case _          => "Success"
            }

            assertTrue(test == "Failure")
          } @@ ignore
          /**
           * 연습문제
           *
           * `parseInt`가 `Either`를 반환하도록 수정하세요. `Left`는 
           * 정수 파싱 실패를 의미합니다.
           */
          test("Either") {
            def parseInt(s: String) = s.toInt

            def test = (parseInt(""): Any) match {
              case Left(_) => "Left"
              case _       => "Right"
            }

            assertTrue(test == "Left")
          } @@ ignore
      } +
        suite("map") {

          /**
           * 연습문제
           *
           * `Option#map`을 사용하여 `parseInt` 헬퍼 함수로 
           * 올바른 `Id` 생성자를 구현하세요.
           */
          test("Option") {
            def parseInt(i: String): Option[Int] =
              try Some(i.toInt)
              catch { case _: Throwable => None }

            final case class Id private (value: Int)

            object Id {
              def fromString(value: String): Option[Id] = {
                parseInt(value)

                ???
              }
            }

            assertTrue(Id.fromString("123").isDefined)
          } @@ ignore
            /**
             * 연습문제
             *
             * `Try#map`을 사용하여 `parseInt` 헬퍼 함수로 
             * 올바른 `Natural.fromString` 생성자를 구현하세요. 
             * 문자열이 숫자이고 그 숫자가 음이 아닌 경우에만 성공합니다.
             */
            test("Try") {
              import scala.util._

              def parseInt(i: String): Try[Int] = Try(i.toInt)

              final case class Id private (value: Int)

              object Id {
                def fromString(value: String): Try[Id] = {
                  parseInt(value)

                  ???
                }
              }

              assertTrue(Id.fromString("123").isSuccess)
            } @@ ignore
            /**
             * 연습문제
             *
             * `Either#map`을 사용하여 `parseInt` 헬퍼 함수로 
             * 올바른 `Natural.fromString` 생성자를 구현하세요. 
             * 문자열이 숫자이고 그 숫자가 음이 아닌 경우에만 성공합니다.
             */
            test("Either") {
              def parseInt(i: String): Either[String, Int] =
                try Right(i.toInt)
                catch {
                  case e: NumberFormatException => Left(e.getMessage())
                }

              final case class Id private (value: Int)

              object Id {
                def fromString(value: String): Either[String, Id] = {
                  parseInt(value)

                  ???
                }
              }

              assertTrue(Id.fromString("123").isRight)
            } @@ ignore
        } +
        suite("fallback") {

          /**
           * 연습문제
           *
           * `fallback`을 구현하세요. 좌측에 값이 있으면 좌측을 선호하고,
           * 그렇지 않으면 우측을 사용하도록 합니다.
           */
          test("Option") {
            def fallback[A](left: Option[A], right: Option[A]): Option[A] = ???

            assertTrue(fallback(None, Some(42)) == Some(42))
          } @@ ignore
            /**
             * 연습문제
             *
             * `fallback`을 구현하세요. 좌측에 값이 있으면 좌측을 선호하고,
             * 그렇지 않으면 우측을 사용하도록 합니다.
             */
            test("Try") {
              import scala.util._

              def fallback[A](left: Try[A], right: Try[A]): Try[A] = ???

              assertTrue(fallback(Failure(new Throwable), Success(42)) == Success(42))
            } @@ ignore
            /**
             * 연습문제
             *
             * `fallback`을 구현하세요. 좌측에 값이 있으면 좌측을 선호하고,
             * 그렇지 않으면 우측을 사용하도록 합니다.
             */
            test("Either") {
              def fallback[E, A](left: Either[E, A], right: Either[E, A]): Either[E, A] = ???

              assertTrue(fallback(Left("Uh oh!"), Right(42)) == Right(42))
            } @@ ignore
        } +
        suite("flatMap") {

          /**
           * 연습문제
           *
           * `Option#flatMap`을 사용하여 `parseInt` 헬퍼 함수로 
           * 올바른 `Natural.fromString` 생성자를 구현하세요. 
           * 문자열이 숫자이고 그 숫자가 음이 아닌 경우에만 성공합니다.
           */
          test("Option") {
            def parseInt(i: String): Option[Int] =
              try Some(i.toInt)
              catch { case _: Throwable => None }

            final case class Natural(value: Int)

            object Natural {
              def fromString(value: String): Option[Natural] = {
                parseInt(value)

                ???
              }
            }

            assertTrue(Natural.fromString("123").isDefined)
          } @@ ignore
            /**
             * 연습문제
             *
             * `Try#flatMap`을 사용하여 `parseInt` 헬퍼 함수로 
             * 올바른 `Natural.fromString` 생성자를 구현하세요. 
             * 문자열이 숫자이고 그 숫자가 음이 아닌 경우에만 성공합니다.
             */
            test("Try") {
              import scala.util._

              def parseInt(i: String): Try[Int] = Try(i.toInt)

              final case class Natural(value: Int)

              object Natural {
                def fromString(value: String): Try[Natural] = {
                  parseInt(value)

                  ???
                }
              }

              assertTrue(Natural.fromString("123").isSuccess)
            } @@ ignore
            /**
             * 연습문제
             *
             * `Either#flatMap`을 사용하여 `parseInt` 헬퍼 함수로 
             * 올바른 `Natural.fromString` 생성자를 구현하세요. 
             * 문자열이 숫자이고 그 숫자가 음이 아닌 경우에만 성공합니다.
             */
            test("Either") {
              def parseInt(i: String): Either[String, Int] =
                try Right(i.toInt)
                catch {
                  case e: NumberFormatException => Left(e.getMessage())
                }

              final case class Natural(value: Int)

              object Natural {
                def fromString(value: String): Either[String, Natural] = {
                  parseInt(value)

                  ???
                }
              }

              assertTrue(Natural.fromString("123").isRight)
            } @@ ignore
        } +
        suite("both") {

          /**
           * 연습문제
           *
           * `both`를 구현하세요. 양쪽에 값이 모두 존재할 때 
           * 해당 값들의 튜플을 생성합니다.
           */
          test("Option") {
            def both[A, B](left: Option[A], right: Option[B]): Option[(A, B)] = ???

            assertTrue(both(Some(4), Some(2)) == Some((4, 2)))
          } @@ ignore
            /**
             * 연습문제
             *
             * `both`를 구현하세요. 양쪽에 값이 모두 존재할 때 
             * 해당 값들의 튜플을 생성합니다.
             */
            test("Try") {
              import scala.util._

              def both[A, B](left: Try[A], right: Try[B]): Try[(A, B)] = ???

              assertTrue(both(Try(4), Try(2)) == Try((4, 2)))
            } @@ ignore
            /**
             * 연습문제
             *
             * `both`를 구현하세요. 양쪽에 값이 모두 존재할 때 
             * 해당 값들의 튜플을 생성합니다.
             */
            test("Either") {
              def both[E, A, B](left: Either[E, A], right: Either[E, B]): Either[E, (A, B)] = ???

              assertTrue(both(Right(4), Right(2)) == Right((4, 2)))
            } @@ ignore
        } +
        suite("porting") {

          /**
           * 연습문제
           *
           * 다음 코드를 예외 대신 `Option`을 사용하도록 다시 작성하세요.
           */
          test("Option") {
            object Config {
              def getHost(): String = {
                val result = System.getProperty("CONFIG_HOST")

                if (result == null) throw new RuntimeException("Host is missing")

                result
              }

              def getPort(): Int = {
                val result = System.getProperty("CONFIG_HOST")

                if (result == null) throw new RuntimeException("Port is missing")

                result.toInt
              }
            }

            final case class ConnectionInfo(host: String, port: Int)

            def loadConnectionInfo(): ConnectionInfo =
              ConnectionInfo(Config.getHost(), Config.getPort())

            assertTrue(loadConnectionInfo().FIXME)
          } @@ ignore
            /**
             * 연습문제
             *
             * 다음 코드를 예외 대신 `Try`를 사용하도록 다시 작성하세요.
             */
            test("Try") {
              object Config {
                def getHost(): String = {
                  val result = System.getProperty("CONFIG_HOST")

                  if (result == null) throw new RuntimeException("Host is missing")

                  result
                }

                def getPort(): Int = {
                  val result = System.getProperty("CONFIG_HOST")

                  if (result == null) throw new RuntimeException("Port is missing")

                  result.toInt
                }
              }

              final case class ConnectionInfo(host: String, port: Int)

              def loadConnectionInfo(): ConnectionInfo =
                ConnectionInfo(Config.getHost(), Config.getPort())

              assertTrue(loadConnectionInfo().FIXME)
            } @@ ignore
            /**
             * 연습문제
             *
             * 다음 코드를 예외 대신 `Either`를 사용하도록 다시 작성하세요.
             */
            test("Either") {
              object Config {
                def getHost(): String = {
                  val result = System.getProperty("CONFIG_HOST")

                  if (result == null) throw new RuntimeException("Host is missing")

                  result
                }

                def getPort(): Int = {
                  val result = System.getProperty("CONFIG_HOST")

                  if (result == null) throw new RuntimeException("Port is missing")

                  result.toInt
                }
              }

              final case class ConnectionInfo(host: String, port: Int)

              def loadConnectionInfo(): ConnectionInfo =
                ConnectionInfo(Config.getHost(), Config.getPort())

              assertTrue(loadConnectionInfo().FIXME)
            } @@ ignore
        } +
        suite("mixed") {

          /**
           * 연습문제
           *
           * Option과 Try를 정보 손실 없이 결합하는 방법을 찾으세요.
           */
          test("Option/Try") {
            import scala.util._

            type User = String
            type Docs = List[String]

            def getUser: Option[User] = Some("sherlock@holmes.com")
            def getDocs: Try[Docs]    = Try(List("Doc 1", "Doc 2"))

            def getUserAndDocs = {
              getUser
              getDocs
              ???
            }

            assertTrue(getUserAndDocs == ???)
          } @@ ignore
            /**
             * 연습문제
             *
             * Either와 Option을 정보 손실 없이 결합하는 방법을 찾으세요.
             */
            test("Either/Option") {
              import scala.util._

              type User = String
              type Docs = List[String]

              def getUser: Either[String, User] = Right("sherlock@holmes.com")
              def getDocs: Option[Docs]         = Some(List("Doc 1", "Doc 2"))

              def getUserAndDocs = {
                getUser
                getDocs
                ???
              }

              assertTrue(getUserAndDocs == ???)
            } @@ ignore
            /**
             * 연습문제
             *
             * Either와 Try를 정보 손실 없이 결합하는 방법을 찾으세요.
             */
            test("Either/Try") {
              import scala.util._

              type User = String
              type Docs = List[String]

              def getUser: Either[String, User] = Right("sherlock@holmes.com")
              def getDocs: Try[Docs]            = Try(List("Doc 1", "Doc 2"))

              def getUserAndDocs = {
                getUser
                getDocs
                ???
              }

              assertTrue(getUserAndDocs == ???)
            } @@ ignore
            /**
             * 연습문제
             *
             * Either, Try, Option을 정보 손실 없이 결합하는 방법을 찾으세요.
             */
            test("Either/Try/Option") {
              import scala.util._

              type User  = String
              type Docs  = List[String]
              type Prefs = Map[String, Boolean]

              def getUser: Either[String, User] = Right("sherlock@holmes.com")
              def getDocs: Try[Docs]            = Try(List("Doc 1", "Doc 2"))
              def getPrefs: Option[Prefs]       = Some(Map("autosave" -> true))

              def getUserAndDocsAndPrefs = {
                getUser
                getDocs
                getPrefs
                ???
              }

              assertTrue(getUserAndDocsAndPrefs == ???)
            } @@ ignore
        }
    }
}
