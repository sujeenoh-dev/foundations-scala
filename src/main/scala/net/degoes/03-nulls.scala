/**
 * Tony Hoare는 null을 "10억 달러 실수"라고 부르는 것으로 유명합니다.
 * 실제로는 null이 운영 소프트웨어에 가하는 비용이 훨씬 더 높습니다.
 * 함수형 Scala에서는 null 값을 사용하지 않고, 대신 컴파일 타임에
 * 정보를 전달하는 데이터 타입으로 선택성(optionality)을 표현합니다.
 * 이러한 결정은 NullPointerException을 방지하며, 결과적으로
 * 더 잘 정의된 오류 처리와 더 안정적인 애플리케이션을 만들어냅니다.
 *
 * 이 모듈에서는 null을 option으로 대체하는 방법을 배웁니다.
 */
package net.degoes

import zio.test._
import zio.test.TestAspect._

object Nulls extends ZIOSpecDefault {
  def spec =
    suite("Nulls") {
      suite("basics") {

        /**
         * EXERCISE
         *
         * `parentOf` 함수는 일부 경로에 대해 `null`을 반환합니다. 함수를
         * `File | Null` 대신 `Option[File]`을 반환하도록 수정하세요.
         */
        test("apply") {
          import java.io.File

          def parentOf(file: String) = new File(file).getParent

          assertTrue(parentOf("") != null)
        } @@ ignore +
          /**
           * EXERCISE
           *
           * `Option`의 `Some`과 `None` 생성자를 직접 사용하여,
           * null일 수 있는 `A` 값으로부터 `Option[A]`를 생성하세요.
           */
          test("Some / None") {
            def fromNullable[A](a: A): Option[A] =
              ???

            val nullInt = null.asInstanceOf[Int]

            assertTrue(fromNullable(nullInt) == None && fromNullable(42) == Some(42))
          } @@ ignore +
          /**
           * EXERCISE
           *
           * `Option#getOrElse`를 사용하여, `loadConfig` 메서드가 `None`을
           * 반환할 경우 `DefaultConfig`를 기본값으로 사용하세요.
           */
          test("getOrElse") {
            final case class Config(host: String, port: Int)
            val DefaultConfig = Config("localhost", 7777)

            val _ = DefaultConfig

            def loadConfig(): Option[Config] = None

            def config: Config = {
              loadConfig()
              ???
            }

            assertTrue(config != null)
          } @@ ignore +
          /**
           * EXERCISE
           *
           * `Option#map`을 사용하여, int를 char로 변환함으로써
           * `Option[Int]`를 `Option[Char]`로 변환하세요.
           */
          test("map") {
            val option: Option[Int] = Some(42)

            def convert(o: Option[Int]): Option[Char] = ???

            assertTrue(convert(option) == Some(42.toChar))
          } @@ ignore +
          /**
           * EXERCISE
           *
           * 두 개의 옵션을 결합하여 두 결과의 튜플을 담은 단일 옵션으로
           * 만들 수 있는 `both` 함수를 구현하세요.
           */
          test("both") {
            def both[A, B](left: Option[A], right: Option[B]): Option[(A, B)] =
              ???

            assertTrue(both(Some(42), Some(24)) == Some((42, 24)))
          } @@ ignore +
          /**
           * EXERCISE
           *
           * 첫 번째로 이용 가능한 값을 사용하여 두 개의 옵션을 단일 옵션으로
           * 결합할 수 있는 `firstOf` 함수를 구현하세요.
           */
          test("oneOf") {
            def firstOf[A](left: Option[A], right: Option[A]): Option[A] =
              ???

            assertTrue(firstOf(None, Some(24)) == Some(24))
          } @@ ignore +
          /**
           * EXERCISE
           *
           * 옵션의 값을 지정된 콜백에 전달하여 다른 옵션을 생성하고
           * 반환하는 `chain` 함수를 구현하세요. 옵션에 값이 없으면
           * `chain`의 반환값은 `None`입니다. `chain` 메서드의 "단락 회로"
           * 동작에 주목하세요. 이것이 무엇을 연상시키나요?
           */
          test("chain") {
            def chain[A, B](first: Option[A], andThen: A => Option[B]): Option[B] =
              ???

            assertTrue(chain(Some(42), (x: Int) => if (x < 10) None else Some(x)) == Some(42))
          } @@ ignore +
          /**
           * EXERCISE
           *
           * `Option#flatMap`을 사용하여 다음의 패턴 매칭이 많은
           * 코드를 단순화하세요.
           */
          test("flatMap") {
            final case class LatLong(lat: Double, long: Double)
            final case class Location(country: String, latLong: Option[LatLong])
            final case class Profile(location: Option[Location])
            final case class User(name: String, profile: Option[Profile])

            def getLatLong(user: User): Option[LatLong] =
              user.profile match {
                case None => None
                case Some(v) =>
                  v.location match {
                    case None    => None
                    case Some(v) => v.latLong
                  }
              }

            val latLong = LatLong(123, 123)

            val user = User("Holmes", Some(Profile(Some(Location("UK", Some(latLong))))))

            assertTrue(getLatLong(user) == Some(latLong))
          }
      } +
        suite("porting") {

          /**
           * EXERCISE
           *
           * System.property 메서드의 null-safe 버전을 만드세요.
           */
          test("property") {
            object SafeProperty {
              def getProperty(name: String): Option[String] = ???

              def getIntProperty(name: String): Option[Int] = ???

              def getBoolProperty(name: String): Option[Boolean] = ???
            }

            assertTrue(SafeProperty.getProperty("foo.bar") == None)
          } @@ ignore +
            /**
             * EXERCISE
             *
             * 다음 코드를 null 대신 `Option`을 사용하도록 다시 작성하세요.
             */
            test("example 1") {
              final case class Address(street: String)
              final case class Profile(address: Address)
              final case class User(id: String, profile: Profile)

              val user1 =
                User("Sherlock Holmes", null)
              val user2 =
                User("Sherlock Holmes", Profile(null))
              val user3 =
                User("Sherlock Holmes", Profile(Address(null)))

              def getStreet(user: User): String =
                if (user == null) null
                else if (user.profile == null) null
                else if (user.profile.address == null) null
                else if (user.profile.address.street == null) null
                else user.profile.address.street

              def assertFails(value: => Any) = assertTrue(value == null)

              assertFails(getStreet(user1)) &&
              assertFails(getStreet(user2)) &&
              assertFails(getStreet(user3))
            } @@ ignore
        }
    }
}
