/**
 * **null의 문제점을 이해하고 Option으로 해결하기**
 * 
 * 모든 프로그래머가 한 번쯤은 만나게 되는 오류가 있습니다: NullPointerException
 * 이 오류는 값이 없다는 것을 나타내기 위해 null을 사용할 때 발생합니다.
 * 
 * 문제의 예시:
 * ```scala
 * val name: String = getName()  // null을 반환할 수 있음
 * println(name.length)          // 💥 NullPointerException!
 * ```
 * 
 * **null의 근본적인 문제들:**
 * 1. **타입 시스템이 거짓말한다**: `String` 타입이라고 했는데 실제로는 null일 수 있음
 * 2. **컴파일러가 도와줄 수 없다**: null 체크를 깜빠뜨려도 컴파일 에러가 나지 않음  
 * 3. **런타임에 터진다**: 프로그램이 실행 중에 갑자기 크래시 남
 * 
 * **Scala의 해결책: Option 타입**
 * Option은 "값이 있을 수도 있고 없을 수도 있다"는 상황을 타입으로 명확히 표현합니다:
 * - `Some(value)`: 값이 있는 경우
 * - `None`: 값이 없는 경우
 * 
 * ```scala
 * val name: Option[String] = getName()  // 명확히 "값이 없을 수 있다"고 표현
 * name match {
 *   case Some(actualName) => println(actualName.length)  // 안전하게 사용
 *   case None => println("이름이 없습니다")
 * }
 * ```
 * 
 * Tony Hoare(null을 발명한 사람)는 null을 "10억 달러 실수"라고 부르며 후회했습니다.
 * 이 모듈에서는 null 대신 Option을 사용하여 안전하고 명확한 코드를 작성하는 방법을 배웁니다.
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
