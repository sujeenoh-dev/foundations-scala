/**
 * 함수형 스칼라에서는 거의 모든 데이터가 불변(immutable)입니다. 
 * 데이터 모델은 서로 다른 타입의 여러 필드를 가진 "레코드(곱객체)" 
 * 또는 서로 다른 구조를 가진 여러 케이스의 "열거형(합객체)"으로 완전히 구성됩니다. 
 * 스칼라는 레코드체 열거형에 내장된 강력한 기능을 제공합니다(스칼라 3만 해당). 
 * 이러한 데이터 모델을 구성하는 방식부터 사용하는 방식까지 모든 것이 객체지향 프로그래밍 언어와는 다릅니다. 
 * 핵심은 "잘못된 데이터"를 저장하는 데 사용할 수 없는 매우 정확한 데이터 모델을 만드는 것으로, 
 * 이는 런타임 오류를 제거하고 데이터베이스와 외부 시스템에 잘못된 데이터가 들어가는 것을 방지합니다.
 *
 * 이 모듈에서는 소프트웨어의 유지보수성과 신뢰성을 향상시키는 정확한 방식으로 데이터 모델링 문제를 해결하기 위해 스칼라가 제공하는 함수형 도구를 채택하는 방법을 배우게 됩니다.
 */
package net.degoes

import zio._
import zio.test._
import zio.test.TestAspect._

object Data extends ZIOSpecDefault {
  type ???

  def spec =
    suite("Data") {
      suite("Case Classes") {
        /**
         * 스칼라에서 **곱객체(Product Type)**란 여러 값을 함께 묶어 하나의 단위로 표현하는 타입을 말합니다. 
         * 예를 들어 `case class`가 대표적이며, 클래스 안에 여러 필드가 존재하고 이 값들을 모두 함께 가진다는 의미에서 곱(×)이라는 이름이 붙습니다. 
         * 예를 들어 `case class Person(name: String, age: Int)`라고 하면, `Person`은 반드시 이름과 나이를 둘 다 함께 가지게 됩니다. 즉, 곱객체는 “그리고(AND)” 관계를 나타냅니다.
         * 곱객체는 여러 값이 항상 함께 존재해야 함을 보장합니다.
         */

        /**
         * 연습문제
         *
         * 클래스의 모든 생성자 매개변수에 대해 자동으로 getter를 생성하기 위해 `Person` 케이스 클래스를 생성하세요.
         */
        test("fields") {
          class Person(name: String, age: Int) // TODO

          def getName(p: Person): String = ??? // TODO
          def getAge(p: Person): Int     = ??? // TODO

          val holmes = new Person("Sherlock Holmes", 42) // TODO

          assertTrue(getName(holmes) == "Sherlock Holmes" && getAge(holmes) == 42)
        } @@ ignore +
          /**
           * 연습문제
           *
           * 이름(String)과 나이(Int)를 가진 `Person` 케이스 클래스를 생성하고, 가짜 생성자를 구현하여 모든 케이스 클래스가 컴패니언 객체에서 받는 자동 생성자를 만드세요.
           */
          test("apply") {
            object Person {
              def apply(name: String, age: Int) = ??? // TODO
            }

            assertTrue(Person("Sherlock Holmes", 42) == Person("Sherlock Holmes", 42))
          } @@ ignore +
          /**
           * 연습문제
           *
           * `Profile` 클래스를 케이스 클래스로 변환하여 동등성(equality)의 자동 구현을 확인하세요.
           */
          test("equals") {
            class Profile(val age: Int) // TODO

            assertTrue(new Profile(42) == new Profile(42)) // TODO
          } @@ ignore +
          /**
           * 연습문제
           *
           * `CreditCard` 클래스를 케이스 클래스로 변환하여 해시 코드의
           * 자동 구현을 받으세요.
           */
          test("hashCode") {
            class CreditCard(val number: String) // TODO

            assertTrue(new CreditCard("123").hashCode == new CreditCard("123").hashCode) // TODO
          } @@ ignore +
          /**
           * 연습문제
           *
           * `Address` 클래스를 케이스 클래스로 변환하여 `toString`의
           * 자동 구현을 받으세요.
           */
          test("toString") {
            class Address(val street: String) // TODO

            assertTrue(new Address("221B Baker Street").toString == "Address(221B Baker Street)") // TODO
          } @@ ignore +
          /**
           * 연습문제
           *
           * `Permissions` 클래스를 케이스 클래스로 변환하여 `copy`의
           * 자동 구현을 받으세요.
           */
          test("copy") {
            class Permissions(val canRead: Boolean, canWrite: Boolean, canShare: Boolean) { // TODO
              def copy(
                canRead: Boolean = this.canRead,
                canWrite: Boolean = this.canWrite,
                canShare: Boolean = this.canShare
              ): Permissions = ???
            }

            val perms = new Permissions(true, false, false) // TODO

            assertTrue(perms.copy(canRead = false) == new Permissions(false, false, false)) // TODO
          } @@ ignore +
          suite("patterns") {

            /**
             * 연습문제
             *
             * 패턴 매칭을 사용하여 `Address`의 `street`을 추출하세요.
             */
            test("simple") {
              final case class Address(street: String)

              def extractStreet(address: Address): String = ??? // TODO

              assertTrue(extractStreet(Address("221B Baker")) == "221B Baker")
            } @@ ignore +
              /**
               * 연습문제
               *
               * 패턴 매칭을 사용하여 `Address`의 `postalCode`를 추출하세요.
               * 와일드카드를 사용하여 `street`을 무시(어떤 값이든 매칭)하세요.
               */
              test("wildcard") {
                final case class Address(street: String, postalCode: String)

                def extractPostalCode(address: Address): String = ??? // TODO

                assertTrue(extractPostalCode(Address("221B Baker", "NW1 6XE")) == "NW1 6XE")
              } @@ ignore +
              /**
               * 연습문제
               *
               * 상수에 대한 패턴 매칭을 사용하여 제공된 함수를 구현하세요.
               * 우편번호에 관계없이 "221B Baker"와 일치하는 모든 거리에 대해
               * true를 반환해야 합니다.
               */
              test("constant") {
                final case class Address(street: String, postalCode: String)

                def is221B(address: Address): Boolean = ??? // TODO

                assertTrue(is221B(Address("221B Baker", "NW1 6XE")))
              } @@ ignore +
              /**
               * 연습문제
               *
               * 패턴 매칭에서 여러 개의 순서가 있는 케이스 절을 사용하여
               * 제공된 함수를 구현하세요. "Baker" 거리의 모든 주소에 대해
               * "Knows Holmes"를 반환하고, 그렇지 않으면 "Unknown"을
               * 반환해야 합니다.
               */
              test("ordered") {
                final case class Address(number: String, street: String, postalCode: String)

                def neighbor(address: Address): String = ??? // TODO

                assertTrue(neighbor(Address("220", "Baker", "NW1 6XE")) == "Knows Holmes")
              } @@ ignore +
              /**
               * 연습문제
               *
               * 조건부 패턴을 사용하여 제공된 함수를 구현하세요. (conditional patterns)
               * "Baker"를 포함하는 모든 거리에 대해 true를 반환해야 합니다.
               */
              test("conditional") {
                final case class Address(street: String, postalCode: String)

                def isBaker(address: Address): Boolean = ??? // TODO

                assertTrue(isBaker(Address("220 Baker", "NW1 6XE")))
              } @@ ignore +
              /**
               * 연습문제
               *
               * 중첩된 패턴을 사용하여 제공된 함수를 구현하세요. (nested patterns)
               * 모든 사람의 우편번호를 추출해야 합니다.
               */
              test("nested") {
                final case class Person(name: String, address: Address)
                final case class Address(street: String, postalCode: String)

                def extractPostalCode(person: Person): String = ??? // TODO

                val sherlock = Person("Sherlock Holmes", Address("221B Baker", "NW1 6XE"))

                assertTrue(extractPostalCode(sherlock) == "NW1 6XE")
              } @@ ignore +
              /**
               * 연습문제
               *
               * 입력된 address.street가 `sherlockStreet`와 일치하면 true를 반환하세요.
               */
              test("quoted") {
                final case class Address(number: String, street: String, postalCode: String)

                val sherlockStreet = "Baker"

                val _ = sherlockStreet

                def isSherlockStreet(address: Address): Boolean = ???

                val address = Address("220", "Baker", "NW1 6XE")

                assertTrue(isSherlockStreet(address))
              } @@ ignore
          }
      } +
        suite("Sealed Traits") {

          /**
           * 연습문제
           *
           * 다음 trait를 sealed로 만들어 패턴 매칭에서 완전성 검사를
           * 얻으세요. 경고가 어떻게 변하는지 주목하세요.
           */
          test("sealed") {
            trait Color
            case object Red   extends Color
            case object Green extends Color
            case object Blue  extends Color

            val _ = Green
            val _ = Blue
            val _ = Red

            val isRed: Color => Boolean = {
              case _ => ???
            }

            assertTrue(!isRed(Blue))
          } @@ ignore +
            /**
             * 연습문제
             *
             * `UK`, `Germany`, `India`, `Netherlands`, `USA` 케이스 객체로
             * 확장되는 sealed trait `Country`를 생성하세요.
             */
            test("country") {
              trait Country
              object UK
              object USA

              def isCountry(a: Any) = a.isInstanceOf[Country]

              assertTrue(isCountry(UK) && isCountry(USA))
            } @@ ignore +
            /**
             * 연습문제
             *
             * `asCreditCard` 메서드를 구현할 때 `as` 패턴을 사용하여
             * `CreditCard` 결제 방법에 대해 매칭하고, 변수로 캡처한 후
             * `Some(_)` 생성자로 감싸서 반환하세요. 다른 결제 방법의 경우
             * `None`을 반환하세요.
             */
            test("as patterns") {
              sealed trait PaymentMethod
              final case class CreditCard(number: String, expDate: java.time.YearMonth, securityCode: Short)
                  extends PaymentMethod
              final case class PayPal(email: String) extends PaymentMethod

              val _ = PayPal("")

              def asCreditCard(paymentMethod: PaymentMethod): Option[CreditCard] = ???

              val cc: CreditCard = CreditCard("123123123123", java.time.YearMonth.of(1984, 12), 123)

              assertTrue(asCreditCard(cc) == Option(cc))
            }
        } +
        suite("Modeling") {

          /**
           * 연습문제
           *
           * 개인의 관계 상태를 모델링하는 `RelationshipStatus`의 정확한
           * 데이터 모델을 생성하세요: 기혼, 독신, 이혼.
           */
          test("example 1") {
            type RelationshipStatus = ???

            def makeMarried: RelationshipStatus = ???

            def makeSingle: RelationshipStatus = ???

            assertTrue(makeMarried != makeSingle)
          } @@ ignore +
            /**
             * 연습문제
             *
             * 연결 URL, 데이터 형식(JSON 또는 XML), API 토큰(문자열)을
             * 저장하는 `PaymentProcessorAPI`의 정확한 데이터 모델을
             * 생성하세요.
             */
            test("example 2") {
              type PaymentProcessorAPI = ???
              type DataFormat          = ???

              def define(url: java.net.URI, df: DataFormat, apiToken: String): PaymentProcessorAPI = ???

              val url              = new java.net.URI("https://stripe.com")
              def json: DataFormat = ???

              val api1 = define(url, json, "123123")
              val api2 = define(url, json, "123124")

              assertTrue(api1 == api1 && api1 != api2)
            } @@ ignore +
            /**
             * 연습문제
             *
             * 사용자의 암호화폐 포트폴리오에 대한 정확한 데이터 모델을
             * 생성하세요.
             */
            test("example 3") {
              type Portfolio = ???

              type Symbol = ???

              def ETH: Symbol = ???
              def BTC: Symbol = ???

              def add(portfolio: Portfolio, symbol: Symbol, amount: Double): Portfolio = ???

              def empty: Portfolio = ???

              val p1 = add(add(add(empty, ETH, 1.0), ETH, 1.0), BTC, 2.0)
              val p2 = add(add(empty, BTC, 2.0), ETH, 2.0)

              assertTrue(p1 == p2)
            } @@ ignore +
            /**
             * 연습문제
             *
             * SaaS 제품에 대한 구독의 정확한 데이터 모델을 생성하세요.
             * 연간 또는 월간 수준일 수 있고, 계획에 다양한 기능을
             * 번들로 포함할 수 있습니다.
             */
            test("example 4") {
              type Features     = ???
              type Subscription = ???
              def makeFeatures(space: Int, sso: Boolean, customLogo: Boolean): Features = ???
              def makeMonthly(amount: Double, features: Features): Subscription         = ???
              def makeAnnually(amount: Double, features: Features): Subscription        = ???

              val features = makeFeatures(2048, true, true)

              assertTrue(makeMonthly(9.99, features) != makeAnnually(9.99, features))
            } @@ ignore +
            /**
             * 연습문제
             *
             * 이름과 필드 타입을 포함하는 필드의 정확한 데이터 모델을
             * 생성하세요. 필드 타입은 정수, 문자열, 불린 또는 폼의
             * 필드에 대한 기타 일반적인 타입일 수 있습니다.
             */
            test("advanced example") {
              type Field[A]     = ???
              type FieldType[A] = ???

              def intType: FieldType[Int]    = ???
              def strType: FieldType[String] = ???

              def makeField[A](name: String, fieldType: FieldType[A]): Field[A] = ???

              val strField1 = makeField("name", strType)
              val strField2 = makeField("name", strType)
              val numField  = makeField("age", intType)

              assertTrue(strField1 == strField2 && numField != strField1)
            } @@ ignore
        }
    }
}

/**
 * 많은 프로그래밍 언어가 케이스 클래스와 같은 구성을 가지고 있지만,
 * sealed trait의 힘을 가진 언어는 거의 없고, 대부분이 스칼라의 패턴 매칭
 * 기능을 가지고 있지 않습니다. 이러한 강력한 기능들의 조합으로, 런타임 오류를
 * 제거하고 코드를 테스트하고 유지보수하기를 이전보다 쉽게 만드는 매우 정확한
 * 데이터 모델을 구성할 수 있습니다.
 *
 * 이 졸업 프로젝트에서는 케이스 클래스와 sealed trait를 사용하여 정확한
 * 데이터 모델을 구성하는 경험을 얻게 됩니다.
 */
object DataGraduation extends ZIOAppDefault {

  sealed trait Command
  object Command {
    case object Exit                        extends Command
    final case class Look(what: String)     extends Command
    final case class Go(where: String)      extends Command
    final case class Take(what: String)     extends Command
    final case class Drop(what: String)     extends Command
    final case class Fight(who: String)     extends Command
    final case class TalkTo(who: String)    extends Command
    final case class Unknown(input: String) extends Command

    def fromString(input: String): Command =
      input.trim.toLowerCase.split("\\w+").toList match {
        case exit :: Nil                  => Exit
        case "look" :: what :: Nil        => Look(what)
        case "go" :: where :: Nil         => Go(where)
        case "take" :: what :: Nil        => Take(what)
        case "drop" :: what :: Nil        => Drop(what)
        case "fight" :: who :: Nil        => Fight(who)
        case "talk" :: "to" :: who :: Nil => TalkTo(who)
        case _                            => Unknown(input)
      }
  }

  /**
   * 연습문제
   *
   * 텍스트 기반 롤플레잉 게임의 게임 월드 상태에 대한 데이터 모델을
   * 구성하세요. 데이터 모델은 플레이어 캐릭터, 게임 월드의 지도,
   * 게임 월드의 아이템과 캐릭터, 그리고 게임과 관련된 기타 모든 것을
   * 표현해야 합니다.
   */
  final case class State(playerName: String)

  final case class Step(nextState: Option[State], output: String)

  /**
   * 연습문제
   *
   * 사용자로부터 읽은 현재 명령과 이전 상태 모두에서 게임 월드의
   * 새로운 상태가 구성되도록 `nextStep` 함수를 구현하세요.
   */
  def nextStep(state: State, command: Command): Step = ???

  def mainLoop(ref: Ref[State]) =
    (for {
      line    <- Console.readLine
      command = Command.fromString(line)
      state   <- ref.get
      step    = nextStep(state, command)
      cont    <- step.nextState.fold(ZIO.succeed(false))(ref.set(_).as(true))
      _       <- Console.printLine(step.output)
    } yield cont).repeatWhile(cont => cont == true)

  def run =
    for {
      _     <- Console.printLine("Welcome to the game! What is your name?")
      name  <- Console.readLine
      state = State(name)
      ref   <- Ref.make(state)
      _     <- mainLoop(ref)
    } yield ()
}
