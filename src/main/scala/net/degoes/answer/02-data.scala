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

object DataAnswer extends ZIOSpecDefault {

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
         * 연습문제-01
         *
         * 클래스의 모든 생성자 매개변수에 대해 자동으로 getter를 생성하기 위해 `Person` 케이스 클래스를 생성하세요.
         */
        test("fields") {
          // Case classes are the foundation of data modeling in functional Scala
          // They automatically provide: getters, equals, hashCode, toString, copy
          case class Person(name: String, age: Int)

          // These getter functions work because case class fields are public by default
          def getName(p: Person): String = p.name  // Direct field access - no getter method needed!
          def getAge(p: Person): Int     = p.age   // Case classes make fields accessible

          val holmes = Person("Sherlock Holmes", 42)

          assertTrue(getName(holmes) == "Sherlock Holmes" && getAge(holmes) == 42)
        } @@ ignore
          /**
           * 연습문제-02
           *
           * 이름(String)과 수량(Int)를 가진 `Recipe` 케이스 클래스를 생성하고, 가짜 생성자를 변경하여 모든 케이스 클래스가 컴패니언 객체에서 받는 자동 생성자를 만드세요.
           */
          test("apply") {
            case class Recipe(name: String, quantity: Int)

            object Recipe {
              def apply(name: String, quantity: Int): Recipe = Recipe(
                name = name,
                quantity = quantity
              )
            }

            assertTrue(Recipe("Sweet Potato", 42) == Recipe("Sweet Potato", 42))
          } @@ ignore
          /**
           * 연습문제-03
           *
           * `Profile` 클래스를 케이스 클래스로 변환하여 동등성(equality)이 자동 구현된 것을 확인하세요.
           */
          test("equals") {
            case class Profile(age: Int)

            assertTrue(Profile(42) == Profile(42))
          } @@ ignore
          /**
           * 연습문제-04
           *
           * `CreditCard` 클래스를 케이스 클래스로 변환하여 해시 코드가 자동 구현된 것을 확인하세요.
           */
          test("hashCode") {
            case class CreditCard(val number: String)

            assertTrue(CreditCard("123").hashCode == CreditCard("123").hashCode)
          } @@ ignore
          /**
           * 연습문제-05
           *
           * `Address` 클래스를 케이스 클래스로 변환하여 `toString`이 자동 구현된 것을 확인하세요.
           */
          test("toString") {
            case class Address(val street: String)

            assertTrue(Address("221B Baker Street").toString == "Address(221B Baker Street)")
          } @@ ignore
          /**
           * 연습문제-06
           *
           * `Permissions` 클래스를 케이스 클래스로 변환하여 `copy`가 자동 구현된 것을 확인하세요.
           */
          test("copy") {
            case class Permissions(canRead: Boolean, canWrite: Boolean, canShare: Boolean) {
              def copy(
                canRead: Boolean = canRead,
                canWrite: Boolean = canWrite,
                canShare: Boolean = canShare
              ): Permissions = copy(
                canRead = canRead,
                canWrite = canWrite,
                canShare = canShare
              )
            }

            val perms = Permissions(true, false, false)

            assertTrue(perms.copy(canRead = false) == Permissions(false, false, false))
          } @@ ignore
          suite("patterns") {
            /**
              * 패턴매칭은 값이 어떤 모양인지 살펴보고 “이 경우엔 이렇게, 저 경우엔 저렇게” 처리할 수 있도록 해 주는 일종의 다중 if-else 문입니다.
              * 
              * ```scala
              * fruit match {
              *   case "apple" => "apple"
              *   case "banana" => "banana"
              *   case "cherry" => "cherry"
              *   case _ => "unknown"
              * }
              * ```
              */

            /**
             * 연습문제-07
             *
             * 패턴 매칭을 사용하여 `Address`의 `street`을 추출하세요.
             */
            test("simple") {
              final case class Address(street: String)

              def extractStreet(address: Address): String = address match {
                case Address(street) => street
              }

              assertTrue(extractStreet(Address("221B Baker")) == "221B Baker")
            } @@ ignore
              /**
               * 연습문제-08
               *
               * 패턴 매칭을 사용하여 `Address`의 `postalCode`를 추출하세요.
               * 와일드카드를 사용하여 `street`을 무시(어떤 값이든 매칭)하세요.
               */
              test("wildcard") {
                final case class Address(street: String, postalCode: String)

                def extractPostalCode(address: Address): String = address match {
                  case Address(_, postalCode) => postalCode
                }

                assertTrue(extractPostalCode(Address("221B Baker", "NW1 6XE")) == "NW1 6XE")
              } @@ ignore
              /**
               * 연습문제-09
               *
               * 상수에 대한 패턴 매칭을 사용하여 제공된 함수를 구현하세요.
               * 우편번호에 관계없이 "221B Baker"와 일치하는 모든 거리에 대해
               * true를 반환해야 합니다.
               */
              test("constant") {
                final case class Address(street: String, postalCode: String)

                def is221B(address: Address): Boolean = address match {
                  case Address("221B Baker", _) => true
                  case _ => false
                }

                assertTrue(is221B(Address("221B Baker", "NW1 6XE")))
              } @@ ignore
              /**
               * 연습문제-10
               *
               * 패턴 매칭에서 여러 개의 순서가 있는 케이스 절을 사용하여
               * 제공된 함수를 구현하세요. "Baker" 거리의 모든 주소에 대해
               * "Knows Holmes"를 반환하고, 그렇지 않으면 "Unknown"을
               * 반환해야 합니다.
               */
              test("ordered") {
                final case class Address(number: String, street: String, postalCode: String)

                def neighbor(address: Address): String = address.street match {
                  case "Baker" => "Knows Holmes"
                  case _ => "Unknown"
                }

                assertTrue(neighbor(Address("220", "Baker", "NW1 6XE")) == "Knows Holmes")
              } @@ ignore
              /**
               * 연습문제-11
               *
               * 조건부 패턴을 사용하여 제공된 함수를 구현하세요. (conditional patterns)
               * "Baker"를 포함하는 모든 거리에 대해 true를 반환해야 합니다.
               */
              test("conditional") {
                final case class Address(street: String, postalCode: String)

                def isBaker(address: Address): Boolean = address.street match {
                  case street: String if street.contains("Baker") => true
                  case _ => false
                }

                assertTrue(isBaker(Address("220 Baker", "NW1 6XE")))
              } @@ ignore
              /**
               * 연습문제-12
               *
               * 중첩된 패턴을 사용하여 제공된 함수를 구현하세요. (nested patterns)
               * 모든 사람의 우편번호를 추출해야 합니다.
               */
              test("nested") {
                final case class Person(name: String, address: Address)
                final case class Address(street: String, postalCode: String)

                def extractPostalCode(person: Person): String = person match {
                  case Person(_, address) => address match {
                    case Address(_, postalCode) => postalCode
                  }
                }

                val sherlock = Person("Sherlock Holmes", Address("221B Baker", "NW1 6XE"))

                assertTrue(extractPostalCode(sherlock) == "NW1 6XE")
              } @@ ignore
              /**
               * 연습문제-13
               *
               * 입력된 address.street가 `sherlockStreet`와 일치하면 true를 반환하세요.
               */
              test("quoted") {
                final case class Address(number: String, street: String, postalCode: String)

                val sherlockStreet = "Baker"

                def isSherlockStreet(address: Address): Boolean = address match {
                  case Address(_, street, _) => street == sherlockStreet
                }

                val address = Address("220", "Baker", "NW1 6XE")

                assertTrue(isSherlockStreet(address))
              } @@ ignore
          }
      } +
        suite("Sealed Traits") {
          /**
           * 스칼라에서 sealed trait는 `합 타입(Sum Type)`을 정의할 때 가장 흔히 사용되는 방식입니다. 
           * sealed 키워드는 해당 트레이트를 상속하는 클래스나 객체가 같은 파일 내에만 정의될 수 있도록 제한합니다. 
           * 이 덕분에 컴파일러는 이 트레이트를 상속한 모든 하위 타입을 완전히 파악할 수 있고, 패턴 매칭에서 모든 경우를 빠짐없이 처리했는지 검사할 수 있습니다. 
           * 즉, sealed trait는 타입의 닫힌 계층을 정의할 때 사용하며, 안전하고 예측 가능한 패턴 매칭을 가능하게 해 줍니다.
           * 예를 들면 다음과 같습니다:
           * ```scala
           * sealed trait Animal
           * object Animal {
           *   case class Dog(name: String) extends Animal
           *   case class Cat(name: String) extends Animal
           * }
           * ```
           */

          /**
           * 연습문제-14
           *
           * 다음 trait를 sealed로 만들어 패턴 매칭으로 아래 빈칸을 채워보세요.
           */
          test("sealed") {
            sealed trait Color
            case object Red   extends Color
            case object Green extends Color
            case object Blue  extends Color

            val isRed: Color => Boolean = _ match {
              case Red => true
              case _ => false
            }

            assertTrue(!isRed(Blue))
          } @@ ignore
            /**
             * 연습문제-15
             *
             * `UK`, `USA` 케이스 객체로 확장되는 sealed trait `Country`를 생성하세요.
             */
            test("country") {
              sealed trait Country
              object UK extends Country
              object USA extends Country

              def isCountry(a: Any) = a.isInstanceOf[Country]

              assertTrue(isCountry(UK) && isCountry(USA))
            } @@ ignore
            /**
             * 연습문제-16
             *
             * `asCreditCard` 메서드를 구현할 때 `as` 메서드을 사용하여
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

              def asCreditCard(paymentMethod: PaymentMethod): Option[CreditCard] = paymentMethod match {
                case pp: PayPal => None
                case cc: CreditCard => Some(cc)
              }

              val cc: CreditCard = CreditCard("123123123123", java.time.YearMonth.of(1984, 12), 123)

              assertTrue(asCreditCard(cc) == Option(cc))
            }
            /*
             * 패턴 매칭에서 **as 패턴(as pattern)**은, 패턴으로 값을 분해하면서 분해한 값 전체를 그대로 별도의 이름으로도 저장하고 싶을 때 사용하는 문법입니다.
             *  즉, 내부 값도 꺼내고 싶고, 동시에 원래 값 전체도 변수로 가지고 있고 싶을 때 쓰는 패턴입니다.
             * ```scala
             * sealed trait Shape
             * case class Rectangle(width: Double, height: Double) extends Shape
             * 
             * def describe(shape: Shape): String = shape match {
             *   case rect @ Rectangle(w, h) if w == h =>
             *     s"Square detected: $rect"
             *   case Rectangle(w, h) =>
             *     s"Rectangle of width $w and height $h"
             * }
             * ```
             * 기억해두면 가끔 유용합니다.
             */
        } +
        suite("Modeling") {

          /**
           * 연습문제-17
           *
           * 개인의 관계 상태를 모델링하는 `RelationshipStatus`의 데이터 모델을 만드세요: 기혼, 독신, 이혼.
           */
          test("example 1") {
            sealed trait RelationshipStatus
            object RelationshipStatus {
              case object Married extends RelationshipStatus
              case object Divorced extends RelationshipStatus
              case object Single extends RelationshipStatus
            }

            def makeMarried: RelationshipStatus = RelationshipStatus.Married

            def makeSingle: RelationshipStatus = RelationshipStatus.Single

            assertTrue(makeMarried != makeSingle)
          } @@ ignore
            /**
             * 연습문제-18
             *
             * 연결 URL, 데이터 형식(JSON 또는 XML), API 토큰(문자열)을 저장하는 `PaymentProcessorAPI`의 데이터 모델을 만드세요.
             */
            test("example 2") {
              sealed trait DataFormat
              object DataFormat {
                case object JSON extends DataFormat
                case object XML extends DataFormat
              }
              case class PaymentProcessorAPI(url: java.net.URI, df: DataFormat, apiToken: String)

              def define(url: java.net.URI, df: DataFormat, apiToken: String): PaymentProcessorAPI = PaymentProcessorAPI(url, df, apiToken)

              val url              = new java.net.URI("https://stripe.com")
              def json: DataFormat = DataFormat.JSON

              val api1 = define(url, json, "123123")
              val api2 = define(url, json, "123124")

              assertTrue(api1 == api1 && api1 != api2)
            } @@ ignore
            /**
             * 연습문제-19
             *
             * 사용자의 암호화폐 포트폴리오에 대한 데이터 모델을 생성하세요.
             * Symbol이 같은 경우에만 포트폴리오에 값을 더할 수 있습니다.
             */
            test("example 3") {
              case class Portfolio(symbol: Symbol, amount: Double)

              sealed trait Symbol
              object Symbol {
                case object ETH extends Symbol
                case object BTC extends Symbol
              }

              def ETH: Symbol = Symbol.ETH
              def BTC: Symbol = Symbol.BTC

              def add(portfolio: Portfolio, symbol: Symbol, amount: Double): Portfolio = 
                if (portfolio.symbol == symbol) portfolio.copy(amount = portfolio.amount + amount)
                else portfolio

              def empty(symbol: Symbol): Portfolio = Portfolio(symbol, 0d)

              val p1 = add(add(add(empty(Symbol.ETH), ETH, 1.0), ETH, 1.0), BTC, 2.0)
              val p2 = add(add(empty(Symbol.ETH), BTC, 2.0), ETH, 2.0)

              assertTrue(p1 == p2)
            } @@ ignore
        }
    }
}
