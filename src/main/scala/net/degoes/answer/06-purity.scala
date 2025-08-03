/**
 * 함수형 스칼라는 함수형 프로그래밍을 기반으로 하며, 프로그램을 **참조 투명한 표현식(expression)**들로 구성하는 것을 강조합니다. 
 * 이때 사용되는 함수들은 보통 다음 두 가지 중요한 성질을 가집니다:
 * 1. 전역성(totality): 모든 유효한 입력에 대해 항상 결과를 반환하며, 예외를 던지거나 정의되지 않은 동작을 하지 않습니다.
 * 2. 결정성(determinism): 동일한 입력에 대해 항상 동일한 결과를 반환합니다.
 * 이 두 성질이 결합된 함수를 우리는 **순수한 함수(pure function)**라고 부릅니다. 순수한 함수는 외부 상태나 부작용에 의존하지 않기 때문에, 
 * 프로그램을 더 쉽게 이해하고 테스트하며 안전하게 수정할 수 있게 해줍니다.
 * 또한, 순수한 함수 기반의 설계는 프로그램의 흐름 제어를 명시적으로 만들고, 함수 호출자에게 더 큰 제어권을 부여하여 예측 가능하고 안정적인 코드 작성을 가능하게 합니다.
 *
 * 예를 들어, def add(a: Int, b: Int): Int = a + b는 어떤 입력이든 항상 잘 동작하며 같은 결과를 주기 때문에 순수하고 total한 함수입니다. 
 * 반면 list.head는 빈 리스트일 경우 예외를 던지므로 total하지 않으며, 순수하지 않은 코드 흐름의 원인이 될 수 있습니다.
 */
package net.degoes

import zio.test._
import zio.test.TestAspect._
import scala.annotation.tailrec
import java.time.temporal.ChronoUnit
import java.time.MonthDay
import scala.util.Random

object PurityAnswer extends ZIOSpecDefault {
  def spec =
    suite("Purity") {
      suite("functions") {

        /**
         * 연습문제
         *
         * 다음 함수를 모든 유효한 입력에 대해 정의된 함수(total function)로 만드세요.
         * => 입력 도메인의 모든 값에 대해 문제없이 작동해야 합니다.
         * 예를 들어, 리턴 타입에 명시되지 않은 에러가 발생하는 등의 현상이 없어야 합니다.
         */
        test("total 1") {
          def reduce[A](elements: List[A], f: (A, A) => A): Option[A] = {
            @tailrec
            def loop(current: A, elements: List[A]): A =
              elements match {
                case Nil              => current
                case next :: elements => loop(f(current, next), elements)
              }

            elements match {
              case Nil => None
              case head :: tail => Some(loop(head, tail))
            }
          }

          assertTrue(reduce[Int](List.empty[Int], _ + _) ne null)
        } @@ ignore +
          /**
           * 연습문제
           *
           * 다음 함수를 total function으로 만드세요.
           */
          test("total 2") {
            type Make = String

            def generateEmailSubject(make: Make, city: String, total: Double, start: MonthDay, end: MonthDay)
              : String = {
              val gap = ChronoUnit.DAYS.between(start.atYear(2022), end.atYear(2022))
              val ppd = if (gap == 0) None else Some(total / gap.toDouble)

              s"Don't lose your ${make} car rental on your trip to ${city}, for only ${ppd.getOrElse(0d)}!"
            }

            assertTrue(
              generateEmailSubject("Toyota Accord", "New York City", 295.00, MonthDay.now(), MonthDay.now()) != null
            )
          } @@ ignore +
          /**
           * 연습문제
           *
           * 다음 함수를 순수 함수로 만드세요.
           */
          test("pure 1") {
            def normal(seed: Long = 1): Double = {
              val rng = new Random(seed)
              val random1 = rng.nextDouble()
              val random2 = rng.nextDouble()

              -2.0 * Math.log(random1) * Math.cos(2 * Math.PI * random2)
            }

            assertTrue(normal() == normal())
          } @@ ignore +
          /**
           * 연습문제
           *
           * 다음 함수를 순수 함수로 만드세요.
           *
           * 주의: 고급 문제입니다.
           */
          test("pure 2") {
            def getName(readLine: () => String, println: String => Unit): String = {
              println("What is your name?")
              readLine()
            }

            def mockReadLine(): String = "test"
            def mockPrintln(s: String): Unit = ()
            assertTrue(getName(mockReadLine, mockPrintln) == getName(mockReadLine, mockPrintln))
          } @@ ignore
      }
    }
}

/**
 * 함수형 프로그래밍은 원칙에 기반한 방식으로 코드를 추론하고 테스트하는 전례 없는 능력을
 * 제공합니다. 이 능력의 비밀은 소위 참조 투명성입니다. 이는 값을 계산하는 코드만 작성하는 것을
 * 포함하며, 모든 함수는 전역적이고 결정적이며 부작용이 없습니다.
 *
 * 이 졸업 프로젝트에서는 상태를 가진 계산을 모델링하는 데이터 타입을 구축하고,
 * 이를 사용하여 절차적 스타일의 애플리케이션을 함수형 스타일로 업데이트하면서
 * 순수한 상태 계산의 한계를 탐구하게 됩니다.
 */
object PurityGraduationAnswer extends ZIOSpecDefault {
  final case class Stateful[State, +A](compute: State => (State, A)) { self =>
    def map[B](f: A => B): Stateful[State, B] =
      self.flatMap(a => Stateful.succeed(f(a)))

    /**
     * 연습문제
     *
     * 상태가 두 계산을 관통하여 전달되도록 `flatMap` 메서드를 구현하세요.
     */
    def flatMap[B](f: A => Stateful[State, B]): Stateful[State, B] = 
      Stateful(state => {
        val (newState, a) = self.compute(state)
        f(a).compute(newState)
      })

    def zip[B](that: => Stateful[State, B]): Stateful[State, (A, B)] =
      for {
        a <- self
        b <- that
      } yield (a, b)
  }
  object Stateful {

    /**
     * 연습문제
     *
     * 상태를 변경하지 않는 방식으로 `succeed` 메서드를 구현하세요.
     */
    def succeed[S, A](a: => A): Stateful[S, A] = Stateful(state => (state, a))

    /**
     * 연습문제
     *
     * 상태를 수정하지 않고 반환하는 `get`을 구현하세요.
     */
    def get[S]: Stateful[S, S] = Stateful(state => (state, state))

    /**
     * 연습문제
     *
     * 상태를 설정하고 unit을 반환하는 `set`을 구현하세요.
     */
    def set[S](s: S): Stateful[S, Unit] = Stateful(_ => (s, ()))

    def update[S](f: S => S): Stateful[S, S] =
      for {
        s <- Stateful.get[S]
        s <- Stateful.succeed(f(s))
        _ <- set(s)
      } yield s
  }

  final case class Element(
    tag: String,
    attributes: Map[String, String] = Map(),
    children: List[Element] = Nil
  )

  /**
   * 위에서 구성한 `Stateful` 데이터 타입을 사용하여 이 절차적 예제를
   * 함수형 스칼라로 이식하세요.
   */
  def render(element: Element): String = {
    var output: String = ""
    var indent: Int    = 0

    def increaseIndent() = indent += 1

    def decreaseIndent() = indent -= 1

    def print(s: String): Unit = output += s

    def printIndent() = print("  " * indent)

    def printNewline() = {
      print("\n")
      printIndent()
    }

    def printOpenTag(tag: String, attrs: Map[String, String]) = {
      printNewline()
      increaseIndent()
      if (attrs.isEmpty) print(s"<${tag}>")
      else {
        val renderAttrs = attrs.map {
          case (key, value) => key + "=\"" + value + "\""
        }.mkString(" ")

        print(s"<${tag} ${renderAttrs}>")
      }
    }

    def printCloseTag(tag: String) = {
      decreaseIndent()
      printNewline()
      print(s"</${tag}>")
    }

    def loop(element: Element): Unit = {
      printOpenTag(element.tag, element.attributes)

      if (element.children.nonEmpty) {
        printNewline()
        element.children.foreach(loop(_))
        printNewline()
      }

      printCloseTag(element.tag)
    }

    loop(element)

    output
  }

  val data =
    Element("body", Map(), List(Element("h1"), Element("p")))

  val expected =
    """
      |<body>
      |  
      |  <h1>
      |  </h1>
      |  <p>
      |  </p>
      |  
      |</body>""".stripMargin

  def spec =
    test("conversion") {
      assertTrue(render(data) == expected)
    } @@ ignore
}
