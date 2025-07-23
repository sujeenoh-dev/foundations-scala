/**
 * 함수형 스칼라는 함수형 프로그래밍을 사용합니다. 이는 참조 투명성을 가진 표현식으로
 * 프로그램을 구성하는 것을 강조하며, 함수는 전역성(total)과 결정성(deterministic)을
 * 가집니다. 이 속성은 때때로 _순수성(purity)_ 이라 불리며, 함수형 프로그램에서
 * 제어를 완전히 역전시켜 함수 호출자에게 완전한 제어권을 제공합니다. 이는
 * 프로그램을 이해하기 쉽게 하고, 안전하게 수정하는 비용을 줄이며, 테스트를 더 쉽게 만듭니다.
 *
 * 이 모듈에서는 순수성과 애플리케이션 내에서 순수성을 사용하는 기법들을 탐구하게 됩니다.
 */
package net.degoes

import zio.test._
import zio.test.TestAspect._
import scala.annotation.tailrec
import java.time.temporal.ChronoUnit
import java.time.MonthDay
import scala.util.Random

object Purity extends ZIOSpecDefault {
  def spec =
    suite("Purity") {
      suite("functions") {

        /**
         * 연습문제
         *
         * 다음 함수를 전역 함수로 만드세요.
         */
        test("total 1") {
          def reduce[A](elements: List[A], f: (A, A) => A): A = {
            @tailrec
            def loop(current: A, elements: List[A]): A =
              elements match {
                case Nil              => current
                case next :: elements => loop(f(current, next), elements)
              }

            loop(elements.head, elements.tail)
          }

          assertTrue(reduce[Int](List.empty[Int], _ + _) ne null)
        } @@ ignore +
          /**
           * 연습문제
           *
           * 다음 함수를 전역 함수로 만드세요.
           */
          test("total 2") {
            type Make = String

            def generateEmailSubject(make: Make, city: String, total: Double, start: MonthDay, end: MonthDay)
              : String = {
              val ppd = total / ChronoUnit.DAYS.between(end.atYear(2022), start.atYear(2022)).toDouble

              s"Don't lose your ${make} car rental on your trip to ${city}, for only ${ppd}!"
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
            def normal(): Double = {
              val random1 = Random.nextDouble()
              val random2 = Random.nextDouble()

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
            def getName(): String = {
              println("What is your name?")
              scala.io.StdIn.readLine()
            }

            assertTrue(getName() == getName())
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
object PurityGraduation extends ZIOSpecDefault {
  final case class Stateful[State, +A](compute: State => (State, A)) { self =>
    def map[B](f: A => B): Stateful[State, B] =
      self.flatMap(a => Stateful.succeed(f(a)))

    /**
     * 연습문제
     *
     * 상태가 두 계산을 관통하여 전달되도록 `flatMap` 메서드를 구현하세요.
     */
    def flatMap[B](f: A => Stateful[State, B]): Stateful[State, B] = ???

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
    def succeed[S, A](a: => A): Stateful[S, A] = ???

    /**
     * 연습문제
     *
     * 상태를 수정하지 않고 반환하는 `get`을 구현하세요.
     */
    def get[S]: Stateful[S, S] = ???

    /**
     * 연습문제
     *
     * 상태를 설정하고 unit을 반환하는 `set`을 구현하세요.
     */
    def set[S](s: S): Stateful[S, Unit] = ???

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
