/**
 * 애플리케이션의 기반 중 하나는 반복하는 능력입니다: 반복은 대부분의 알고리즘(리스트 정렬 등)과
 * 데이터베이스 쿼리에서 결과를 반복적으로 읽거나 소켓에서 요청을 반복적으로 처리하는 등의
 * 대규모 애플리케이션 동작에 사용됩니다. 함수형 애플리케이션은 전통적인 루프를 사용하지 않고,
 * 대신 재귀를 사용합니다. 재귀는 루프와 같은 능력을 가지지만 불변 데이터와 순수 함수에
 * 더 적합한 형태로 제공됩니다.
 *
 * 이 모듈에서는 절차적 프로그래밍 언어에서 루프로 해결했을 같은 작업들을 재귀로 해결하는
 * 방법을 탐구하게 됩니다.
 */
package net.degoes

import zio.test._
import zio.test.TestAspect._

object Recursion extends ZIOSpecDefault {
  def spec =
    suite("Recursion") {
      suite("basics") {

        /**
         * 연습문제
         *
         * 재귀를 사용하여 정수 리스트의 합을 계산하세요.
         */
        test("sum") {
          def sum(list: List[Int]): Int = ???

          assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
        } @@ ignore
          /**
           * 연습문제
           *
           * 재귀를 사용하여 정수 리스트의 최대값을 계산하세요.
           */
          test("max") {
            def max(list: List[Int]): Int = ???

            assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
          } @@ ignore
          /**
           * 연습문제
           *
           * 재귀를 사용하여 수가 소수인지 판별하세요.
           */
          test("prime") {
            def isPrime(n: Int): Boolean = {
              def loop(n: Int, divisor: Int): Boolean = ???

              loop(n, 2)
            }

            assertTrue(!isPrime(4) && isPrime(7) && isPrime(11))
          } @@ ignore
          /**
           * 연습문제
           *
           * 재귀를 사용하여 n번째 피보나치 수를 계산하세요. 피보나치
           * 수열은 0, 1, 1, <이전 두 수의 합>... 으로 정의됩니다.
           */
          test("fibs") {
            def fib(n: Int): Int = ???

            assertTrue(fib(3) == 2 && fib(4) == 3 && fib(5) == 5)
          } @@ ignore
          /**
           * 연습문제
           *
           * 재귀를 사용하여 제공된 리스트를 정렬하세요. 헤드를 선택하고,
           * 헤드보다 작은 것들과 작지 않은 것들을 (각각) 정렬한 다음,
           * 올바른 순서로 연결하는 방식으로 정렬하세요.
           */
          test("pivot sort") {
            def sort[A](list: List[A])(implicit ordering: Ordering[A]): List[A] = ???

            assertTrue(sort(List(9, 23, 1, 5)) == List(1, 5, 9, 23))
          } @@ ignore
          /**
           * 연습문제
           *
           * 재귀를 사용하여 조건(predicate)이 만족될 때까지 반복하는
           * 메서드를 구현하세요.
           */
          test("loop") {
            def loop[S](start: S)(pred: S => Boolean)(iterate: S => S): S = ???

            val inc = loop(0)(_ < 10)(_ + 1)

            assertTrue(inc == 10)
          } @@ ignore
          /**
           * 연습문제
           *
           * 재귀를 사용하여 조건(predicate)이 참이 될 때까지 지정된
           * 액션을 계속 반복하는 메서드를 구현하세요.
           */
          test("repeat") {
            var input = "John" :: "Scotty" :: "Bob" :: "Sherlock" :: Nil

            val readLine: () => String = () =>
              input match {
                case Nil => ""
                case head :: tail =>
                  input = tail
                  head
              }

            def repeatWhile[A, S](action: () => A)(pred: A => Boolean)(reducer: (A, A) => A): A = ???

            val result = repeatWhile(readLine)(_ == "Sherlock")((a, b) => b)

            assertTrue(result == "Sherlock")
          } @@ ignore

      } +
        suite("tail recursion") {
          // import scala.annotation.tailrec
          /**
           * 연습문제
           *
           * 이전 `sum`의 꼬리 재귀 버전을 작성하세요.
           */
          test("sum") {
            // @tailrec
            def sum(list: List[Int]): Int = ???

            assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
          } @@ ignore
            /**
             * 연습문제
             *
             * 이전 `max`의 꼬리 재귀 버전을 작성하세요.
             */
            test("max") {
              // @tailrec
              def max(list: List[Int]): Int = ???

              assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
            } @@ ignore
            /**
             * 연습문제
             *
             * 이전 `loop`의 꼬리 재귀 버전을 작성하세요.
             */
            test("loop") {
              def loop[S](start: S)(pred: S => Boolean)(iterate: S => S): S = ???

              val inc = loop(0)(_ < 10)(_ + 1)

              assertTrue(inc == 10)
            } @@ ignore
            /**
             * 연습문제
             *
             * 이전 `repeat`의 꼬리 재귀 버전을 작성하세요.
             */
            test("repeat") {
              var input = "John" :: "Scotty" :: "Bob" :: "Sherlock" :: Nil

              val readLine: () => String = () =>
                input match {
                  case Nil => ""
                  case head :: tail =>
                    input = tail
                    head
                }

              def repeatWhile[A, S](action: () => A)(pred: A => Boolean)(reducer: (A, A) => A): A = ???

              val result = repeatWhile(readLine)(_ == "Sherlock")((a, b) => b)

              assertTrue(result == "Sherlock")
            } @@ ignore
            /**
             * 연습문제
             *
             * 꼬리 재귀를 사용하여 피보나치 수열을 작성하는 방법을 찾아보세요.
             *
             * 주의: 고급 문제입니다.
             */
            test("fibs") {
              def fib(n: Int): Int = ???

              assertTrue(fib(3) == 2 && fib(4) == 3 && fib(5) == 5)
            } @@ ignore
            /**
             * 연습문제
             *
             * 꼬리 재귀를 사용하여 피벗 정렬을 작성하는 방법을 찾아보세요.
             *
             * 주의: 고급 문제입니다.
             */
            test("pivot sort") {
              def sort[A](list: List[A])(implicit ordering: Ordering[A]): List[A] = ???

              assertTrue(sort(List(9, 23, 1, 5)) == List(1, 5, 9, 23))
            } @@ ignore
        }
    }
}

/**
 * 재귀는 함수형 스칼라에서 루프의 범용적 대체재입니다. 더 간단한 대안이
 * 존재할 때(예: List의 foldLeft)는 재귀를 피해야 하지만, 그렇지 않은 경우
 * 재귀는 가장 복잡한 반복 문제를 해결할 수 있는 매우 강력한 도구를 제공합니다.
 *
 * 이 졸업 프로젝트에서는 행맨 게임을 구현하면서 함수형 이펙트 시스템에서
 * 재귀를 실험해 볼 수 있습니다.
 */
object RecursionGraduation extends zio.ZIOAppDefault {
  import zio._
  import java.io.IOException

  /**
   * 연습문제
   *
   * 사용자로부터 하나의 소문자를 받는 이펙트를 구현하세요.
   */
  lazy val getChoice: ZIO[Any, IOException, Char] = ???

  /**
   * 연습문제
   *
   * 사용자에게 이름을 묻는 프롬프트를 표시하고 그 이름을 반환하는
   * 이펙트를 구현하세요.
   */
  lazy val getName: ZIO[Any, IOException, String] = ???

  /**
   * 연습문제
   *
   * 사전에서 랜덤한 단어를 선택하는 이펙트를 구현하세요.
   * 사전은 `Dictionary`입니다.
   */
  lazy val chooseWord: ZIO[Any, Nothing, String] = ???

  /**
   * 연습문제
   *
   * 게임이 이기거나 질 때까지 사용자로부터 선택을 받는 메인 게임 루프를
   * 구현하세요.
   */
  def gameLoop(oldState: State): ZIO[Any, IOException, Unit] = ???

  def renderState(state: State): ZIO[Any, IOException, Unit] = {

    /**
     *
     *  f     n  c  t  o
     *  -  -  -  -  -  -  -
     *
     *  Guesses: a, z, y, x
     *
     */
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) s" $c " else "   ")
        .mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    Console.printLine(text)
  }

  final case class State(name: String, guesses: Set[Char], word: String) {
    final def failures: Int = (guesses -- word.toSet).size

    final def playerLost: Boolean = failures > 10

    final def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    final def addChar(char: Char): State = copy(guesses = guesses + char)
  }

  final case class Step(output: String, keepPlaying: Boolean, state: State)

  def analyzeChoice(
    oldState: State,
    char: Char
  ): Step = {
    val newState = oldState.addChar(char)

    if (oldState.guesses.contains(char))
      Step("You already guessed this character!", true, newState)
    else if (newState.playerWon)
      Step("Congratulations, you won!!!", false, newState)
    else if (newState.playerLost)
      Step(s"Sorry, ${oldState.name}, you lost. Try again soon!", false, newState)
    else if (oldState.word.contains(char))
      Step(s"Good work, ${oldState.name}, you got that right! Keep going!!!", true, newState)
    else Step(s"Uh, oh! That choice is not correct. Keep trying!", true, newState)
  }

  /**
   * 연습문제
   *
   * 메인 함수를 실행하고 프로그램이 의도한 대로 작동하는지 확인하세요.
   */
  def run =
    for {
      name  <- getName
      word  <- chooseWord
      state = State(name, Set(), word)
      _     <- renderState(state)
      _     <- gameLoop(state)
    } yield ()
}
