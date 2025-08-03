/**
 * **재귀: 함수형 프로그래밍의 반복문**
 * 
 * 지금까지 우리는 데이터를 모델링하고(case class, sealed trait), 
 * 안전한 타입을 사용하고(Option, Try, Either), 
 * 컬렉션으로 데이터를 처리하는 방법을 배웠습니다.
 * 
 * 하지만 때로는 더 직접적인 제어가 필요합니다. 예를 들어:
 * - 복잡한 알고리즘 구현 (정렬, 탐색)
 * - 트리나 그래프 같은 중첩된 구조 처리
 * - 컬렉션 메서드로는 표현하기 어려운 로직
 * 
 * 함수형 프로그래밍에서는 전통적인 for/while 루프 대신 **재귀**를 사용합니다.
 * 재귀의 장점:
 * - 불변 데이터와 잘 어울림 (var 없이도 반복 가능)
 * - 수학적 정의와 유사해서 이해하기 쉬움
 * - 복잡한 데이터 구조를 자연스럽게 처리
 * 
 * **재귀의 기본 패턴:**
 * 1. **종료 조건(Base case)**: 더 이상 재귀하지 않는 경우
 * 2. **재귀 호출(Recursive case)**: 문제를 작게 만들어서 자기 자신을 호출
 * 
 * 이 모듈에서는 단순한 재귀부터 시작해서 꼬리재귀까지 단계적으로 배워봅시다.
 */
package net.degoes

import zio.test._
import zio.test.TestAspect._

object RecursionAnswer extends ZIOSpecDefault {
  def spec =
    suite("Recursion") {
      suite("basics") {

        /**
         * 연습문제-01
         *
         * 재귀를 사용하여 정수 리스트의 합을 계산하세요.
         */
        test("sum") {
          def sum(list: List[Int]): Int = list match {
            case Nil => 0
            case head :: tail => head + sum(tail)
          }

          assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
        } @@ ignore +
          /**
           * 연습문제-02
           *
           * 재귀를 사용하여 정수 리스트의 최대값을 계산하세요.
           * 리스트가 비어 있다면 아무 값이나 리턴하세요.
           * 리스트 안의 값은 모두 양수입니다.
           */
          test("max") {
            def max(list: List[Int]): Int = list match {
              case Nil => 0
              case head :: tail => head max max(tail)
            }

            assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
          } @@ ignore +
          /**
           * 연습문제-03
           *
           * 재귀를 사용하여 수가 소수인지 판별하세요.
           */
          test("prime") {
            def isPrime(n: Int): Boolean = {
              def loop(n: Int, divisor: Int): Boolean = 
                if (divisor * divisor > n) true
                else if (n % divisor == 0) false
                else loop(n, divisor + 1)

              loop(n, 2)
            }

            assertTrue(!isPrime(4) && isPrime(7) && isPrime(11))
          } @@ ignore +
          /**
           * 연습문제-04
           *
           * 재귀를 사용하여 n번째 피보나치 수를 계산하세요. 피보나치
           * 수열은 0, 1, 1, <이전 두 수의 합>... 으로 정의됩니다.
           */
          test("fibs") {
            def fib(n: Int): Int = 
              if (n > 1) fib(n - 1) + fib(n - 2)
              else n

            assertTrue(fib(3) == 2 && fib(4) == 3 && fib(5) == 5)
          } @@ ignore +
          /**
           * 연습문제-05
           *
           * 재귀를 사용하여 제공된 리스트를 정렬하세요. 헤드를 선택하고,
           * 헤드보다 작은 것들과 작지 않은 것들을 (각각) 정렬한 다음,
           * 올바른 순서로 연결하는 방식으로 정렬하세요.
           */
          test("pivot sort") {
            def sort[A](list: List[A])(implicit ordering: Ordering[A]): List[A] = list match {
              case Nil => list
              case head :: tail => 
                val (less, more) = tail.partition(ordering.lt(_, head))
                sort(less) ++ (head :: sort(more))
            }

            assertTrue(sort(List(9, 23, 1, 5)) == List(1, 5, 9, 23))
          } @@ ignore +
          /**
           * 연습문제-06
           *
           * 재귀를 사용하여 조건(predicate)이 만족될 때까지 반복하는
           * 메서드를 구현하세요.
           */
          test("loop") {
            def loop[S](start: S)(pred: S => Boolean)(iterate: S => S): S = 
              if (pred(start)) start else loop(iterate(start))(pred)(iterate)

            val inc = loop(0)(_ < 10)(_ + 1)

            assertTrue(inc == 10)
          } @@ ignore +
          /**
           * 연습문제-07
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

            def repeatWhile[A, S](action: () => A)(pred: A => Boolean)(reducer: (A, A) => A): A = {
              val current = action()
              if (pred(current)) reducer(current, repeatWhile(action)(pred)(reducer)) else current
            }

            val result = repeatWhile(readLine)(_ == "Sherlock")((a, b) => b)

            assertTrue(result == "Sherlock")
          } @@ ignore

      } +
        suite("tail recursion") {
          /* TODO : 꼬리재귀 설명 추가
           */
          
          import scala.annotation.tailrec
          /**
           * 연습문제-08
           *
           * 이전 `sum`의 꼬리 재귀 버전을 작성하세요.
           * 파라미터를 조정해도 좋습니다.
           */
          test("sum") {
            @tailrec
            def sum(list: List[Int], acc: Int = 0): Int = list match {
              case Nil => acc
              case head :: tail => sum(tail, acc + head)
            }

            assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
          } @@ ignore +
            /**
             * 연습문제-09
             *
             * 이전 `max`의 꼬리 재귀 버전을 작성하세요.
             * 파라미터를 조정해도 좋습니다.
             * 모든 원소는 양수입니다.
             */
            test("max") {
              @tailrec
              def max(list: List[Int], acc: Int = 0): Int = list match {
                case Nil => acc
                case head :: tail => max(tail, acc max head)
              }

              assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
            } @@ ignore +
            /**
             * 연습문제-10
             *
             * 이전 `loop`의 꼬리 재귀 버전을 작성하세요.
             * 파라미터를 조정해도 좋습니다.
             */
            test("loop") {
              @tailrec
              def loop[S](start: S)(pred: S => Boolean)(iterate: S => S): S = 
                if (pred(start)) start else loop(iterate(start))(pred)(iterate)

              val inc = loop(0)(_ < 10)(_ + 1)

              assertTrue(inc == 10)
            } @@ ignore +
            /**
             * 연습문제-11
             *
             * 이전 `repeat`의 꼬리 재귀 버전을 작성하세요.
             * 새로운 메서드를 만들거나 파라미터를 변경해도 좋습니다.
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

              
              def repeatWhile[A, S](action: () => A)(pred: A => Boolean)(reducer: (A, A) => A): A = {
                @tailrec
                def loop(current: A): A = {
                  val next = action()
                  if(pred(next)) next
                  else loop(reducer(current, next))
                }

                val first: A = action()
                if (pred(first)) first
                else loop(first)
              }


              val result = repeatWhile(readLine)(_ == "Sherlock")((a, b) => b)

              assertTrue(result == "Sherlock")
            } @@ ignore +
            /**
             * 연습문제-12
             *
             * 꼬리 재귀를 사용하여 피보나치 수열을 작성하는 방법을 찾아보세요.
             *
             * 주의: 고급 문제입니다.
             */
            test("fibs") {
              @tailrec
              def fib(n: Int, acc: Int = 1, left: Int = 0, right: Int = 1): Int = 
                if (acc <= n) fib(n, acc + 1, right, left + right)
                else right

              assertTrue(fib(3) == 2 && fib(4) == 3 && fib(5) == 5)
            } @@ ignore +
            /**
             * 연습문제-13
             *
             * 꼬리 재귀를 사용하여 피벗 정렬을 작성하는 방법을 찾아보세요.
             *
             * 주의: 고급 문제입니다.
             */
            test("pivot sort with tail recursion") {
              def sort[A](list: List[A])(implicit ordering: Ordering[A]): List[A] = {
                @tailrec
                def loop(stack: List[List[A]], acc: List[A]): List[A] = stack match {
                  case Nil => acc.reverse
                  case Nil :: rest => loop(rest, acc)
                  case (pivot :: tail) :: rest =>
                    val (left, right) = tail.partition(ordering.lt(_, pivot))
                    loop(left :: Nil :: right :: rest, pivot :: acc)
                }

                loop(List(list), Nil).asInstanceOf[List[A]]
              }

              assertTrue(sort(List(9, 23, 1, 5)) == List(1, 5, 9, 23))
            } @@ ignore
        }
    }
}
