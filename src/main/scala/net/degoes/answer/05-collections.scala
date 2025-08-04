/**
 * **컬렉션으로 데이터 처리하기**
 * 
 * 이제 여러분은 함수형 프로그래밍의 핵심 도구들을 모두 배웠습니다:
 * - 함수와 람다 (00-lambdas)
 * - 데이터 모델링 (01-data)  
 * - Option으로 null 안전성 (02-nulls)
 * - Try와 Either로 예외 처리 (03-exceptions)
 * 
 * 이제 이 모든 지식을 활용하여 컬렉션을 다뤄봅시다!
 * 
 * 실제 프로그래밍에서는 데이터 목록을 처리하는 일이 매우 많습니다:
 * - 사용자 목록에서 특정 조건에 맞는 사용자 찾기
 * - 주문 목록에서 총 금액 계산하기  
 * - 로그 파일에서 에러 메시지만 추출하기
 * 
 * 전통적인 for 루프 대신, Scala는 강력한 컬렉션 메서드들을 제공합니다:
 * - `map`: 각 요소를 변환
 * - `filter`: 조건에 맞는 요소만 선택
 * - `find`: 조건에 맞는 첫 번째 요소 찾기 (Option 반환! - 이제 이해할 수 있습니다)
 * - `headOption`: 안전한 첫 번째 요소 접근 (Option 반환)
 * - `fold`: 모든 요소를 하나의 값으로 축약
 * 
 * Option을 이미 배웠기 때문에, 컬렉션 메서드들이 왜 Option을 반환하는지, 
 * 그리고 이를 어떻게 안전하게 사용하는지 자연스럽게 이해할 수 있을 것입니다.
 */
package net.degoes

import zio.test._
import zio.test.TestAspect._

object CollectionsAnswer extends ZIOSpecDefault {
  def spec =
    suite("Collections") {
      suite("operations") {

        /**
         * 연습문제-01
         *
         * `List#foreach`를 사용하여, 주어진 리스트의 모든 값을 변수 `sum`에 더하세요.
         */
        test("foreach") {
          var sum = 0

          val list = List(0, 3, 0, 2, 1)

          list.foreach(sum += _)

          assertTrue(sum == 6)
        } @@ ignore
          /**
           * 연습문제-02
           *
           * `List#map`을 사용하여, 주어진 리스트의 각 요소를 2배로 곱한 새로운 리스트로 변환하세요.
           */
          test("map") {
            val list1 = List(0, 3, 0, 2, 1)

            val list2 = list1.map(_ * 2)

            assertTrue(list2.sum == 12 && list2.length == list1.length)
          } @@ ignore
          /**
           * 연습문제-03
           *
           * `List#filter`를 사용하여, 주어진 리스트에서 짝수만 남도록 필터링하세요.
           */
          test("filter") {
            val isEven = (i: Int) => i % 2 == 0

            val list1 = List(0, 3, 0, 2, 1)

            val list2 = list1.filter(isEven)

            assertTrue(list2 == List(0, 0, 2))
          } @@ ignore
          /**
           * 연습문제-04
           *
           * `List#take`을 사용하여, 주어진 리스트의 앞에서 2개의 요소만 취하세요.
           */
          test("take") {
            val list1 = List(1, 2, 3, 4)

            val list2 = list1.take(2)

            assertTrue(list2 == List(1, 2))
          } @@ ignore
          /**
           * 연습문제-05
           *
           * `List#takeWhile`을 사용하여, 요소가 3보다 작은 동안만 리스트에서 값을 취하세요.
           */
          test("takeWhile") {
            val list1 = List(1, 2, 0, 3, 1, 2)

            val list2 = list1.takeWhile(_ < 3)

            assertTrue(list2 == List(1, 2, 0))
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#drop`을 사용하여, 주어진 리스트의 앞에서 2개의 요소를 버리세요.
           */
          test("drop") {
            val list1 = List(1, 2, 3, 4)

            val list2 = list1.drop(2)

            assertTrue(list2 == List(3, 4))
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#dropWhile`을 사용하여, 요소가 3보다 작은 동안만 리스트에서 값을 버리세요.
           */
          test("dropWhile") {
            val list1 = List(1, 2, 0, 3, 1, 2)

            val list2 = list1.dropWhile(_ < 3)

            assertTrue(list2 == List(3, 1, 2))
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#collect`과 부분 함수를 사용하여, 주어진 리스트에서 짝수만 골라내고, 이를 `Even` 래퍼 타입으로 감싸세요.
           */
          test("collect") {
            final case class Even(number: Int)

            val isEven = (i: Int) => i % 2 == 0

            val list1 = List(0, 3, 0, 2, 1)

            def list2: List[Even] = list1.collect { case x if isEven(x) => Even(x) }

            assertTrue(list2 == List(Even(0), Even(0), Even(2)))
          } @@ ignore
          /**
           * 연습문제
           *
           * `partition`을 사용하여, 주어진 정수 리스트를 짝수와 홀수로 분리하세요.
           */
          test("partition") {
            val isEven = (i: Int) => i % 2 == 0

            val list = List(0, 3, 0, 2, 1)

            val (even, odd) = list.partition(isEven)

            assertTrue(even == List(0, 0, 2) && odd == List(3, 1))
          } @@ ignore
          /**
           * 연습문제
           *
           * `reduceOption`을 사용하여, 주어진 두 리스트의 합을 구하세요.
           * `reduceOption`이 `None`을 반환하는 경우는 언제인가요?
           */
          test("reduceOption") {
            val list1: List[Int] = List()
            val list2: List[Int] = List(1, 2, 3)

            def summedList1: Option[Int] = list1.reduceOption(_ + _)
            def summedList2: Option[Int] = list2.reduceOption(_ + _)

            assertTrue(summedList1 == None && summedList2 == Some(6))
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#find`를 사용하여, 주어진 리스트에서 2보다 큰 첫 번째 숫자를 찾으세요.
           */
          test("find") {
            val list = List(1, 2, 3, 4)

            def firstGreaterThan2: Option[Int] = list.find(_ > 2)

            assertTrue(firstGreaterThan2 == Some(3))
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#exists`를 사용하여, 주어진 리스트에 음수가 존재하는지 확인하세요.
           */
          test("exists") {
            val list = List(1, 2, 3, 4, -1)

            def existsNegative: Boolean = list.exists(_ < 0)

            assertTrue(existsNegative)
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#forall`를 사용하여, 주어진 리스트의 모든 요소가 짝수인지 확인하세요.
           */
          test("forall") {
            val isEven = (i: Int) => i % 2 == 0

            val list = List(0, 2, 6, 8, 12, 10)

            def forallEven: Boolean = list.forall(isEven)

            assertTrue(forallEven)
          } @@ ignore
          suite("folds") {

            /**
             * 연습문제
             *
             * `List#foldLeft`를 사용하여, 주어진 리스트의 합을 구하세요.
             */
            test("sum") {
              def sum(list: List[Int]): Int = list.foldLeft(0) { (acc, ele) =>
                acc + ele
              }

              assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
            } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트의 최대 요소를 구하세요.
               * 리스트의 원소는 모두 양수입니다
               */
              test("max") {
                def max(list: List[Int]): Int = list.foldLeft(0) { (acc, ele) =>
                  acc max ele
                }

                assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
              } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트의 최소 요소를 구하세요.
               * 리스트 안 원소의 최댓값은 100입니다.
               */
              test("min") {
                def min(list: List[Int]): Int = list.foldLeft(100) { (acc, ele) =>
                  acc min ele
                }

                assertTrue(min(List(1, 7, 3, 2, 0, 4, 5)) == 0)
              } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트를 뒤집으세요.
               */
              test("reverse") {
                def reverse[A](list: List[A]): List[A] = list.foldLeft (List.empty[A]) { (acc, ele) =>
                  ele :: acc
                }

                assertTrue(reverse(List(1, 7, 3)) == List(3, 7, 1))
              } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트를 조건에 맞는 요소와 그렇지 않은 요소로 분리하는 함수를 구현하세요.
               */
              test("partition") {
                def partition[A](list: List[A])(pred: A => Boolean): (List[A], List[A]) = list.foldLeft((List.empty[A], List.empty[A])) { 
                  case ((isTrue, isFalse), ele) => if (pred(ele)) (ele :: isTrue, isFalse) else (isTrue, ele :: isFalse)
                }

                assertTrue(partition(List(1, 7, 3))(_ < 5) == (List(3, 1), List(7)))
              } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트에서 `n`개의 요소를 취하는 함수를 구현하세요.
               */
              test("take") {
                def take[A](n: Int, list: List[A]): List[A] = {
                  val (taken, _) = list.foldLeft((List.empty[A], 0)) { case ((acc, count), ele) =>
                    if (count < n) (ele :: acc, count + 1) else (acc, count)
                  }
                  taken.reverse
                }

                assertTrue(take(2, List(1, 7, 3)) == List(1, 7))
              } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트에서 조건에 맞는 요소를 취하는 함수를 구현하세요.
               */
              test("takeWhile") {
                def takeWhile[A](list: List[A])(pred: A => Boolean): List[A] = list.foldLeft((List.empty[A], false)) { 
                  case ((acc, stopped), ele) =>
                    if (pred(ele) && !stopped) (ele :: acc, false)
                    else (acc, true)
                }._1

                assertTrue(takeWhile(List(1, 7, 3))(_ < 5).reverse == List(1))
              } @@ ignore
          }
      } +
        suite("performance") {

          /**
           * 연습문제
           *
           * 코드의 성능 문제를 해결하기 위해, 사용된 컬렉션 타입만 변경하세요.
           */
          test("head/tail") {
            def sum(values: List[Int]): Int =
              values match {
                case Nil        => 0
                case head :: tail => head + sum(tail.drop(1))
              }

            assertTrue(sum((0 to 10000).toList) > 0)
          } @@ ignore
            /**
             * 연습문제
             *
             * 이 코드의 성능 문제를, 컬렉션 타입만 변경하여 해결해 보세요.
             */
            test("random access") {
              def sumProduct(left: Vector[Int], right: Vector[Int]): Int = {
                val length = left.length.max(right.length)

                (0 to length).foldLeft(0) {
                  case (sum, index) => sum + left(index) * right(index)
                }
              }

              assertTrue(sumProduct(Vector.fill(1000)(2), Vector.fill(1000)(2)) > 0)
            } @@ ignore
            /**
             * 연습문제
             *
             * 이 코드의 성능 문제를, 컬렉션 타입만 변경하여 해결해 보세요.
             */
            test("containment") {
              def equivalent(left: Set[Int], right: Set[Int]): Boolean =
                left.forall(i => right.contains(i)) &&
                  right.forall(i => left.contains(i))

              assertTrue(equivalent(Set.fill(1000)(2), Set.fill(1000)(2)))
            } @@ ignore
        }
    }
}
