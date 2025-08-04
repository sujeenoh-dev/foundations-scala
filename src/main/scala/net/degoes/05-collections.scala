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

object Collections extends ZIOSpecDefault {
  def spec =
    suite("Collections") {
      suite("operations") {

        /**
         * EXERCISE 1
         *
         * Use `List#foreach` to add all values in the given list to the variable `sum`.
         */
        test("foreach") {
          var sum = 0

          val list = List(0, 3, 0, 2, 1)

          val _ = list

          sum = ???

          assertTrue(sum == 6)
        } @@ ignore
          /**
           * EXERCISE 2
           *
           * Use `List#map` to transform each element by doubling it.
           */
          test("map") {
            val list1 = List(0, 3, 0, 2, 1)

            val list2: List[Int] = list1.map(???)

            assertTrue(list2.sum == 12 && list2.length == list1.length)
          } @@ ignore
          /**
           * EXERCISE 3
           *
           * Use `List#filter` to keep only even numbers from the list.
           */
          test("filter") {
            val isEven = (i: Int) => i % 2 == 0

            val _ = isEven

            val list1 = List(0, 3, 0, 2, 1)

            val list2 = list1.filter(???)

            assertTrue(list2 == List(0, 0, 2))
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#take`을 사용하여, 주어진 리스트의 앞에서 2개의 요소만 취하세요.
           */
          test("take") {
            val list1 = List(1, 2, 3, 4)

            val list2 = list1

            assertTrue(list2 == List(1, 2))
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#takeWhile`을 사용하여, 요소가 3보다 작은 동안만 리스트에서 값을 취하세요.
           */
          test("takeWhile") {
            val list1 = List(1, 2, 0, 3, 1, 2)

            val list2 = list1

            assertTrue(list2 == List(1, 2, 0))
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#drop`을 사용하여, 주어진 리스트의 앞에서 2개의 요소를 버리세요.
           */
          test("drop") {
            val list1 = List(1, 2, 3, 4)

            val list2 = list1

            assertTrue(list2 == List(3, 4))
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#dropWhile`을 사용하여, 요소가 3보다 작은 동안만 리스트에서 값을 버리세요.
           */
          test("dropWhile") {
            val list1 = List(1, 2, 0, 3, 1, 2)

            val list2 = list1

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

            val _ = isEven

            val list1 = List(0, 3, 0, 2, 1)

            def list2: List[Even] = list1.collect(???)

            assertTrue(list2 == List(Even(0), Even(0), Even(2)))
          } @@ ignore
          /**
           * 연습문제
           *
           * `partition`을 사용하여, 주어진 정수 리스트를 짝수와 홀수로 분리하세요.
           */
          test("partition") {
            val isEven = (i: Int) => i % 2 == 0

            val _ = isEven

            val list = List(0, 3, 0, 2, 1)

            val (even, odd) = list.partition(_ => ???)

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

            val _ = list1
            val _ = list2

            def summedList1: Option[Int] = ???
            def summedList2: Option[Int] = ???

            assertTrue(summedList1 == None && summedList2 == Some(6))
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#find`를 사용하여, 주어진 리스트에서 2보다 큰 첫 번째 숫자를 찾으세요.
           */
          test("find") {
            val list = List(1, 2, 3, 4)

            val _ = list

            def firstGreaterThan2: Option[Int] = ???

            assertTrue(firstGreaterThan2 == Some(3))
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#exists`를 사용하여, 주어진 리스트에 음수가 존재하는지 확인하세요.
           */
          test("exists") {
            val list = List(1, 2, 3, 4, -1)

            val _ = list

            def existsNegative: Boolean = ???

            assertTrue(existsNegative)
          } @@ ignore
          /**
           * 연습문제
           *
           * `List#forall`를 사용하여, 주어진 리스트의 모든 요소가 짝수인지 확인하세요.
           */
          test("forall") {
            val isEven = (i: Int) => i % 2 == 0

            val _ = isEven

            val list = List(0, 2, 6, 8, 12, 10)

            val _ = list

            def forallEven: Boolean = ???

            assertTrue(forallEven)
          } @@ ignore
          suite("folds") {

            /**
             * 연습문제
             *
             * `List#foldLeft`를 사용하여, 주어진 리스트의 합을 구하세요.
             */
            test("sum") {
              def sum(list: List[Int]): Int = ???

              assertTrue(sum(List(1, 2, 3, 4, 5)) == 15)
            } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트의 최대 요소를 구하세요.
               */
              test("max") {
                def max(list: List[Int]): Int = ???

                assertTrue(max(List(1, 7, 3, 2, 4, 5)) == 7)
              } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트의 최소 요소를 구하세요.
               */
              test("min") {
                def min(list: List[Int]): Int = ???

                assertTrue(min(List(1, 7, 3, 2, 0, 4, 5)) == 0)
              } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트를 뒤집으세요.
               */
              test("reverse") {
                def reverse[A](list: List[A]): List[A] = ???

                assertTrue(reverse(List(1, 7, 3)) == List(3, 7, 1))
              } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트를 조건에 맞는 요소와 그렇지 않은 요소로 분리하는 함수를 구현하세요.
               */
              test("partition") {
                def partition[A](list: List[A])(pred: A => Boolean): (List[A], List[A]) = ???

                assertTrue(partition(List(1, 7, 3))(_ < 5) == ((List(1, 3), List(7))))
              } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트에서 `n`개의 요소를 취하는 함수를 구현하세요.
               */
              test("take") {
                def take[A](n: Int, list: List[A]): List[A] = ???

                assertTrue(take(2, List(1, 7, 3)) == List(1, 7))
              } @@ ignore
              /**
               * 연습문제
               *
               * `List#foldLeft`를 사용하여, 주어진 리스트에서 조건에 맞는 요소를 취하는 함수를 구현하세요.
               */
              test("takeWhile") {
                def takeWhile[A](list: List[A])(pred: A => Boolean): List[A] = ???

                assertTrue(takeWhile(List(1, 7, 3))(_ < 5) == List(1))
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
            def sum(values: Seq[Int]): Int =
              values.headOption match {
                case None        => 0
                case Some(value) => value + sum(values.drop(1))
              }

            assertTrue(sum(0 to 10000) > 0)
          } @@ ignore
            /**
             * 연습문제
             *
             * 이 코드의 성능 문제를, 컬렉션 타입만 변경하여 해결해 보세요.
             */
            test("random access") {
              def sumProduct(left: Seq[Int], right: Seq[Int]): Int = {
                val length = left.length.max(right.length)

                (0 to length).foldLeft(0) {
                  case (sum, index) => sum + left(index) * right(index)
                }
              }

              assertTrue(sumProduct(List.fill(1000)(2), List.fill(1000)(2)) > 0)
            } @@ ignore
            /**
             * 연습문제
             *
             * 이 코드의 성능 문제를, 컬렉션 타입만 변경하여 해결해 보세요.
             */
            test("containment") {
              def equivalent(left: Seq[Int], right: Seq[Int]): Boolean =
                left.forall(i => right.contains(i)) &&
                  right.forall(i => left.contains(i))

              assertTrue(equivalent(List.fill(1000)(2), List.fill(1000)(2)))
            } @@ ignore
        }
    }
}

/**
 * 스칼라 컬렉션 라이브러리는 스칼라 표준 라이브러리의 핵심 기능 중 하나로,
 * 다양하고 높은 성능을 가진 불변 데이터 구조를 제공하며,
 * 다수의 유용한 연산자를 포함하고 있습니다.
 *
 * 이 과제에서는 스칼라 컬렉션을 사용하여 그래프 데이터 구조를 구현하는 경험을 얻게 됩니다.
 */
object CollectionsGraduation {

  /**
   * 연습문제
   *
   * 다른 스칼라 컬렉션을 사용하여, 노드가 타입 `V`로 식별되고, 간선이 타입 `E`로 식별되는 그래프의 표현을 선택하세요.
   */
  final case class Graph[E, V]() {

    /**
     * 연습문제
     *
     * 주어진 노드에 연결된 간선을 검색하는 함수를 구현하세요.
     */
    def edgesOf(v: V): Set[E] = ???

    /**
     * 연습문제
     *
     * 두 노드를 지정된 간선으로 연결하는 함수를 구현하세요.
     */
    def connect(v1: V, e: E, v2: V): Graph[E, V] = ???

    /**
     * 연습문제
     *
     * 지정된 간선으로 두 노드를 연결 해제하는 함수를 구현하세요.
     */
    def disconnect(v1: V, e: E, v2: V): Graph[E, V] = ???

    /**
     * 연습문제
     *
     * 그래프에 포함된 모든 노드의 집합을 반환하는 함수를 구현하세요.
     */
    def nodes: Set[V] = ???

    /**
     * 연습문제
     *
     * 지정된 노드를 삭제하는 함수를 구현하세요.
     */
    def delete(v: V): Graph[E, V] = ???

    /**
     * 연습문제
     *
     * 노드를 순회하면서, 각 노드에 대해 현재 상태 값과 해당 노드에 연결된 간선 집합을 전달하는 함수를 구현하세요.
     */
    def fold[Z](initial: Z)(f: (Z, V, Set[E]) => Z): Z = ???
  }
  object Graph {
    def empty[E, V] = Graph()
  }
}
