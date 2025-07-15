/**
 * # 람다란 무엇인가?
 * 람다 혹은 람다 표현식은 익명 함수를 가리키는 용어입니다. 즉, 이름 없이 즉석에서 정의하는 함수를 뜻합니다.
 * 스칼라에서는 아래와 같이 생성할 수 있습니다.
 * ```scala
 * val square: Int => Int = (x: Int) => x * x
 * ```
 * 람다는 함수 타입을 가지며, 위 예시에서는 `Int => Int` 타입을 가집니다.
 * 
 * 함수는 함수형 프로그래밍의 근간이 됩니다. 함수형 프로그래밍 언어에서 함수는 값으로 취급되며, 필드에 저장되거나 다른 함수의 인자로 전달되거나, 함수에서 반환될 수 있습니다. 
 * 이렇게 이른바 일급 함수(first-class functions)는 언어마다 “클로저(closures)”, “람다(lambdas)”, “익명 함수(anonymous functions)” 등 여러 이름으로 불립니다.
 * 함수형 프로그래밍을 제대로 이해하고 활용하기 위해서는, 특히 스칼라를 비롯한 어떤 프로그래밍 언어를 다룸에 있어서도, 함수를 올바르게 이해하고 사용할 수 있어야 합니다.
 * 이 모듈에서는 “값으로서의 함수” 개념을 배우고, 함수를 직접 만들고, 변환하고, 합성하며, 타입을 지정하는 법을 익히게 될 것입니다.
 */
package net.degoes.lambdas

import zio._
import zio.test._
import zio.test.TestAspect._

object Lambdas extends ZIOSpecDefault {
  def assertTypeEquals[A, B](implicit ev: A <:< B) = assertCompletes

  def spec =
    suite("Lambdas") {
      suite("values") {

        /**
         * EXERCISE
         *
         * 인자를 제곱하는 람다를 생성하고, 이를 `square` 변수에 저장하세요.
         */
        test("square") {
          val square: Int => Int = ??? // TODO

          assertTrue(square(3) == 9)
        } @@ ignore +
          test("plus") {

            /**
             * EXERCISE
             *
             * 두 인자를 더하는 람다를 생성하고, 이를 `plus` 변수에 저장하세요.
             */
            val plus: (Int, Int) => Int = ??? // TODO

            assertTrue(plus(2, 2) == 4)
          } @@ ignore +
          /**
           * EXERCISE
           * 
           * 스칼라에서 `_`는 와일드카드로 쓰입니다. 문맥에 따라 다양한 의미로 사용됩니다.
           * `.map(_.a)`는 `map(x => x.a)`와 동일합니다.
           * `_`를 사용하여 인자에 1을 더하는 람다를 생성하고, 이를 `addTwo` 변수에 저장하세요.
           */
          test("underscore") {
            val addTwo: Int => Int = ??? // TODO

            assertTrue(addTwo(2) == 4)
          } @@ ignore +
          /**
           * EXERCISE
           * 
           * 합성 함수란, 하나의 함수 실행 결과를 다음 함수 입력으로 넘겨 연결해 주는 함수입니다.
           * 스칼라에서는 `Function#andThen`을 사용하여 합성 함수를 생성할 수 있습니다.
           * `Function#andThen`을 사용하여 두 함수를 합성하여, 정수 내부의 숫자 개수를 세는 합성 함수를 생성하세요.
           * f andThen g === g(f(x)) 와 동일합니다.
           */
          test("andThen") {
            val convertToString: Int => String = _.toString
            val countLength: String => Int     = _.length

            val _ = (convertToString, countLength)

            val numberOfDigits: Int => Int = ??? // TODO

            assertTrue(numberOfDigits(123) == 3)
          } @@ ignore +
          /**
           * EXERCISE
           *
           * 합성 함수를 만드는 다른 함수로는 `Function#compose`가 있습니다.
           * `Function#compose`와 `Function#andThen`의 차이점은 순서입니다.
           * f compose g === f(g(x)) 와 동일합니다.
           */
          test("compose") {
            val convertToString: Int => String = _.toString
            val countLength: String => Int     = _.length

            val _ = (convertToString, countLength)

            val numberOfDigits: Int => Int = ??? // TODO

            assertTrue(numberOfDigits(123) == 3)
          } @@ ignore +
          /**
           * EXERCISE
           *
           * `identity`는 자기 자신을 반환하는 함수를 생성합니다.
           * `identity`를 사용하여 자기 자신을 반환하는 함수를 생성하세요.
           */
          test("identity") {
            val sameString: String => String = ???

            assertTrue(sameString("foobar") == "foobar" && sameString("barfoo") == "barfoo")
          } @@ ignore +
          /**
           * EXERCISE
           *
           * `Function.const`는 항상 인자를 무시하고 주어진 입력을 반환합니다.
           * `Function.const`를 사용하여 항상 42를 반환하는 함수를 생성하세요.
           */
          test("const") {
            val answer: String => Int = ??? // TODO

            assertTrue(answer("foo") == answer("bar") && answer("foobar") == 42)
          } @@ ignore +
          /**
           * EXERCISE
           *
           * 주어진 문자열 앞에 입력된 수만큼의 공백을 추가하는 함수를 생성하세요.
           */
          test("prependSpace") {
            val prependSpace: Int => (String => String) = ??? // TODO

            assertTrue(prependSpace(5)("foo") == "     foo")
          } @@ ignore +
          /**
           * EXERCISE
           *
           * 주어진 문자열에 입력된 수만큼 문자열을 반복하여 반환하는 함수를 생성하세요.
           */
          test("repeat") {
            val repeat: Int => (String => String) => (String => String) = ??? // TODO

            assertTrue(repeat(5)(_ + ".")("Coming soon") == "Coming soon.....")
          } @@ ignore
      } +
        suite("types") {

          /**
           * EXERCISE
           *
           * f의 결괏값에 해당하는 타입을 정의하세요.
           */
          test("example 1") {
            val f = (x: Int) => x * x

            type Type = f.type // TODO: EDIT HERE

            assertTypeEquals[f.type, Type]
          } @@ ignore +
            /**
             * EXERCISE
             *
             * f의 결괏값에 해당하는 타입을 정의하세요.
             */
            test("example 2") {
              val f = (x: Int, y: Int) => x + y

              type Type = f.type // TODO: EDIT HERE

              assertTypeEquals[f.type, Type]
            } @@ ignore +
            /**
             * EXERCISE
             *
             * f의 결괏값에 해당하는 타입을 정의하세요.
             */
            test("example 3") {
              val f = (t: (Int, Int)) => t._1 + t._2

              type Type = f.type // TODO: EDIT HERE

              assertTypeEquals[f.type, Type]
            } @@ ignore +
            /**
             * EXERCISE
             *
             * f의 결괏값에 해당하는 타입을 정의하세요.
             */
            test("example 4") {
              val f = (x: Int) => (y: Int) => x + y

              type Type = f.type // TODO: EDIT HERE

              assertTypeEquals[f.type, Type]
            } @@ ignore +
            /**
             * EXERCISE
             *
             * f의 결괏값에 해당하는 타입을 정의하세요.
             */
            test("example 5") {
              val f = (x: Int) => (g: Int => Int) => g(x)

              type Type = f.type // TODO: EDIT HERE

              assertTypeEquals[f.type, Type]
            } @@ ignore
        } +
        suite("partiality") {
          /**
           * `PartialFunction`는 부분 함수를 정의하는 타입입니다.
           * 부분 함수란, 정의된 입력 값들에만 동작하는 함수입니다.
           *  주어진 인자에 대해 함수가 정의되어 있는지 검사할 수 있는 `isDefinedAt` 메서드를 가집니다.
           * ```scala
           * val pf: PartialFunction[Int] = {
           *   case x if x > 0 => x
           * }
           * ```
           * 위 예시에서 `pf`는 양수일 때만 정의됩니다. 따라서 `pf.isDefinedAt(0)`은 `false`를 반환합니다.
           */

          /**
           * EXERCISE
           *
           * 튜플의 두 번째 요소가 0이 아닐 때만 정의되는 부분 함수 `divide`를 정의하세요.
           */
          test("divide") {
            val divide: PartialFunction[(Int, Int), Int] =
              ??? // TODO

            assertTrue(!divide.isDefinedAt((42, 0)))
          } @@ ignore +
            /**
             * EXERCISE
             *
             * `PartialFunction#lift` 메서드를 사용하여 `oddOption` 메서드를 정의하세요.
             */
            test("lift") {
              val odd: PartialFunction[Int, Int] = {
                case x if x % 2 == 1 => x
              }

              val _ = odd

              def divideOption: ((Int, Int)) => Option[Int] = ??? // TODO

              assertTrue(divideOption((42, 0)) == None)
            } @@ ignore
        }
    }
}

/**
 * 파서는 람다가 가진 강력함을 잘 보여주는 예시입니다.
 * 파서는 본질적으로 람다 그 자체로 볼 수 있습니다.
 * 함수는 파서를 생성하거나 결합할 수 있어, 어떤 형태의 데이터든 파싱하는 방법을 구성적으로 정의할 수 있게 해줍니다.
 *
 * 이 마지막 프로젝트에서 여러분은 람다를 직접 만들어보는 경험을 하게 될 것입니다.
 * 여기에는 람다를 인자로 받고, 또 람다를 반환하는 고차 람다를 구성하는 것도 포함됩니다.
 */
object LambdasGraduation extends ZIOAppDefault {
  // 각 String, (String, A)의 의미를 생각해 보세요.
  type Parser[+A] = String => Either[String, (String, A)]

  object Parser {

    /**
     * EXERCISE
     *
     * 주어진 값으로 성공하는 파서를 생성하세요. 입력 이외의 다른 값을 소모하지 않습니다.
     */
    def succeed[A](a: => A): Parser[A] = ??? // TODO

    /**
     * EXERCISE
     *
     * 주어진 메시지로 실패하는 파서를 생성하세요. 입력 이외의 다른 값을 소모하지 않습니다.
     */
    def fail(message: => String): Parser[Nothing] = ??? // TODO

    /**
     * EXERCISE
     *
     * 입력에서 한 글자를 읽는 파서를 생성하세요. 입력이 없으면 실패합니다.
     */
    def anyChar: Parser[Char] = ??? // TODO

    /**
     * EXERCISE
     *
     * 주어진 문자만 파싱하는 파서를 생성하세요. 입력이 없거나 주어진 문자가 아니면 실패합니다.
     */
    def char(char: Char): Parser[Unit] = ??? // TODO
  }

  implicit class ParserExtensionMethods[A](self: Parser[A]) {

    def map[B](f: A => B): Parser[B] = self.flatMap(a => Parser.succeed(f(a)))

    /**
     * EXERCISE
     * 
     * `Function#flatMap`이란, 상자를 열어 값을 꺼내서 처리하고, 다시 상자에 넣어 주는 것과 같습니다.
     *
     * 이 파서의 출력 값을 콜백 함수에 전달하고,
     * 이 콜백이 새 파서를 반환하면,
     * 그 새 파서가 이 파서가 남긴 입력을 이어서 처리할 수 있도록 하는 함수를 구현하세요.
     */
    def flatMap[B](f: A => Parser[B]): Parser[B] = ??? // TODO

    def ~[B](that: => Parser[B]): Parser[(A, B)] =
      self.flatMap(a => that.map(b => (a, b)))

    def <~(that: => Parser[Any]): Parser[A] = (self ~ that).map(_._1)

    def ~>[B](that: => Parser[B]): Parser[B] = (self ~ that).map(_._2)

    /**
     * EXERCISE
     *
     * 좌측 파서를 사용해 파싱을 시도하되,
     * 만약 실패하면 우측 파서를 사용해 파싱을 시도하는 함수를 구현하시오.
     */
    def |(that: => Parser[A]): Parser[A] = ??? // TODO

    def repeat: Parser[List[A]] =
      (self ~ repeat).map {
        case (head, tail) => head :: tail
      } | Parser.succeed(List.empty[A])

    def optional: Parser[Option[A]] =
      self.map(Option(_)) | Parser.succeed(Option.empty[A])

    def run(input: String): Either[String, A] = self(input).map(_._2)
  }

  def readFile(file: String): Task[String] =
    ZIO.attempt {
      val source = scala.io.Source.fromFile(file)

      try {
        source.getLines().mkString("\n")
      } finally {
        source.close()
      }
    }

  sealed trait CSVData
  object CSVData {
    final case class Header(names: List[String])   extends CSVData
    final case class Values(columns: List[String]) extends CSVData
  }

  /**
   * EXERCISE
   *
   * 파일 내용을 CSV 데이터 요소의 리스트로 파싱하는 함수를 구현하세요.
   */
  def parseFile(contents: String): Either[String, List[CSVData]] = ??? // TODO

  def run =
    for {
      args     <- getArgs
      contents <- readFile(args(0))
      parsed   <- ZIO.from(parseFile(contents)).mapError(e => new RuntimeException(e))
      _        <- Console.printLine(parsed.mkString("\n"))
    } yield ()
}
