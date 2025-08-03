/**
 * **안전한 프로그래밍: Option, Try, Either 첫 만남**
 * 
 * 실제 프로그래밍에서는 항상 "예상치 못한 상황"들이 발생합니다:
 * - 사용자를 찾을 수 없는 경우
 * - 파일이 존재하지 않는 경우  
 * - 네트워크 연결이 실패하는 경우
 * - 숫자로 변환할 수 없는 문자열인 경우
 * 
 * 전통적인 방법들의 문제점:
 * - **null 사용**: NullPointerException 위험, 타입이 거짓말함
 * - **예외 던지기**: 프로그램이 갑자기 터짐, 처리하기 어려움
 * 
 * **Scala의 해결책: 타입으로 "실패 가능성"을 명시하기**
 * - `Option[A]`: 값이 있을 수도, 없을 수도 있음 (Some/None)
 * - `Try[A]`: 계산이 성공할 수도, 실패할 수도 있음 (Success/Failure)  
 * - `Either[E, A]`: 두 가지 결과 중 하나 (Left=실패, Right=성공)
 * 
 * 이 모듈에서는 이 세 타입이 왜 필요한지, 언제 사용하는지 간단한 예시로 배워봅시다.
 */
package net.degoes

import zio.test._
import zio.test.TestAspect._
import scala.util.{Try, Success, Failure}

object Safety extends ZIOSpecDefault {
  def spec =
    suite("Safety") {
      suite("Option basics") {
        
        /**
         * EXERCISE 1
         *  
         * Complete the findUser function using Map.get which returns Option[User].
         */
        test("finding users") {
          case class User(name: String, age: Int)
          
          val users = Map(
            1 -> User("Alice", 25),
            2 -> User("Bob", 30),
            3 -> User("Charlie", 28)
          )
          
          def findUser(id: Int): Option[User] = ???
          
          val user1 = findUser(1)
          assertTrue(user1 == Some(User("Alice", 25)))
          
          val user999 = findUser(999)
          assertTrue(user999 == None)
        } @@ ignore +
        
        /**
         * EXERCISE 2
         *
         * Use getOrElse and pattern matching to safely handle Option values.
         */
        test("using Option values") {
          def findUserName(id: Int): Option[String] = {
            val users = Map(1 -> "Alice", 2 -> "Bob")
            users.get(id)
          }
          
          val name1 = findUserName(1).getOrElse(???)
          assertTrue(name1 == "Alice")
          
          val name999 = findUserName(999).getOrElse(???)
          assertTrue(name999 == "Unknown")
          
          def greetUser(id: Int): String = findUserName(id) match {
            case ??? => ???
            case ??? => ???
          }
          
          assertTrue(greetUser(1) == "Hello, Alice!")
          assertTrue(greetUser(999) == "User not found")
        } @@ ignore +
        
        /**
         * EXERCISE 3
         *
         * Transform Option values using map.
         */
        test("transforming Options") {
          def findAge(id: Int): Option[Int] = {
            val ages = Map(1 -> 25, 2 -> 30)
            ages.get(id)
          }
          
          val ageNextYear = findAge(1).map(???)
          assertTrue(ageNextYear == Some(26))
          
          val ageNextYear999 = findAge(999).map(_ + 1)
          assertTrue(ageNextYear999 == None)
        } @@ ignore
        
      } +
      suite("Try basics") {
        
        /**
         * EXERCISE 4
         *
         * Create a safe string to integer conversion using Try.
         */
        test("safe string conversion") {
          def safeStringToInt(s: String): Try[Int] = ???
          
          val result1 = safeStringToInt("123")
          assertTrue(result1 == Success(123))
          
          val result2 = safeStringToInt("abc")
          assertTrue(result2.isFailure)
        } @@ ignore +
        
        /**
         * EXERCISE 5
         *
         * Use Try for safe division and handle results with getOrElse.
         */
        test("Try operations") {
          def divide(a: Int, b: Int): Try[Int] = ???
          
          val result1 = divide(10, 2)
          assertTrue(result1 == Success(5))
          
          val result2 = divide(10, 0)
          assertTrue(result2.isFailure)
          
          val value1 = divide(10, 2).getOrElse(???)
          assertTrue(value1 == 5)
          
          val value2 = divide(10, 0).getOrElse(???)
          assertTrue(value2 == -1)
        } @@ ignore
        
      } +
      suite("Either basics") {
        
        /**
         * EXERCISE 6
         *
         * Complete the user registration function using Either for specific error types.
         */
        test("specific error information") {
          sealed trait RegistrationError
          case object EmailAlreadyExists extends RegistrationError
          case object WeakPassword extends RegistrationError
          case object InvalidEmail extends RegistrationError
          
          case class User(email: String, password: String)
          
          def registerUser(email: String, password: String): Either[RegistrationError, User] = {
            if (!email.contains("@")) ???
            else if (password.length < 6) ???
            else if (email == "existing@test.com") ???
            else ???
          }
          
          val success = registerUser("new@test.com", "strongpass")
          assertTrue(success == Right(User("new@test.com", "strongpass")))
          
          val invalidEmail = registerUser("invalid-email", "strongpass")
          assertTrue(invalidEmail == Left(InvalidEmail))
          
          val weakPassword = registerUser("test@test.com", "123")
          assertTrue(weakPassword == Left(WeakPassword))
        } @@ ignore
        
      } +
      suite("choosing the right type") {
        
        /**
         * EXERCISE 7
         *
         * Complete functions using the appropriate safe type (Option, Try, Either).
         */
        test("type selection guide") {
          def findInList[A](list: List[A], predicate: A => Boolean): Option[A] = ???
          
          def parseNumber(s: String): Try[Double] = ???
          
          sealed trait ValidationError
          case object TooShort extends ValidationError
          case object TooLong extends ValidationError
          case object ContainsInvalidChars extends ValidationError
          
          def validateUsername(username: String): Either[ValidationError, String] = {
            if (username.length < 3) Left(TooShort)
            else if (username.length > 20) Left(TooLong)
            else if (!username.matches("[a-zA-Z0-9_]+")) Left(ContainsInvalidChars)
            else Right(username)
          }
          
          assertTrue(findInList(List(1, 2, 3), _ > 2) == Some(3))
          assertTrue(parseNumber("3.14").map(_.round) == Success(3))
          assertTrue(validateUsername("ab") == Left(TooShort))
        } @@ ignore
        
      }
    }
}