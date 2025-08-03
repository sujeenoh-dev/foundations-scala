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

object SafetyAnswer extends ZIOSpecDefault {
  def spec =
    suite("Safety") {
      suite("Option 기초") {
        
        /**
         * 연습문제-01: Option이 왜 필요한가?
         * 
         * 사용자 데이터베이스에서 사용자를 찾는 상황을 생각해봅시다.
         * 사용자가 존재할 수도, 존재하지 않을 수도 있습니다.
         */
        test("사용자 찾기") {
          case class User(name: String, age: Int)
          
          // 간단한 사용자 데이터베이스 (실제로는 복잡한 시스템)
          val users = Map(
            1 -> User("홍길동", 25),
            2 -> User("김철수", 30),
            3 -> User("이영희", 28)
          )
          
          // Option을 사용한 안전한 사용자 조회
          def findUser(id: Int): Option[User] = users.get(id)
          
          // 존재하는 사용자
          val user1 = findUser(1)
          assertTrue(user1 == Some(User("홍길동", 25)))
          
          // 존재하지 않는 사용자
          val user999 = findUser(999)
          assertTrue(user999 == None)
          
          // Option을 사용하면 "값이 없을 수 있다"는 것이 타입에 명시됨!
        } @@ ignore +
        
        /**
         * 연습문제-02: Option 값 사용하기
         * 
         * Option 값을 안전하게 사용하는 기본적인 방법들을 배워봅시다.
         */
        test("Option 값 사용하기") {
          def findUserName(id: Int): Option[String] = {
            val users = Map(1 -> "홍길동", 2 -> "김철수")
            users.get(id)
          }
          
          // 방법 1: getOrElse - 값이 없으면 기본값 사용
          val name1 = findUserName(1).getOrElse("알 수 없음")
          assertTrue(name1 == "홍길동")
          
          val name999 = findUserName(999).getOrElse("알 수 없음")
          assertTrue(name999 == "알 수 없음")
          
          // 방법 2: 패턴 매칭
          def greetUser(id: Int): String = findUserName(id) match {
            case Some(name) => s"안녕하세요, ${name}님!"
            case None => "사용자를 찾을 수 없습니다."
          }
          
          assertTrue(greetUser(1) == "안녕하세요, 홍길동님!")
          assertTrue(greetUser(999) == "사용자를 찾을 수 없습니다.")
        } @@ ignore +
        
        /**
         * 연습문제-03: Option 변환하기
         * 
         * Option 안의 값을 변환하는 방법을 배워봅시다.
         */
        test("Option 변환하기") {
          def findAge(id: Int): Option[Int] = {
            val ages = Map(1 -> 25, 2 -> 30)
            ages.get(id)
          }
          
          // map: Option 안의 값을 변환
          val ageNextYear = findAge(1).map(_ + 1)
          assertTrue(ageNextYear == Some(26))
          
          val ageNextYear999 = findAge(999).map(_ + 1)
          assertTrue(ageNextYear999 == None)  // None은 그대로 None
          
          // Option의 특별한 점: 값이 없으면 자동으로 "아무것도 하지 않음"
        } @@ ignore
        
      } +
      suite("Try 기초") {
        
        /**
         * 연습문제-04: Try가 왜 필요한가?
         * 
         * 문자열을 숫자로 변환하는 상황을 생각해봅시다.
         * 변환이 성공할 수도, 실패할 수도 있습니다.
         */
        test("문자열을 숫자로 변환") {
          // 안전한 문자열 -> 숫자 변환
          def safeStringToInt(s: String): Try[Int] = Try(s.toInt)
          
          // 성공하는 경우
          val result1 = safeStringToInt("123")
          assertTrue(result1 == Success(123))
          
          // 실패하는 경우
          val result2 = safeStringToInt("abc")
          assertTrue(result2.isFailure)  // Failure(NumberFormatException)
          
          // Try를 사용하면 "실패할 수 있는 계산"이 타입에 명시됨!
        } @@ ignore +
        
        /**
         * 연습문제-05: Try 값 사용하기
         */
        test("Try 값 사용하기") {
          def divide(a: Int, b: Int): Try[Int] = Try(a / b)
          
          // 성공하는 계산
          val result1 = divide(10, 2)
          assertTrue(result1 == Success(5))
          
          // 실패하는 계산 (0으로 나누기)
          val result2 = divide(10, 0)
          assertTrue(result2.isFailure)
          
          // getOrElse로 안전하게 값 추출
          val value1 = divide(10, 2).getOrElse(-1)
          assertTrue(value1 == 5)
          
          val value2 = divide(10, 0).getOrElse(-1)
          assertTrue(value2 == -1)  // 실패시 기본값
        } @@ ignore
        
      } +
      suite("Either 기초") {
        
        /**
         * 연습문제-06: Either가 왜 필요한가?
         * 
         * 때로는 "어떤 종류의 실패인지" 구체적인 정보가 필요합니다.
         * Either는 성공(Right)과 실패(Left)를 모두 값으로 표현합니다.
         */
        test("구체적인 오류 정보와 함께") {
          // 사용자 등록시 발생할 수 있는 오류들
          sealed trait RegistrationError
          case object EmailAlreadyExists extends RegistrationError
          case object WeakPassword extends RegistrationError
          case object InvalidEmail extends RegistrationError
          
          case class User(email: String, password: String)
          
          def registerUser(email: String, password: String): Either[RegistrationError, User] = {
            if (!email.contains("@")) Left(InvalidEmail)
            else if (password.length < 6) Left(WeakPassword)  
            else if (email == "existing@test.com") Left(EmailAlreadyExists)
            else Right(User(email, password))
          }
          
          // 성공하는 경우
          val success = registerUser("new@test.com", "strongpass")
          assertTrue(success == Right(User("new@test.com", "strongpass")))
          
          // 다양한 실패 케이스들
          val invalidEmail = registerUser("invalid-email", "strongpass")
          assertTrue(invalidEmail == Left(InvalidEmail))
          
          val weakPassword = registerUser("test@test.com", "123")
          assertTrue(weakPassword == Left(WeakPassword))
          
          val existingEmail = registerUser("existing@test.com", "strongpass")
          assertTrue(existingEmail == Left(EmailAlreadyExists))
          
          // Either를 사용하면 "어떤 실패인지" 구체적으로 알 수 있음!
        } @@ ignore
        
      } +
      suite("언제 무엇을 사용할까?") {
        
        /**
         * 연습문제-07: 상황별 적절한 타입 선택
         * 
         * 각 상황에 맞는 적절한 타입을 선택하는 연습을 해봅시다.
         */
        test("상황별 타입 선택 가이드") {
          // Option: 값이 있을 수도, 없을 수도 있는 경우 (실패 이유가 중요하지 않음)
          def findInList[A](list: List[A], predicate: A => Boolean): Option[A] = 
            list.find(predicate)
          
          // Try: 예외가 발생할 수 있는 계산 (구체적인 예외 정보 필요없음)  
          def parseNumber(s: String): Try[Double] = Try(s.toDouble)
          
          // Either: 구체적인 오류 정보가 필요한 경우
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
          
          // 사용 예시
          assertTrue(findInList(List(1, 2, 3), _ > 2) == Some(3))
          assertTrue(parseNumber("3.14").map(_.round) == Success(3))
          assertTrue(validateUsername("ab") == Left(TooShort))
          
          /*
           * 선택 기준:
           * - Option: "있다/없다"만 중요한 경우
           * - Try: 예외 가능한 연산, 예외 종류는 중요하지 않은 경우  
           * - Either: 실패 이유가 중요하고, 이를 기반으로 다른 처리를 해야 하는 경우
           */
        } @@ ignore
        
      }
    }
}