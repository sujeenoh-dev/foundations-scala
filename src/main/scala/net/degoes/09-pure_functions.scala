/**
 * **ZPure: 순수 함수형 프로그래밍의 완성**
 * 
 * 지금까지 배운 모든 개념들(함수, 데이터 모델링, Option/Try/Either, 컬렉션, 재귀, 순수성)이
 * ZPure라는 강력한 타입에서 하나로 합쳐집니다.
 * 
 * **ZPure란 무엇인가?**
 * ZPure[W, S1, S2, R, E, A]는 다음을 나타내는 데이터 타입입니다:
 * - W: 로그나 출력 같은 부작용(Write effects)
 * - S1: 초기 상태(Initial state)  
 * - S2: 최종 상태(Final state)
 * - R: 환경이나 의존성(Requirements/Environment)
 * - E: 에러 타입(Error type)
 * - A: 성공시 결과 타입(Success value type)
 * 
 * **왜 ZPure가 필요한가?**
 * 실제 프로그램에서는 여러 복잡한 요구사항이 동시에 있습니다:
 * - 상태를 변경해야 함 (데이터베이스 업데이트, 카운터 증가 등)
 * - 실패할 수 있음 (네트워크 오류, 검증 실패 등)
 * - 환경에 의존함 (설정, 데이터베이스 연결 등)  
 * - 로그를 남겨야 함 (디버깅, 모니터링 등)
 * 
 * ZPure는 이 모든 것을 타입 안전하고 조합 가능한 방식으로 다룰 수 있게 해줍니다.
 * 
 * **ZPure의 핵심 특징:**
 * 1. **순수함**: 모든 부작용이 타입에 명시됨
 * 2. **조합 가능**: 작은 ZPure들을 큰 ZPure로 조합 가능
 * 3. **타입 안전**: 컴파일 타임에 모든 부작용을 추적
 * 4. **테스트 가능**: 모든 것이 순수하므로 쉽게 테스트 가능
 * 
 * **실무에서 ZPure 사용 사례:**
 * - 비즈니스 로직 모델링 (상태 + 에러 + 로깅)
 * - 데이터 검증 파이프라인 (여러 검증 단계 조합)
 * - 상태 머신 구현 (상태 전이 + 에러 처리)
 * - 함수형 웹 애플리케이션 (요청 처리 + 응답 생성)
 * 
 * 이 모듈에서는 ZPure의 기본 사용법부터 traverse 같은 고급 패턴까지 배워봅시다.
 */
package net.degoes

import zio._
import zio.prelude._
import zio.test._
import zio.test.TestAspect._

object PureFunctions extends ZIOSpecDefault {
  def spec = 
    suite("Pure Functions") {
      
      suite("ZPure 기초") {
        
        /**
         * EXERCISE 1
         *
         * Create a simple ZPure computation that succeeds with a value.
         * ZPure.succeed creates a computation that always succeeds.
         */
        test("succeed") {
          val computation: ZPure[Nothing, Unit, Unit, Any, Nothing, Int] = ???
          
          assertTrue(computation.runAll(()) == ((), (), 42))
        } @@ ignore +
        
        /**
         * EXERCISE 2
         *
         * Create a ZPure computation that fails with an error.
         * ZPure.fail creates a computation that always fails.
         */
        test("fail") {
          val computation: ZPure[Nothing, Unit, Unit, Any, String, Int] = ???
          
          assertTrue(computation.runAll(()).isLeft)
        } @@ ignore +
        
        /**
         * EXERCISE 3
         *
         * Transform the value inside a ZPure using map.
         * This is similar to Option.map or List.map.
         */
        test("map") {
          val original = ZPure.succeed(10)
          val doubled: ZPure[Nothing, Unit, Unit, Any, Nothing, Int] = ???
          
          assertTrue(doubled.runAll(()) == ((), (), 20))
        } @@ ignore +
        
        /**
         * EXERCISE 4
         *
         * Chain ZPure computations using flatMap.
         * This allows you to use the result of one computation in the next.
         */
        test("flatMap") {
          def divide(a: Int, b: Int): ZPure[Nothing, Unit, Unit, Any, String, Int] =
            if (b == 0) ZPure.fail("Division by zero")
            else ZPure.succeed(a / b)
          
          val computation: ZPure[Nothing, Unit, Unit, Any, String, Int] = 
            ZPure.succeed(20).flatMap(a => ???)
          
          assertTrue(computation.runAll(()) == Right(((), (), 4)))
        } @@ ignore
        
      } +
      
      suite("상태 관리") {
        
        /**
         * EXERCISE 5
         *
         * Use ZPure.get to access the current state.
         */
        test("get state") {
          val computation: ZPure[Nothing, Int, Int, Any, Nothing, Int] = ???
          
          assertTrue(computation.runAll(42) == ((), 42, 42))
        } @@ ignore +
        
        /**
         * EXERCISE 6
         *
         * Use ZPure.set to update the state.
         */
        test("set state") {
          val computation: ZPure[Nothing, Int, Int, Any, Nothing, Unit] = ???
          
          assertTrue(computation.runAll(10) == ((), 100, ()))
        } @@ ignore +
        
        /**
         * EXERCISE 7
         *
         * Use ZPure.modify to both read and update state.
         */
        test("modify state") {
          val computation: ZPure[Nothing, Int, Int, Any, Nothing, Int] = ???
          
          val (_, finalState, result) = computation.runAll(5)
          assertTrue(finalState == 10 && result == 5)
        } @@ ignore
        
      } +
      
      suite("환경과 의존성") {
        
        /**
         * EXERCISE 8
         *
         * Use ZPure.access to read from the environment.
         */
        test("access environment") {
          final case class Config(multiplier: Int)
          
          val computation: ZPure[Nothing, Unit, Unit, Config, Nothing, Int] = ???
          
          val config = Config(3)
          assertTrue(computation.provideEnvironment(config).runAll(()) == ((), (), 15))
        } @@ ignore +
        
        /**
         * EXERCISE 9
         *
         * Combine state and environment in a single computation.
         */
        test("state and environment") {
          final case class Config(increment: Int)
          
          val computation: ZPure[Nothing, Int, Int, Config, Nothing, Int] = ???
          
          val config = Config(5)
          val (_, finalState, result) = computation.provideEnvironment(config).runAll(10)
          assertTrue(finalState == 15 && result == 10)
        } @@ ignore
        
      } +
      
      suite("로깅과 출력") {
        
        /**
         * EXERCISE 10
         *
         * Use ZPure.log to write log messages.
         */
        test("logging") {
          val computation: ZPure[String, Unit, Unit, Any, Nothing, Int] = ???
          
          val (logs, _, result) = computation.runAll(())
          assertTrue(logs == "Computing result" && result == 42)
        } @@ ignore +
        
        /**
         * EXERCISE 11
         *
         * Combine multiple log messages in a computation.
         */
        test("multiple logs") {
          val computation: ZPure[Chunk[String], Unit, Unit, Any, Nothing, Int] = 
            for {
              _ <- ZPure.log(Chunk("Starting"))
              x <- ZPure.succeed(10)
              _ <- ZPure.log(Chunk("Processing"))
              y <- ZPure.succeed(x * 2)
              _ <- ZPure.log(Chunk("Finished"))
            } yield y
          
          val (logs, _, result) = computation.runAll(())
          assertTrue(logs == Chunk("Starting", "Processing", "Finished") && result == 20)
        } @@ ignore
        
      } +
      
      suite("에러 처리") {
        
        /**
         * EXERCISE 12
         *
         * Handle errors using catchAll.
         */
        test("catch errors") {
          def risky(x: Int): ZPure[Nothing, Unit, Unit, Any, String, Int] =
            if (x < 0) ZPure.fail("Negative number")
            else ZPure.succeed(x * 2)
          
          val computation: ZPure[Nothing, Unit, Unit, Any, Nothing, Int] = ???
          
          assertTrue(computation.runAll(()) == ((), (), 0))
        } @@ ignore +
        
        /**
         * EXERCISE 13
         *
         * Use mapError to transform error types.
         */
        test("map errors") {
          val original: ZPure[Nothing, Unit, Unit, Any, String, Int] = ZPure.fail("oops")
          val mapped: ZPure[Nothing, Unit, Unit, Any, Int, Int] = ???
          
          assertTrue(mapped.runAll(()).swap.getOrElse(((), (), 0))._3 == 4)
        } @@ ignore
        
      } +
      
      suite("Traverse 패턴") {
        
        /**
         * EXERCISE 14
         *
         * Use ZPure.foreach to process a list of values.
         * This is like List.map but for ZPure computations.
         */
        test("foreach") {
          def processNumber(n: Int): ZPure[Nothing, Unit, Unit, Any, Nothing, Int] = 
            ZPure.succeed(n * 2)
          
          val numbers = List(1, 2, 3, 4, 5)
          val computation: ZPure[Nothing, Unit, Unit, Any, Nothing, List[Int]] = ???
          
          assertTrue(computation.runAll(()) == ((), (), List(2, 4, 6, 8, 10)))
        } @@ ignore +
        
        /**
         * EXERCISE 15
         *
         * Use traverse with stateful computations.
         * Process each number while maintaining a running total.
         */
        test("stateful traverse") {
          def addToTotal(n: Int): ZPure[Nothing, Int, Int, Any, Nothing, Int] = 
            ZPure.modify((total: Int) => (total + n, total))
          
          val numbers = List(1, 2, 3, 4, 5)
          val computation: ZPure[Nothing, Int, Int, Any, Nothing, List[Int]] = ???
          
          val (_, finalState, results) = computation.runAll(0)
          assertTrue(finalState == 15 && results == List(0, 1, 3, 6, 10))
        } @@ ignore +
        
        /**
         * EXERCISE 16
         *
         * Use traverse with error handling.
         * Process numbers but fail if any number is negative.
         */
        test("traverse with errors") {
          def validatePositive(n: Int): ZPure[Nothing, Unit, Unit, Any, String, Int] =
            if (n < 0) ZPure.fail(s"Negative number: $n")
            else ZPure.succeed(n * n)
          
          val validNumbers = List(1, 2, 3)
          val invalidNumbers = List(1, -2, 3)
          
          val validComputation: ZPure[Nothing, Unit, Unit, Any, String, List[Int]] = ???
          val invalidComputation: ZPure[Nothing, Unit, Unit, Any, String, List[Int]] = ???
          
          assertTrue(validComputation.runAll(()) == Right(((), (), List(1, 4, 9))))
          assertTrue(invalidComputation.runAll(()).isLeft)
        } @@ ignore
        
      } +
      
      suite("실제 사용 사례") {
        
        /**
         * EXERCISE 17
         *
         * Build a validation pipeline using ZPure.
         * Validate user registration data with multiple checks.
         */
        test("validation pipeline") {
          final case class User(name: String, email: String, age: Int)
          final case class ValidationError(field: String, message: String)
          
          def validateName(name: String): ZPure[Nothing, Unit, Unit, Any, ValidationError, String] =
            if (name.length >= 2) ZPure.succeed(name)
            else ZPure.fail(ValidationError("name", "Name must be at least 2 characters"))
          
          def validateEmail(email: String): ZPure[Nothing, Unit, Unit, Any, ValidationError, String] =
            if (email.contains("@")) ZPure.succeed(email)
            else ZPure.fail(ValidationError("email", "Invalid email format"))
          
          def validateAge(age: Int): ZPure[Nothing, Unit, Unit, Any, ValidationError, Int] =
            if (age >= 18) ZPure.succeed(age)
            else ZPure.fail(ValidationError("age", "Must be at least 18 years old"))
          
          def validateUser(name: String, email: String, age: Int): ZPure[Nothing, Unit, Unit, Any, ValidationError, User] = ???
          
          val validUser = validateUser("John", "john@example.com", 25)
          val invalidUser = validateUser("A", "invalid-email", 15)
          
          assertTrue(validUser.runAll(()).isRight)
          assertTrue(invalidUser.runAll(()).isLeft)
        } @@ ignore +
        
        /**
         * EXERCISE 18
         *
         * Create a simple state machine using ZPure.
         * Model a traffic light that cycles through states.
         */
        test("state machine") {
          sealed trait TrafficLight
          case object Red extends TrafficLight  
          case object Yellow extends TrafficLight
          case object Green extends TrafficLight
          
          def nextLight: ZPure[Nothing, TrafficLight, TrafficLight, Any, Nothing, TrafficLight] = ???
          
          val cycle = for {
            light1 <- nextLight  // Red -> Green
            light2 <- nextLight  // Green -> Yellow  
            light3 <- nextLight  // Yellow -> Red
          } yield (light1, light2, light3)
          
          val (_, finalState, (l1, l2, l3)) = cycle.runAll(Red)
          assertTrue(l1 == Green && l2 == Yellow && l3 == Red && finalState == Red)
        } @@ ignore
        
      }
    }
}