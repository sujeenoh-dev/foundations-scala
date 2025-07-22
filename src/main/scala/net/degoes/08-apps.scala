package net.degoes

import zio._

import java.time.LocalDate

final case class Plane()

final case class Flight()

final case class Airport()

final case class Passenger()

final case class Booking()

final case class SearchError()

final case class BookingError()

/**
 * 연습문제
 *
 * 항공편 검색과 항공편 예약, 확인을 위한 데이터 타입으로
 * API를 생성하세요.
 */
trait FlightAPI {
  def findFlights(
    origin: Airport,
    dest: Airport,
    departure: LocalDate,
    returnDate: Option[LocalDate]
  ): IO[SearchError, List[Flight]]

  def bookFlight(passenger: Passenger, flight: Flight): IO[BookingError, Booking]
}

final case class EmailFailure()

final case class Email()

/**
 * 연습문제
 *
 * 이메일 발송을 위한 데이터 타입으로 API를 생성하세요.
 */
trait EmailAPI {
  def sendEmail(email: Email): IO[EmailFailure, Unit]
}

/**
 * 연습문제
 *
 * 사용자가 항공편을 검색하고 예약할 수 있는 명령줄 메뉴 기반
 * 애플리케이션을 생성하세요.
 */
object AppsGraduation extends ZIOAppDefault {
  def run =
    Console.printLine("The End!")
}
