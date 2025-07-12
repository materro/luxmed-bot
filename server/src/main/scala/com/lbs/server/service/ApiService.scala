package com.lbs.server.service

import cats.instances.either._
import com.lbs.api.LuxmedApi
import com.lbs.api.http.Session
import com.lbs.api.json.model._
import com.lbs.server.ThrowableOr
import com.lbs.server.util.DateTimeUtil
import org.jasypt.util.text.TextEncryptor
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service
import scalaj.http.HttpResponse
import scala.util.Random

import java.net.HttpCookie
import java.time.{LocalDateTime, LocalTime, LocalDate}

@Service
class ApiService extends SessionSupport {

  @Autowired
  protected var dataService: DataService = _
  @Autowired
  private var textEncryptor: TextEncryptor = _

  private val luxmedApi = new LuxmedApi[ThrowableOr]

  def getAllCities(accountId: Long): ThrowableOr[List[DictionaryCity]] =
    withSession(accountId) { session =>
      luxmedApi.dictionaryCities(session)

    }

  def getAllFacilities(accountId: Long, cityId: Long, serviceVariantId: Long): ThrowableOr[List[IdName]] =
    withSession(accountId) { session =>
      luxmedApi
        .dictionaryFacilitiesAndDoctors(session, cityId = Some(cityId), serviceVariantId = Some(serviceVariantId))
        .map(_.facilities)
    }

  def getAllServices(accountId: Long): ThrowableOr[List[DictionaryServiceVariants]] =
    withSession(accountId) { session =>
      luxmedApi.dictionaryServiceVariants(session).map(s => s.flatMap(_.flatten.filterNot(_.children.nonEmpty)))
    }

  def getAllDoctors(accountId: Long, cityId: Long, serviceVariantId: Long): ThrowableOr[List[Doctor]] =
    withSession(accountId) { session =>
      luxmedApi
        .dictionaryFacilitiesAndDoctors(session, cityId = Some(cityId), serviceVariantId = Some(serviceVariantId))
        .map(_.doctors)
    }

  def getAvailableTerms(
    accountId: Long,
    cityId: Long,
    clinicId: Option[Long],
    serviceId: Long,
    doctorId: Option[Long],
    fromDate: LocalDateTime,
    toDate: LocalDateTime,
    timeFrom: LocalTime,
    timeTo: LocalTime,
    languageId: Long = 10
  ): ThrowableOr[List[TermExt]] = {

    var availableDays = List[LocalDateTime]()

    val termsFromIndex = withSession(accountId) { session =>
      luxmedApi.termsIndex(session, cityId, clinicId, serviceId, doctorId, fromDate, toDate, languageId)
        .map { response =>
          availableDays = availableDays ++ response.termsForService.termsInfoForDays
            .filter(info =>
              info.termsStatus == 0 &&
              !info.day.get.isBefore(fromDate) &&
              !info.day.get.isAfter(toDate)
            )
            .map(info => info.day.get)

          response.termsForService.termsForDays.flatMap(
            _.terms.map(term => TermExt(response.termsForService.additionalData, term))
          )
        }
    }

    termsFromIndex.flatMap { terms =>
      if (terms.nonEmpty) {
        Right(terms)
      } else {
        val oneDayResponses = availableDays.map { date =>
          if (!fromDate.toLocalDate.isEqual(toDate.toLocalDate) && !date.toLocalDate.isEqual(fromDate.toLocalDate)) {
            val randomDelay = Random.nextInt(500) + 500
            Thread.sleep(randomDelay)
          }
          withSession(accountId) { session =>
            luxmedApi.oneDayTerms(session, cityId, serviceId, doctorId, date, languageId)
          }
        }
        val termsFromOneDay = oneDayResponses.flatMap {
          case Right(response) => response.termsForDay.terms.map(
            term => TermExt(AdditionalData(isPreparationRequired = false, List.empty), term)
          )
          case Left(_) => List.empty
        }
        Right(termsFromOneDay)
      }
    }.map { allTerms =>
      allTerms.filter { term =>
        val time = term.term.dateTimeFrom.get.toLocalTime
        val date = term.term.dateTimeFrom.get
        (doctorId.isEmpty || doctorId.contains(term.term.doctor.id)) &&
        (clinicId.isEmpty || clinicId.contains(term.term.clinicGroupId)) &&
        (time == timeFrom || time == timeTo || (time.isAfter(timeFrom) && time.isBefore(timeTo))) &&
        (date.isEqual(fromDate) || date.isEqual(toDate) || (date.isAfter(fromDate) && date.isBefore(toDate)))
      }
    }
  }

  def reservationLockterm(
    accountId: Long,
    xsrfToken: XsrfToken,
    reservationLocktermRequest: ReservationLocktermRequest
  ): ThrowableOr[ReservationLocktermResponse] =
    withSession(accountId) { session =>
      luxmedApi.reservationLockterm(session, xsrfToken, reservationLocktermRequest)
    }

  def deleteTemporaryReservation(
    accountId: Long,
    xsrfToken: XsrfToken,
    temporaryReservationId: Long
  ): ThrowableOr[Unit] =
    withSession(accountId) { session =>
      luxmedApi.deleteTemporaryReservation(session, xsrfToken, temporaryReservationId)
    }

  def reservationConfirm(
    accountId: Long,
    xsrfToken: XsrfToken,
    reservationConfirmRequest: ReservationConfirmRequest
  ): ThrowableOr[ReservationConfirmResponse] =
    withSession(accountId) { session =>
      luxmedApi.reservationConfirm(session, xsrfToken, reservationConfirmRequest)
    }

  def reservationChangeTerm(
    accountId: Long,
    xsrfToken: XsrfToken,
    reservationChangetermRequest: ReservationChangetermRequest
  ): ThrowableOr[ReservationConfirmResponse] =
    withSession(accountId) { session =>
      luxmedApi.reservationChangeTerm(session, xsrfToken, reservationChangetermRequest)
    }

  def history(
    accountId: Long,
    fromDate: LocalDateTime = LocalDateTime.now().minusYears(1),
    toDate: LocalDateTime = LocalDateTime.now()
  ): ThrowableOr[List[Event]] =
    withSession(accountId) { session =>
      luxmedApi
        .events(session, fromDate.atZone(DateTimeUtil.Zone), toDate.atZone(DateTimeUtil.Zone))
        .map(_.events.filter(_.status == "Realized").sortBy(_.date).reverse)
    }

  def reserved(
    accountId: Long,
    fromDate: LocalDateTime = LocalDateTime.now(),
    toDate: LocalDateTime = LocalDateTime.now().plusMonths(3)
  ): ThrowableOr[List[Event]] =
    withSession(accountId) { session =>
      luxmedApi
        .events(session, fromDate.atZone(DateTimeUtil.Zone), toDate.atZone(DateTimeUtil.Zone))
        .map(_.events.filter(event => event.status == "Reserved" || event.status == "Confirmed").sortBy(_.date))
    }

  def deleteReservation(accountId: Long, reservationId: Long): ThrowableOr[HttpResponse[String]] =
    withSession(accountId) { session =>
      luxmedApi.reservationDelete(session, reservationId)
    }

  private def joinCookies(cookies: Seq[HttpCookie]*): Seq[HttpCookie] = {
    cookies.map(_.map(v => v.getName -> v).toMap).reduce(_ ++ _).values.toSeq
  }

  override def fullLogin(username: String, encryptedPassword: String, attemptNumber: Int = 0): ThrowableOr[Session] = {
    val password = textEncryptor.decrypt(encryptedPassword)
    val clientId = java.util.UUID.randomUUID.toString
    val maxAttempts = 2
    try {
      luxmedApi.setProxy()
      logger.info(s"Attempting to login using connection: ${luxmedApi.getProxy()}")
      for {
        r1 <- luxmedApi.login(username, password, clientId)
        tmpSession = Session(r1.body.accessToken, r1.body.accessToken, "", r1.cookies)
        _ = Thread.sleep(1000 + Random.nextInt(1000))
        r2 <- luxmedApi.loginToApp(tmpSession)
        jwtToken = extractAuthorizationTokenFromCookies(r2)
        cookies = joinCookies(r1.cookies, r2.cookies, Seq(new HttpCookie("GlobalLang", "pl")))
        accessToken = r1.body.accessToken
        tokenType = r1.body.tokenType
        _ = Thread.sleep(1000 + Random.nextInt(1000))
        r3 <- luxmedApi.getReservationPage(tmpSession, cookies)
      } yield Session(accessToken, tokenType, jwtToken, joinCookies(cookies, r3.cookies))
    } catch {
      case e: Exception if attemptNumber < maxAttempts => {
        logger.warn(s"Couldn't login from ${attemptNumber + 1} attempt. Trying again after a short pause", e)
        Thread.sleep(1000 + Random.nextInt(1000))
        fullLogin(username, encryptedPassword, attemptNumber + 1)
      }
      case e: Exception => Left(e)
    }
  }

  def getXsrfToken(accountId: Long): ThrowableOr[XsrfToken] = {
    withSession(accountId) { session =>
      luxmedApi.getForgeryToken(session).map(ft => XsrfToken(ft.body.token, ft.cookies))
    }
  }

  private def extractAccessTokenFromReservationPage(responsePage: String): String = {
    val accessTokenRegex = """(?s).*'Authorization', '(.+?)'.*""".r
    responsePage match {
      case accessTokenRegex(token) => token
      case _ => throw new java.lang.RuntimeException(s"Unable to extract authorization token from reservation page")
    }
  }

  private def extractAuthorizationTokenFromCookies(response: HttpResponse[_]): String = {
    response.cookies
      .find(_.getName == "Authorization-Token")
      .map(_.getValue)
      .getOrElse(throw new RuntimeException("Authorization-Token cookie not found in response"))
  }

}
