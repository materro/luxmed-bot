package com.lbs.api

import com.lbs.api.http.Session
import com.lbs.api.http.headers._
import scalaj.http.{BaseHttp, HttpRequest}

import java.net.HttpCookie
import java.time.LocalDate
import java.time.DayOfWeek
import java.util.UUID

object ApiHttpOld extends BaseHttp(
  userAgent = "okhttp/4.12.0"
)

object ApiHttpNew extends BaseHttp(
  userAgent = "Mozilla/5.0 (Linux; Android 15; Pixel 9 Pro XL Build/AP4A.250105.002; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/124.0.6367.219 Mobile Safari/537.36"
)

trait ApiBase {
  private val CommonHeaders =
    Map(
      Host -> "portalpacjenta.luxmed.pl",
      Origin -> "https://portalpacjenta.luxmed.pl",
      Accept -> "application/json, text/plain, */*",
      `Accept-Encoding` -> "gzip, deflate, br",
      `Accept-Language` -> "pl;q=1.0, pl;q=0.9, en;q=0.8"
    )

  private def generateUUID(input: String): String = {
    val monday = LocalDate.now().minusDays(LocalDate.now().getDayOfWeek.getValue - 1)
    val period = monday.toString()
    UUID.nameUUIDFromBytes((input + period).getBytes("UTF-8")).toString()
  }

  private def generateClientId(login: String): String = generateUUID(login)

  private def generateDeviceId(login: String): String = generateUUID(s"device-$login")

  private def oldApiHeaders(deviceId: String) = CommonHeaders ++ Map(
    `X-Api-Client-Identifier` -> "Android",
    `Custom-User-Agent` -> s"Patient Portal; 5.0.0; $deviceId; Android; 35; google Pixel 9 Pro XL",
  )

  protected def httpUnauthorized(username: String, url: String): HttpRequest = {
    val clientId = generateClientId(username)
    val deviceId = generateDeviceId(username)
    ApiHttpOld(s"https://portalpacjenta.luxmed.pl/PatientPortalMobileAPI/api/$url")
      .param("client_id", clientId)
      .param("device_id", deviceId)
      .headers(oldApiHeaders(deviceId))
  }

  protected def http(url: String, session: Session): HttpRequest = {
    val deviceId = generateDeviceId(session.username)
    ApiHttpOld(s"https://portalpacjenta.luxmed.pl/PatientPortalMobileAPI/api/$url")
      .headers(oldApiHeaders(deviceId))
      .cookies(session.cookies)
      .header(Authorization, s"${session.tokenType} ${session.accessToken}")
  }

  protected def httpNewApi(url: String, session: Session, cookiesMaybe: Option[Seq[HttpCookie]] = None): HttpRequest = {
    val req = ApiHttpNew(s"https://portalpacjenta.luxmed.pl/PatientPortal/$url")
      .headers(CommonHeaders)
      .header(AuthorizationToken, s"Bearer ${session.jwtToken}")
    cookiesMaybe.map(cookies => req.cookies(cookies)).getOrElse(req.cookies(session.cookies))
  }

  protected def httpNewApiWithOldToken(url: String, session: Session, cookiesMaybe: Option[Seq[HttpCookie]] = None): HttpRequest = {
    val req = ApiHttpNew(s"https://portalpacjenta.luxmed.pl/PatientPortal/$url")
      .headers(CommonHeaders)
      .header(`X-Requested-With`, "pl.luxmed.pp")
      .header("Upgrade-Insecure-Requests", "1")
      .header(Authorization, session.accessToken)
    cookiesMaybe.map(cookies => req.cookies(cookies)).getOrElse(req.cookies(session.cookies))
  }
}
