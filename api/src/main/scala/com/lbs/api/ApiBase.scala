package com.lbs.api

import com.lbs.api.http.Session
import com.lbs.api.http.headers._
import scalaj.http.{BaseHttp, HttpRequest}

import java.net.HttpCookie

object ApiHttpOld extends BaseHttp(
  userAgent = "okhttp/4.9.1"
)

object ApiHttpNew extends BaseHttp(
  userAgent = "Mozilla/5.0 (Linux; Android 14; Pixel 8 Build/AP2A.240905.003.D1; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/113.0.5672.136 Mobile Safari/537.36"
)

trait ApiBase {
  private val uuid = java.util.UUID.randomUUID.toString.toUpperCase
  private val CommonHeaders =
    Map(
      Host -> "portalpacjenta.luxmed.pl",
      Origin -> "https://portalpacjenta.luxmed.pl",
      Accept -> "application/json, text/plain, */*",
      `Accept-Encoding` -> "gzip, deflate, br",
      `Accept-Language` -> "pl;q=1.0, pl;q=0.9, en;q=0.8"
    )

  private val OldApiHeaders =
    Map(
      `X-Api-Client-Identifier` -> "Android",
      `Custom-User-Agent` -> s"Portal Pacjenta; 4.45.0; $uuid; Android; 34; google Pixel 8",
      `User-Agent` -> "okhttp/4.9.1"
    )

  protected def httpUnauthorized(url: String): HttpRequest = {
    ApiHttpOld(s"https://portalpacjenta.luxmed.pl/PatientPortalMobileAPI/api/$url")
      .headers(CommonHeaders ++ OldApiHeaders)
  }

  protected def http(url: String, session: Session): HttpRequest = {
    ApiHttpOld(s"https://portalpacjenta.luxmed.pl/PatientPortalMobileAPI/api/$url")
      .headers(CommonHeaders ++ OldApiHeaders)
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
