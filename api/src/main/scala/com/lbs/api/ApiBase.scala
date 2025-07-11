package com.lbs.api

import com.lbs.api.http.Session
import com.lbs.api.http.headers._
import scalaj.http.{BaseHttp, HttpRequest}

import java.net.HttpCookie

object ApiHttp
  extends BaseHttp(
    userAgent =
      "Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148"
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
      `X-Api-Client-Identifier` -> "iPhone",
      `Custom-User-Agent` -> s"PatientPortal; 4.42.0; $uuid; iOS; 18.0; iPhone16,2",
      `User-Agent` -> "Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148"
    )

  private val NewApiHeaders =
    Map(
      `Custom-User-Agent` -> s"PatientPortal; 4.42.0; $uuid; iOS; 18.0; iPhone16,2",
      `User-Agent` -> "Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Mobile/15E148"
    )

  protected def httpUnauthorized(url: String): HttpRequest = {
    ApiHttp(s"https://portalpacjenta.luxmed.pl/PatientPortalMobileAPI/api/$url")
      .headers(CommonHeaders ++ OldApiHeaders)
  }

  protected def http(url: String, session: Session): HttpRequest = {
    ApiHttp(s"https://portalpacjenta.luxmed.pl/PatientPortalMobileAPI/api/$url")
      .headers(CommonHeaders ++ OldApiHeaders)
      .cookies(session.cookies)
      .header(Authorization, s"${session.tokenType} ${session.accessToken}")
  }

  protected def httpNewApi(url: String, session: Session, cookiesMaybe: Option[Seq[HttpCookie]] = None): HttpRequest = {
    val req = ApiHttp(s"https://portalpacjenta.luxmed.pl/PatientPortal/$url")
      .headers(CommonHeaders ++ NewApiHeaders)
      .header(AuthorizationToken, s"Bearer ${session.jwtToken}")
    cookiesMaybe.map(cookies => req.cookies(cookies)).getOrElse(req.cookies(session.cookies))
  }

  protected def httpNewApiWithOldToken(url: String, session: Session, cookiesMaybe: Option[Seq[HttpCookie]] = None): HttpRequest = {
    val req = ApiHttp(s"https://portalpacjenta.luxmed.pl/PatientPortal/$url")
      .headers(CommonHeaders ++ NewApiHeaders)
      .header(`X-Requested-With`, "pl.luxmed.pp")
      .header(Authorization, session.accessToken)
    cookiesMaybe.map(cookies => req.cookies(cookies)).getOrElse(req.cookies(session.cookies))
  }
}
