package com.lbs.api

import cats.MonadError
import cats.implicits._
import com.lbs.api.exception._
import com.lbs.api.json.JsonSerializer.extensions._
import com.lbs.api.json.model._
import com.typesafe.scalalogging.StrictLogging
import scalaj.http.{HttpRequest, HttpResponse}

import java.net.{HttpCookie, HttpURLConnection, Proxy, InetSocketAddress}
import scala.util.{Failure, Success, Try, Random}
import scala.jdk.CollectionConverters._
import java.nio.file.{Files, Paths}

package object http extends StrictLogging {

  case class Session(accessToken: String, tokenType: String, jwtToken: String, cookies: Seq[HttpCookie], proxy: Option[Proxy])

  object headers {
    val `Content-Type` = "Content-Type"
    val `xsrf-token` = "xsrf-token"
    val Host = "Host"
    val Origin = "Origin"
    val Accept = "Accept"
    val `Accept-Encoding` = "Accept-Encoding"
    val `User-Agent` = "User-Agent"
    val `Custom-User-Agent` = "Custom-User-Agent"
    val `X-Api-Client-Identifier` = "X-Api-Client-Identifier"
    val `Accept-Language` = "accept-language"
    val Authorization = "Authorization"
    val AuthorizationToken = "authorization-token"
    val `X-Requested-With` = "X-Requested-With"
  }

  val proxyServers: List[String] = {
    Try {
      val path = Paths.get("proxy.txt")
      Files.readAllLines(path).asScala.filter(_.nonEmpty).filter(_.matches(".+:\\d+")).toList
    }.getOrElse {
      logger.warn("Proxy list is empty. Using direct connection.")
      List.empty[String]
    }
  }

  def getRandomProxy(): Option[Proxy] = {
    if (proxyServers.isEmpty) {
      None
    } else {
      val choices = proxyServers :+ "direct"
      val choice = choices(Random.nextInt(choices.length))
      choice match {
        case "direct" => None
        case server =>
          val Array(host, port) = server.split(":")
          Some(new Proxy(Proxy.Type.HTTP, new InetSocketAddress(host, port.toInt)))
      }
    }
  }

  private val SensitiveHeaders = List("passw", "access_token", "refresh_token", "authorization")

  implicit class ExtendedHttpRequest[F[_]: ThrowableMonad](httpRequest: HttpRequest) {
    def invoke(proxy: Option[Proxy]): F[HttpResponse[String]] = {
      val me = MonadError[F, Throwable]
      var attempts = 1
      val randomDelay = Random.nextInt(4000) + 1000
      var result: Option[HttpResponse[String]] = None

      while (attempts > 0 && result.isEmpty) {
        val configuredRequest = proxy match {
          case Some(proxy) => httpRequest.proxy(proxy)
          case None => httpRequest
        }

        logger.debug(s"Sending request:\n${hideSensitive(configuredRequest)}")
        try {
          val httpResponse = configuredRequest.asString
          logger.debug(s"Received response:\n${hideSensitive(httpResponse)}")

          val errorMaybe = extractLuxmedError(httpResponse)
          errorMaybe match {
            case Some(error) =>
              me.raiseError(error)
            case None =>
              Try(httpResponse.throwError) match {
                case Failure(error) =>
                  logger.error(s"${proxy.getOrElse("Direct")} connection error: ${error.getMessage}")
                  attempts -= 1
                  if (attempts > 0) {
                    Thread.sleep(randomDelay)
                  }
                case Success(value) =>
                  result = Some(value)
              }
          }
        } catch {
          case e: Exception =>
            logger.error(s"Proxy connection is invalid: $proxy - ${e.getMessage}")
            attempts -= 1
            if (attempts > 0) {
              Thread.sleep(randomDelay)
            }
        }
      }

      result match {
        case Some(response) => me.pure(response)
        case None => me.raiseError(new Exception(s"Failed to connect"))
      }
    }

    def param(key: String, value: Option[String]): HttpRequest = {
      value.map(v => httpRequest.param(key, v)).getOrElse(httpRequest)
    }

    private def luxmedErrorToApiException[T <: LuxmedBaseError](code: Int, error: T): ApiException = {
      val message = error.message
      val errorMessage = message.toLowerCase
      if (errorMessage.contains("invalid login or password"))
        new InvalidLoginOrPasswordException
      else if (errorMessage.contains("session has expired"))
        new SessionExpiredException
      else
        GenericException(code, message)
    }

    private def extractLuxmedError(httpResponse: HttpResponse[String]) = {
      val body = httpResponse.body
      val lowercasedBody = body.toLowerCase
      val code = httpResponse.code
      code match {
        case HttpURLConnection.HTTP_MOVED_TEMP if httpResponse.header("Location").exists(url => url.contains("/LogOn") || url.contains("/UniversalLink")) =>
          Some(new SessionExpiredException)
        case HttpURLConnection.HTTP_CONFLICT
            if lowercasedBody
              .contains("nieprawidłowy login lub hasło") || lowercasedBody.contains("invalid login or password") =>
          Some(new InvalidLoginOrPasswordException)
        case _ if code >=  HttpURLConnection.HTTP_BAD_REQUEST =>
          Try(body.as[LuxmedErrorsList])
            .orElse(Try(body.as[LuxmedErrorsMap]))
            .orElse(Try(body.as[LuxmedError]))
            .map(error => luxmedErrorToApiException(code, error))
            .toOption
        case _ =>
          Try(body.as[LuxmedErrorsMap])
            .map(error => luxmedErrorToApiException(code, error))
            .toOption
      }
    }

    private def hideSensitive(httpRequest: HttpRequest) = {
      httpRequest.copy(params = httpRequest.params.map { case (k, v) =>
        if (hide(k)) k -> "******" else k -> v
      })
    }

    private def hideSensitive(httpResponse: HttpResponse[String]): HttpResponse[String] = {
      httpResponse.copy(headers = httpResponse.headers.map { case (k, v) =>
        if (hide(k)) k -> IndexedSeq("******") else k -> v
      })
    }

    private def hideSensitive(httpResponse: F[HttpResponse[String]]) = {
      httpResponse.map { response =>
        response.copy(headers = response.headers.map { case (k, v) =>
          if (hide(k)) k -> IndexedSeq("******") else k -> v
        })
      }
    }

    private def hide(key: String) = {
      val lowerCaseKey = key.toLowerCase
      SensitiveHeaders.exists(h => lowerCaseKey.contains(h))
    }
  }

}
