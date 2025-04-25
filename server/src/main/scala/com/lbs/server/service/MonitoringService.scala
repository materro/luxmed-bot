package com.lbs.server.service

import com.lbs.api.exception.InvalidLoginOrPasswordException
import com.lbs.api.json.model._
import com.lbs.bot.Bot
import com.lbs.bot.model.{MessageSource, MessageSourceSystem}
import com.lbs.common.Scheduler
import com.lbs.server.lang.Localization
import com.lbs.server.repository.model._
import com.lbs.server.util.DateTimeUtil._
import com.lbs.server.util.ServerModelConverters._
import com.typesafe.scalalogging.StrictLogging
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service

import java.time.{LocalDateTime, ZonedDateTime}
import java.util.concurrent.{ConcurrentHashMap, ScheduledFuture}
import javax.annotation.PostConstruct
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Random

@Service
class MonitoringService extends StrictLogging {

  @Autowired
  private var bot: Bot = _
  @Autowired
  private var dataService: DataService = _
  @Autowired
  private var apiService: ApiService = _
  @Autowired
  private var localization: Localization = _

  private var activeMonitorings = new ConcurrentHashMap[JLong, (Monitoring, ScheduledFuture[_])]

  private val nextUnprocessedRecordIds = new ConcurrentHashMap[Long, mutable.Set[Long]]

  private val lastMonitoringTimes = new ConcurrentHashMap[Long, Long]

  private val activeMonitoringsCount = new ConcurrentHashMap[Long, Int]

  private val dbChecker = new Scheduler(1)

  private val monitoringExecutor = new Scheduler(10)

  private val TimeOffsetBase = 3.seconds

  private val MaxDelay = 60.seconds

  private val PeriodMaxDelta = 20.seconds

  private val MaxActiveMonitoringsPerUser = 2

  private val MinMonitoringIntervalDay = 185.seconds.toMillis

  private val MinMonitoringIntervalNight = 195.seconds.toMillis

  private def delay = Random.nextInt(MaxDelay.toSeconds.toInt).seconds

  private def period = (TimeOffsetBase.toSeconds + Random.nextInt(PeriodMaxDelta.toSeconds.toInt)).seconds

  private var checkedOn: ZonedDateTime = _

  def notifyUserAboutTerms(terms: Seq[TermExt], monitoring: Monitoring): Unit = {
    deactivateMonitoring(monitoring.accountId, monitoring.recordId)

    val fiveTerms = terms.take(5).zipWithIndex // send only 5 closest terms
    val messages = lang(monitoring.userId)

    val message = messages.availableTermsHeader(terms.length) + "\n\n" +
      fiveTerms.map { case (term, index) =>
        messages.availableTermEntry(term, monitoring, index)
      }.mkString

    bot.sendMessage(monitoring.source, message)
  }

  private def monitor(monitoring: Monitoring): Unit = {
    val accountId = monitoring.accountId
    val now = System.currentTimeMillis
    val currentHour = (System.currentTimeMillis.milliseconds.toHours % 24).toInt
    val isNightTime = currentHour < 6 || currentHour > 22
    val minInterval = if (isNightTime) MinMonitoringIntervalNight else MinMonitoringIntervalDay

    val lastMonitoringTime: Long = Option(lastMonitoringTimes.get(accountId)).getOrElse(now - minInterval)
    val timeSinceLastMonitoring = now - lastMonitoringTime

    val lastMonitoringCount: Int = Option(activeMonitoringsCount.get(accountId)).getOrElse(0)

    logger.debug(
      s"Monitoring [#${monitoring.recordId}] "
      + s"for account [#${monitoring.accountId}] "
      + s"is executing (${lastMonitoringCount + 1}/$MaxActiveMonitoringsPerUser)")

    if (lastMonitoringCount >= MaxActiveMonitoringsPerUser) {
      logger.debug(
        s"Skipping monitoring "
          + s"because the maximum number of active monitorings "
          + s"per user ($MaxActiveMonitoringsPerUser) has been reached"
      )
      activeMonitoringsCount.put(accountId, 0)
      lastMonitoringTimes.put(accountId, now)
      return
    }

    val recordIds: mutable.Set[Long] = Option(nextUnprocessedRecordIds.get(accountId)).getOrElse(mutable.Set.empty)
    if (recordIds.nonEmpty) {
      val nextRecordId = recordIds.min
      if (monitoring.recordId != nextRecordId) {
        logger.debug(
          s"Skipping monitoring because this recordId is different from the next unprocessed record [$nextRecordId]"
        )
        return
      }
    } else {
      initializeUnprocessedRecordIds(accountId)
    }

    if (timeSinceLastMonitoring < minInterval) {
      logger.debug(
        s"Skipping monitoring "
          + s"because the minimum interval ($minInterval) has not passed "
          + s"since the last monitoring ($timeSinceLastMonitoring)"
      )
      return
    }

    activeMonitoringsCount.put(accountId, lastMonitoringCount + 1)
    Option(nextUnprocessedRecordIds.get(accountId)).foreach(_.remove(monitoring.recordId))

    logger.debug(s"Looking for available terms...")
    if (Option(nextUnprocessedRecordIds.get(accountId)).forall(_.isEmpty)) {
      logger.debug(s"Initializing next monitorings")
      initializeUnprocessedRecordIds(accountId)
    }
    logger.debug(s"Next monitorings for account [#${monitoring.accountId}]: ${nextUnprocessedRecordIds.get(accountId)}")

    val dateFrom = optimizeDateFrom(monitoring.dateFrom.toLocalDateTime, monitoring.offset)
    val termsEither = apiService.getAvailableTerms(
      monitoring.accountId,
      monitoring.cityId,
      monitoring.clinicId,
      monitoring.serviceId,
      monitoring.doctorId,
      dateFrom,
      monitoring.dateTo.toLocalDateTime,
      timeFrom = monitoring.timeFrom,
      timeTo = monitoring.timeTo
    )
    termsEither match {
      case Right(terms) =>
        if (terms.nonEmpty) {
          logger.debug(s"Found ${terms.length} terms by monitoring [#${monitoring.recordId}]")
          if (monitoring.autobook) {
            val term = terms.head
            bookAppointment(term, monitoring, monitoring.rebookIfExists)
          } else {
            notifyUserAboutTerms(terms, monitoring)
          }
        } else {
          logger.debug(s"No new terms found...")
        }
      case Left(ex: InvalidLoginOrPasswordException) =>
        logger.error(s"User entered invalid name or password. Monitoring will be disabled", ex)
        bot.sendMessage(monitoring.source, lang(monitoring.userId).invalidLoginOrPassword)
        val activeUserMonitorings = dataService.getActiveMonitorings(monitoring.accountId)
        activeUserMonitorings.foreach { m =>
          deactivateMonitoring(m.accountId, m.recordId)
        }
      case Left(ex) => logger.error(s"Unable to receive terms by monitoring [#${monitoring.recordId}]", ex)
    }
  }

  private def optimizeDateFrom(date: LocalDateTime, offset: Int) = {
    val nowWithOffset = LocalDateTime.now().plusMinutes(offset)
    if (date.isBefore(nowWithOffset)) nowWithOffset else date
  }

  private def initializeMonitorings(allMonitorings: Seq[Monitoring]): Unit = {
    var delayIndex = 0
    allMonitorings.foreach { monitoring =>
      if (monitoring.active && !activeMonitorings.contains(monitoring.recordId)) {
        delayIndex += TimeOffsetBase.toSeconds.toInt
        val delaySnapshot = (delayIndex + delay.toSeconds).seconds
        val periodSnapshot = period
        val future = monitoringExecutor.schedule(monitor(monitoring), delaySnapshot, periodSnapshot)
        logger.debug(
          s"Scheduled monitoring: [#${monitoring.recordId}] with delay: $delaySnapshot and period: $periodSnapshot"
        )
        activeMonitorings.put(monitoring.recordId, (monitoring, future))
      }
    }
    initializeUnprocessedRecordIds()
    logger.debug(s"Number of active monitorings: ${activeMonitorings.size}")
  }

  private def initializeUnprocessedRecordIds(accountId: Option[Long] = None): Unit = {
    activeMonitorings.forEach { (key: JLong, value: (Monitoring, ScheduledFuture[_])) =>
      val monitoring = value._1
      if (accountId.isEmpty || accountId.contains(monitoring.accountId)) {
        val set = nextUnprocessedRecordIds.computeIfAbsent(monitoring.accountId, _ => mutable.Set.empty[Long])
        set.add(key)
      }
    }
  }

  private def initializeNewMonitorings(): Unit = {
    logger.debug(s"Looking for new monitorings created since $checkedOn")
    val currentTime = ZonedDateTime.now()
    val monitorings = dataService.getActiveMonitoringsSince(checkedOn)
    logger.debug(s"New monitorings found: ${monitorings.length}")
    checkedOn = currentTime
    if (monitorings.isEmpty) return
    initializeMonitorings(monitorings)
  }

  def notifyChatAboutDisabledMonitoring(monitoring: Monitoring): Unit = {
    bot.sendMessage(monitoring.source, lang(monitoring.userId).nothingWasFoundByMonitoring(monitoring))
  }

  private def disableOutdated(): Unit = {
    val now = ZonedDateTime.now()
    val toDisable = activeMonitorings.entrySet().stream()
      .filter(entry => entry.getValue._1.dateTo.isBefore(now))
      .map(entry => entry.getKey -> entry.getValue._1)
      .toArray(size => new Array[(JLong, Monitoring)](size))

    toDisable.foreach { case (id, monitoring) =>
      logger.debug(s"Monitoring [#$id] is going to be disable as outdated")
      notifyChatAboutDisabledMonitoring(monitoring)
      deactivateMonitoring(monitoring.accountId, id)
    }
  }

  private def updateMonitorings(): Unit = {
    initializeNewMonitorings()
    disableOutdated()
  }

  private def initializeDbChecker(): Unit = {
    dbChecker.schedule(updateMonitorings(), 1.minute)
  }

  private def bookAppointment(term: TermExt, monitoring: Monitoring, rebookIfExists: Boolean): Unit = {
    val bookingResult = for {
      xsrfToken <- apiService.getXsrfToken(monitoring.accountId)
      reservationLocktermResponse <- apiService.reservationLockterm(
        monitoring.accountId,
        xsrfToken,
        term.mapTo[ReservationLocktermRequest]
      )
      temporaryReservationId = reservationLocktermResponse.value.temporaryReservationId
      response <-
        if (reservationLocktermResponse.value.changeTermAvailable && rebookIfExists) {
          logger.info(s"Service [${monitoring.serviceName}] is already booked. Trying to update term")
          bookOrUnlockTerm(
            monitoring.accountId,
            xsrfToken,
            temporaryReservationId,
            apiService.reservationChangeTerm(
              _,
              xsrfToken,
              (reservationLocktermResponse, term).mapTo[ReservationChangetermRequest]
            )
          )
        } else {
          bookOrUnlockTerm(
            monitoring.accountId,
            xsrfToken,
            temporaryReservationId,
            apiService.reservationConfirm(
              _,
              xsrfToken,
              (reservationLocktermResponse, term).mapTo[ReservationConfirmRequest]
            )
          )
        }
    } yield (response, reservationLocktermResponse.value.doctorDetails)
    bookingResult match {
      case Right((_, doctorDetails)) =>
        bot.sendMessage(monitoring.source, lang(monitoring.userId).appointmentIsBooked(term, monitoring, doctorDetails))
        deactivateMonitoring(monitoring.accountId, monitoring.recordId)
      case Left(ex) =>
        logger.error(s"Unable to book appointment by monitoring [${monitoring.recordId}]", ex)
    }
  }

  private def bookOrUnlockTerm[T](
    accountId: Long,
    xsrfToken: XsrfToken,
    temporaryReservationId: Long,
    fn: (Long) => Either[Throwable, T]
  ): Either[Throwable, T] = {
    fn(accountId) match {
      case r @ Left(_) =>
        apiService.deleteTemporaryReservation(accountId, xsrfToken, temporaryReservationId)
        r
      case r => r
    }
  }

  def deactivateMonitoring(accountId: JLong, monitoringId: JLong): Unit = {
    val activeMonitoringMaybe = Option(activeMonitorings.remove(monitoringId))
    Option(nextUnprocessedRecordIds.get(accountId)).foreach(_.remove(monitoringId))

    activeMonitoringMaybe match {
      case Some((monitoring, future)) =>
        logger.debug(s"Deactivating scheduled monitoring [#$monitoringId]")
        if (!future.isCancelled) {
          future.cancel(true)
        }
        monitoring.active = false
        dataService.saveMonitoring(monitoring)
      case None =>
        logger.debug(s"Deactivating unscheduled monitoring [#$monitoringId]")
        dataService.findMonitoring(accountId, monitoringId).foreach { monitoring =>
          monitoring.active = false
          dataService.saveMonitoring(monitoring)
        }
    }
  }

  def createMonitoring(monitoring: Monitoring): Monitoring = {
    val userMonitoringsCount = dataService.getActiveMonitoringsCount(monitoring.accountId)
    require(userMonitoringsCount + 1 <= 10, lang(monitoring.userId).maximumMonitoringsLimitExceeded)
    dataService.saveMonitoring(monitoring)
  }

  def getActiveMonitorings(accountId: Long): Seq[Monitoring] = {
    dataService.getActiveMonitorings(accountId)
  }

  def getMonitoringsPage(accountId: Long, start: Int, count: Int): Seq[Monitoring] = {
    dataService.getMonitoringsPage(accountId, start, count)
  }

  def getAllMonitoringsCount(accountId: Long): Long = {
    dataService.getAllMonitoringsCount(accountId)
  }

  def bookAppointmentByScheduleId(accountId: Long, monitoringId: Long, scheduleId: Long, time: Long): Unit = {
    val monitoringMaybe = dataService.findMonitoring(accountId, monitoringId)
    monitoringMaybe match {
      case Some(monitoring) =>
        val termsEither = apiService.getAvailableTerms(
          monitoring.accountId,
          monitoring.cityId,
          monitoring.clinicId,
          monitoring.serviceId,
          monitoring.doctorId,
          monitoring.dateFrom.toLocalDateTime,
          monitoring.dateTo.toLocalDateTime,
          timeFrom = monitoring.timeFrom,
          timeTo = monitoring.timeTo
        )
        termsEither match {
          case Right(terms) =>
            val termMaybe = terms.find(term =>
              term.term.scheduleId == scheduleId && minutesSinceBeginOf2018(term.term.dateTimeFrom.get) == time
            )
            termMaybe match {
              case Some(term) =>
                bookAppointment(term, monitoring, rebookIfExists = true)
              case None =>
                bot.sendMessage(monitoring.source, lang(monitoring.userId).termIsOutdated)
            }
          case Left(ex: InvalidLoginOrPasswordException) =>
            logger.error(s"User entered invalid name or password. Monitoring will be disabled", ex)
            bot.sendMessage(monitoring.source, lang(monitoring.userId).loginHasChangedOrWrong)
          case Left(ex) =>
            logger.error(s"Error occurred during receiving terms for monitoring [#${monitoring.recordId}]", ex)
        }
      case None =>
        logger.debug(s"Monitoring [#$monitoringId] not found in db")
    }
  }

  implicit class MonitoringAsSource(monitoring: Monitoring) {
    def source: MessageSource = MessageSource(MessageSourceSystem(monitoring.sourceSystemId), monitoring.chatId)
  }

  private def lang(userId: Long) = localization.lang(userId)

  @PostConstruct
  private def initialize(): Unit = {
    checkedOn = ZonedDateTime.now()
    val monitorings = dataService.getActiveMonitorings
    logger.debug(s"Active monitorings found: ${monitorings.length}")
    initializeMonitorings(monitorings)
    disableOutdated()
    initializeDbChecker()
  }
}
