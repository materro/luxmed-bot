
package com.lbs.server

import akka.actor.ActorSystem
import com.lbs.api.json.model.{AvailableVisitsTermPresentation, HistoricVisit, ReservedVisit}
import com.lbs.bot.Bot
import com.lbs.bot.telegram.TelegramBot
import com.lbs.server.conversation._
import com.lbs.server.lang.Localization
import com.lbs.server.repository.model
import com.lbs.server.repository.model.Monitoring
import com.lbs.server.service.{ApiService, DataService, MonitoringService}
import org.jasypt.util.text.{StrongTextEncryptor, TextEncryptor}
import org.springframework.beans.factory.annotation.{Autowired, Value}
import org.springframework.context.annotation.{Bean, Configuration}

import scala.io.Source
import scala.util.Try

@Configuration
class BootConfig {
  @Value("${security.secret}")
  private var secret: String = _

  @Value("${telegram.token}")
  private var telegramBotToken: String = _

  @Autowired
  private var apiService: ApiService = _

  @Autowired
  private var dataService: DataService = _

  @Autowired
  private var monitoringService: MonitoringService = _

  @Autowired
  private var localization: Localization = _

  @Bean
  def actorSystem: ActorSystem = ActorSystem()

  @Bean
  def textEncryptor: TextEncryptor = {
    val encryptor = new StrongTextEncryptor
    encryptor.setPassword(secret)
    encryptor
  }

  @Bean
  def authFactory: MessageSourceTo[Auth] = source => new Auth(source,
    dataService, unauthorizedHelpFactory, loginFactory, chatFactory)(actorSystem)

  @Bean
  def loginFactory: MessageSourceWithOriginatorTo[Login] = (source, originator) => new Login(source, bot,
    dataService, apiService, textEncryptor, localization, originator)(actorSystem)

  @Bean
  def bookFactory: UserIdTo[Book] = userId => new Book(userId, bot, apiService, dataService,
    monitoringService, localization, datePickerFactory, timePickerFactory, staticDataFactory, termsPagerFactory)(actorSystem)

  @Bean
  def bookWithTemplateFactory: UserIdTo[BookWithTemplate] = userId => new BookWithTemplate(userId, bot, apiService, dataService,
    monitoringService, localization, datePickerFactory, timePickerFactory, termsPagerFactory)(actorSystem)

  @Bean
  def unauthorizedHelpFactory: MessageSourceTo[UnauthorizedHelp] = source => new UnauthorizedHelp(source, bot)(actorSystem)

  @Bean
  def helpFactory: UserIdTo[Help] = userId => new Help(userId, bot, localization)(actorSystem)

  @Bean
  def monitoringsFactory: UserIdTo[Monitorings] =
    userId => new Monitorings(userId, bot, monitoringService, localization, monitoringsPagerFactory)(actorSystem)

  @Bean
  def monitoringsHistoryFactory: UserIdTo[MonitoringsHistory] =
    userId => new MonitoringsHistory(userId, bot, monitoringService, localization, monitoringsHistoryPagerFactory, bookWithTemplateFactory)(actorSystem)

  @Bean
  def historyFactory: UserIdTo[History] =
    userId => new History(userId, bot, apiService, localization, historyPagerFactory)(actorSystem)

  @Bean
  def visitsFactory: UserIdTo[Visits] =
    userId => new Visits(userId, bot, apiService, localization, visitsPagerFactory)(actorSystem)

  @Bean
  def bugFactory: UserIdTo[Bug] =
    userId => new Bug(userId, bot, dataService, bugPagerFactory, localization)(actorSystem)

  @Bean
  def settingsFactory: UserIdTo[Settings] =
    userId => new Settings(userId, bot, dataService, localization)(actorSystem)

  @Bean
  def accountFactory: UserIdTo[Account] =
    userId => new Account(userId, bot, dataService, localization, router)(actorSystem)

  @Bean
  def chatFactory: UserIdTo[Chat] =
    userId => new Chat(userId, dataService, monitoringService, bookFactory, helpFactory,
      monitoringsFactory, monitoringsHistoryFactory, historyFactory, visitsFactory, settingsFactory, bugFactory, accountFactory)(actorSystem)

  @Bean
  def datePickerFactory: UserIdWithOriginatorTo[DatePicker] = (userId, originator) =>
    new DatePicker(userId, bot, localization, originator)(actorSystem)

  @Bean
  def timePickerFactory: UserIdWithOriginatorTo[TimePicker] = (userId, originator) =>
    new TimePicker(userId, bot, localization, originator)(actorSystem)

  @Bean
  def staticDataFactory: UserIdWithOriginatorTo[StaticData] = (userId, originator) =>
    new StaticData(userId, bot, localization, originator)(actorSystem)

  @Bean
  def termsPagerFactory: UserIdWithOriginatorTo[Pager[AvailableVisitsTermPresentation]] = (userId, originator) =>
    new Pager[AvailableVisitsTermPresentation](userId, bot,
      (term: AvailableVisitsTermPresentation, page: Int, index: Int) => lang(userId).termEntry(term, page, index),
      (page: Int, pages: Int) => lang(userId).termsHeader(page, pages),
      Some("book"), localization, originator)(actorSystem)


  @Bean
  def visitsPagerFactory: UserIdWithOriginatorTo[Pager[ReservedVisit]] = (userId, originator) =>
    new Pager[ReservedVisit](userId, bot,
      (visit: ReservedVisit, page: Int, index: Int) => lang(userId).upcomingVisitEntry(visit, page, index),
      (page: Int, pages: Int) => lang(userId).upcomingVisitsHeader(page, pages),
      Some("cancel"), localization, originator)(actorSystem)

  @Bean
  def bugPagerFactory: UserIdWithOriginatorTo[Pager[model.Bug]] = (userId, originator) =>
    new Pager[model.Bug](userId, bot,
      (bug: model.Bug, page: Int, index: Int) => lang(userId).bugEntry(bug, page, index),
      (page: Int, pages: Int) => lang(userId).bugsHeader(page, pages),
      None, localization, originator)(actorSystem)

  @Bean
  def historyPagerFactory: UserIdWithOriginatorTo[Pager[HistoricVisit]] = (userId, originator) =>
    new Pager[HistoricVisit](userId, bot,
      (visit: HistoricVisit, page: Int, index: Int) => lang(userId).historyEntry(visit, page, index),
      (page: Int, pages: Int) => lang(userId).historyHeader(page, pages),
      None, localization, originator)(actorSystem)

  @Bean
  def monitoringsPagerFactory: UserIdWithOriginatorTo[Pager[Monitoring]] = (userId, originator) =>
    new Pager[Monitoring](userId, bot,
      (monitoring: Monitoring, page: Int, index: Int) => lang(userId).monitoringEntry(monitoring, page, index),
      (page: Int, pages: Int) => lang(userId).monitoringsHeader(page, pages),
      Some("cancel"), localization, originator)(actorSystem)

  @Bean
  def monitoringsHistoryPagerFactory: UserIdWithOriginatorTo[Pager[Monitoring]] = (userId, originator) =>
    new Pager[Monitoring](userId, bot,
      (monitoring: Monitoring, page: Int, index: Int) => lang(userId).monitoringHistoryEntry(monitoring, page, index),
      (page: Int, pages: Int) => lang(userId).monitoringsHistoryHeader(page, pages),
      Some("repeat"), localization, originator)(actorSystem)

  @Bean
  def router: Router = new Router(authFactory)(actorSystem)

  @Bean
  def telegram: TelegramBot = new TelegramBot(router ! _, telegramBotToken)

  @Bean
  def bot: Bot = new Bot(telegram)

  private def lang(userId: Login.UserId) = {
    localization.lang(userId.userId)
  }
}
