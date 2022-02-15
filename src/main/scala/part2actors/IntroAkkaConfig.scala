package part2actors

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object IntroAkkaConfig extends App {

  class SimpleLoggingActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  /**
   * 1 - inline configuration
   */
  val configString =
    """
      |akka {
      |  loglevel = ERROR
      |}
    """.stripMargin
  // DEBUG, INFO, WARN, ERROR

//  val config = ConfigFactory.parseString(configString)
//  val system = ActorSystem("ConfigurationDemo", config)
//  val actor = system.actorOf(Props[SimpleLoggingActor])
//
//  actor ! "A message to remember"

  /**
   * 2 - config file
   */
//  val defaultConfigSystem = ActorSystem("DefaultConfigFileDemo")
//  val defaultConfigActor = defaultConfigSystem.actorOf(Props[SimpleLoggingActor])
//  defaultConfigActor ! "Remember me"

  /**
   * 3 - separate config in the same file
   */
  val specialConfig = ConfigFactory.load().getConfig("mySpecialConfig")
  val specialConfigSystem = ActorSystem("SpecialConfigDemo", specialConfig)
  val specialConfigActor = specialConfigSystem.actorOf(Props[SimpleLoggingActor])
  specialConfigActor ! "Remember me, I'm special"

  /**
   * 4 - separate config in another file
   */
  val separateConfig = ConfigFactory.load("secretFolder/secretConfiguration.conf")
  println(s"Separate config log level: ${separateConfig.getString("akka.loglevel")}")

  /**
   * 5 - different file formats
   * JSON, properties
   */
  val jsonConfig = ConfigFactory.load("json/jsonConfig.json")
  println(s"Json config: ${jsonConfig.getString("aJsonProperty")}")
  println(s"Json config: ${jsonConfig.getString("akka.loglevel")}")

  val propsConfig = ConfigFactory.load("props/propsConfiguration.properties")
  println(s"Props config: ${propsConfig.getString("my.simpleProperty")}")
  println(s"Props config: ${propsConfig.getString("akka.loglevel")}")

}
