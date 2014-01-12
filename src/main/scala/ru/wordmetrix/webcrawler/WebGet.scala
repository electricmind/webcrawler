package ru.wordmetrix.webcrawler

import java.net.URI

import Gather.GatherPage
import SeedQueue.{SeedQueueEmpty, SeedQueueGet}
import akka.actor.{Actor, ActorRef, Props, actorRef2Scala}
import ru.wordmetrix.smartfile.SmartFile.fromFile
import ru.wordmetrix.utils.{CFG, CFGAware}
import ru.wordmetrix.utils.ActorDebug.actor2ActorDebug
import ru.wordmetrix.utils.Utils.uriToFilename

object WebGet {
    abstract sealed trait WebGetMessage
    case class WebGetRequest(seed: URI, gather : ActorRef) extends WebGetMessage

    def props(cfg: CFG): Props =
        Props(new WebGet()(cfg))
}
/*
 * WebGet gets an information from web pages 
 */
class WebGet()(implicit cfg: CFG) extends Actor
        with CFGAware {
    override val name = "WebGet"

    import Gather._
    import WebGet._
    import SeedQueue._
  
    def receive(): Receive = {
        case WebGetRequest(seed, gather) => {
            this.debug("Download %s", seed)
            try {
                this.debug("Sent to gather %s from cache", seed)
                gather ! GatherPage(seed, (cfg.cache / uriToFilename(seed)).readLines.mkString(""))
            } catch {
                case x: Throwable =>
                    try {
                        val connection = seed.toURL.openConnection()
                        connection.getContentType().split(";").head match {
                            case "text/html" => {
                                val text = io.Source.fromInputStream(
                                    connection.getInputStream()).
                                    getLines().mkString("\n")
                                (cfg.cache / uriToFilename(seed)).write(text)

                                this.debug("Sent to gather %s from web", seed)
                                gather ! GatherPage(seed, text)
                            }
                            case _ => None
                        }
                    } catch { case x => println(x) }
            }
            sender ! SeedQueueGet
        }

        case SeedQueueEmpty =>
            println("suicide")
            //self ! Kill
            //self ! PoisonPill
            context.stop(self)
    }
}