package ru.wordmetrix.webcrawler

import java.net.URI

import Gather.GatherPage
import SeedQueue.{ SeedQueueEmpty, SeedQueueGet }
import akka.actor.{ Actor, ActorRef, Props, actorRef2Scala }
import ru.wordmetrix.smartfile.SmartFile.fromFile
import ru.wordmetrix.utils.{ CFG, CFGAware }
import ru.wordmetrix.utils.impl.URIEx

object WebGet {
    abstract sealed trait WebGetMessage
    case class WebGetRequest(seed: URI, gather: ActorRef) extends WebGetMessage

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
            //log("Getting a page %s ", seed)
            try {
                val page = (cfg.cache / seed.toFilename).readLines.mkString("")
                log("Getting from caches %s ", seed)
                gather ! GatherPage(seed, page)

            } catch {
                case x: Throwable =>
                    if (cfg.isdebug) log("Getting from source %s ", seed)
                    
                    time(s"Getting from source $seed") {
                        try {
                            val connection = seed.toURL.openConnection()
                            connection.getContentType().split(";").head match {
                                case "text/html" => {
                                    val text = io.Source.fromInputStream(
                                        connection.getInputStream()).
                                        getLines().mkString("\n")
                                    (cfg.cache / seed.toFilename).write(text)

                                    gather ! GatherPage(seed, text)
                                }
                                case _ => None
                            }
                        } catch {
                            case x =>
                                this.log("Download fault %s because %s", seed, x)
                        }
                    }
            }
            sender ! SeedQueueGet
        }

        case SeedQueueEmpty =>
            context.stop(self)
    }
}