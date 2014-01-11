package ru.wordmetrix.webcrawler
import ru.wordmetrix.smartfile.SmartFile._
import ru.wordmetrix.utils.Utils._
import ru.wordmetrix.utils.{ CFG, CFGAware, log, debug }
import ru.wordmetrix.utils.ActorDebug.actor2ActorDebug
import java.io.CharArrayReader
import java.net.URI
import scala.Option.option2Iterable
import scala.xml.parsing.NoBindingFactoryAdapter
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource
import akka.actor.{ Actor, ActorRef, Props, actorRef2Scala }
import ru.wordmetrix.features.Features
import ru.wordmetrix.utils.{ CFG, CFGAware, Html2Ascii, debug, log }
import ru.wordmetrix.vector.Vector
import ru.wordmetrix.webcrawler.LinkContext.Feature
import akka.actor.Kill
import akka.actor.PoisonPill

object WebGet {
    abstract sealed trait WebGetMessage
    //case class SeedQueueRequest(seed: URI) extends SeedQueueMessage

    // case class SeedQueuePostpone(seed: URI) extends SeedQueueSeed(seed)
    //case class SeedQueueAck extends SendQueueMessage

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
    import SeedQueue._
    
    def receive(): Receive = {
        case SeedQueueRequest(seed,gather) => {
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