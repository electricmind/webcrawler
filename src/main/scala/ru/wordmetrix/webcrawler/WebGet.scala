package ru.wordmetrix.webcrawler
import ru.wordmetrix.smartfile.SmartFile._
import scala.actors.Actor
import ru.wordmetrix.utils.Utils._
import ru.wordmetrix.utils.{CFG, CFGAware, log, debug}
import ru.wordmetrix.utils.ActorDebug.actor2ActorDebug

/*
 * WebGet gets an information from web pages 
 */
class WebGet(queue: Actor, gather: Gather)(implicit cfg: CFG) extends Actor
        with CFGAware {
    override val name = "WebGet"
    def act() = {
        loop {
            react {
                case seed: WebCrawler.Seed => {
                    this.debug("Download %s", seed)
                    try {
                 //       gather ! (seed, (cfg.cache / uriToFilename(seed)).readLines.mkString(""))
                        this.debug("Sent to gather %s from cache", seed)
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

                   //                     gather ! (seed, text)
                                        this.debug("Sent to gather %s from web", seed)
                                    }
                                    case _ => None
                                }
                            } catch { case x => println(x) }
                    }
                    queue ! this
                }
            }
        }
    }
}