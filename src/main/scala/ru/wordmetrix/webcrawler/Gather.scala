package ru.wordmetrix.webcrawler

import java.io.CharArrayReader

import java.net.URI
import scala.Option.option2Iterable
import scala.util.Try
import scala.xml.parsing.NoBindingFactoryAdapter
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource
import EvaluatePriorityMatrix.{ EvaluatePriorityMatrixStop, EvaluatePriorityMatrixStopTargeting }
import GMLStorage.GMLStorageSeed
import akka.actor.{ Actor, ActorRef, Props, actorRef2Scala }
import ru.wordmetrix.features.Features
import ru.wordmetrix.utils.{ CFG, CFGAware, Html2Ascii }
import ru.wordmetrix.vector.Vector
import ru.wordmetrix.webcrawler.LinkContext.Feature
import ru.wordmetrix.webcrawler.LinkedVectorsStorage._

/*
 * Gather analyzes a page and elicits links and useful load.
 */

object Gather {
    abstract sealed trait GatherMessage

    case class GatherLink(storage: ActorRef, sample: ActorRef,
                          gmlstorage: ActorRef, linkedvectors: ActorRef)
            extends GatherMessage

    case class GatherStorageAck extends GatherMessage

    case class GatherStop extends GatherMessage

    abstract sealed class GatherSeed(seed: URI) extends GatherMessage

    // Gather data 
    case class GatherAllow(seed: URI) extends GatherSeed(seed)

    
    //
    case class GatherDecode(v : V) extends GatherMessage
    
    // Gather data from new page
    case class GatherPage(seed: URI, page: String) extends GatherSeed(seed)

    // Store Intel
    case class GatherIntel[U](seed: URI, load: U) extends GatherSeed(seed)

    // Queue new seed
    case class GatherSeeds(seed: URI, seeds: Set[URI], vector: Vector[Word])
        extends GatherSeed(seed)

    // Sample of links for future estimation
    case class GatherLinkContext(seed: URI,
                                 linkcontext: Map[URI, Vector[Feature]])
            extends GatherSeed(seed)

    def props(cfg: CFG): Props =
        Props(new Gather()(cfg))
}

class Gather()(
    implicit val cfg: CFG)
        extends Actor with CFGAware {
    override val name = "Gather"

    import Gather._

    def page2xml_whole(page: Page) = {
        (new NoBindingFactoryAdapter).loadXML(
            new InputSource(new CharArrayReader(page.toArray)),
            new SAXFactoryImpl().newSAXParser())
    }

    def page2xml(page: Page): scala.xml.NodeSeq =
        (page2xml_whole(page) \\ "body")
    //        ((page2xml_whole(page) \\ "div").
    //            filter(
    //                x => x.attribute("id").getOrElse("").toString ==
    //                    "mw-content-text"))

    def xml2seeds(xml: scala.xml.NodeSeq, base: URI, hosts: Set[String]): Set[URI] = (for {
        tag <- (xml \\ "a")
        link <- tag.attribute("href")
        uri <- Try(normalize(base, link.toString)).toOption
        if cfg.allhosts || (hosts contains uri.getHost())
    } yield uri).toSet

    def xml2vector(xml: scala.xml.NodeSeq,
                   index: Features.String2Word[String, Double]) =
        Features.fromText(Html2Ascii(xml).dump(), index)

    def xml2intel(xml: scala.xml.NodeSeq) = {
        new Html2Ascii(
            <html>{ xml \\ "title" }{
                    xml \\ "div" find (
                        x => x.attribute("id").getOrElse("").toString
                            == "mw-content-text"
                    ) match {
                            case Some(x)=> <body> { x } </body>
                            case None=> xml \\ "body" //getOrElse<html></html> 
                        }

                }
            </html>
        ).rectify()
    }

    import EvaluatePriorityMatrix._

    def receive(): Receive = {
        case GatherLink(storage, sample, gmlstorage, linkedvectors) =>
            log("Register storage: %s, sample: %s, gmlstorage:  %s", storage,
                sample, gmlstorage, linkedvectors)
            context.become(active(storage, sample, gmlstorage, linkedvectors,
                Set(), Features.String2Word[String, Double](), Set()))
    }

    def active(storage: ActorRef, sample: ActorRef, gmlstorage: ActorRef,
               linkedvectors: ActorRef,
               links: Set[String], index: Features.String2Word[String, Double],
               hosts: Set[String]): Receive = {

        case GatherAllow(seed) =>
            context.become(
                active(storage, sample, gmlstorage, linkedvectors, links, index,
                    hosts + seed.getHost()))

        case EvaluatePriorityMatrixStop =>
            debug("Stop gather")
            context.parent ! EvaluatePriorityMatrixStop
            sample ! EvaluatePriorityMatrixStop
            storage ! EvaluatePriorityMatrixStop
            context.stop(self)

        case EvaluatePriorityMatrixStopTargeting =>
            debug("Stop targeting")
            context.parent ! EvaluatePriorityMatrixStopTargeting

        case GatherDecode(v : V) =>
            debug("Decode for %s",sender)
            sender ! Vector(v.toList map ({case (id,f) => index.rmap(id) -> f}))
            
        case GatherPage(seed, page) => {
            debug("Gather page %s", seed)
            val hosts1 = hosts + seed.getHost()

            try {
                val xml = page2xml(page)
                storage ! GatherIntel(seed, xml2intel(page2xml_whole(page)))

                sample ! GatherLinkContext(seed,
                    new LinkContext(seed).extract(page2xml_whole(page)))

                val seeds = xml2seeds(xml, seed, hosts1)
                val (v, index1) = xml2vector(xml, index)

                context.parent ! GatherSeeds(seed,
                    seeds.filterNot(links contains _.toString),
                    v)

                gmlstorage ! GMLStorageSeed(seed, seeds, v)

                linkedvectors ! LinkedVectorsStorageSeed(seed,
                    seeds,
                    Features.fromText(Html2Ascii(xml).dump()))

                context.become(
                    active(storage, sample, gmlstorage, linkedvectors,
                        links | seeds.map(_.toString), index1,
                        hosts1))
            } catch {
                case x => log("Gathering failed on %s: %s", seed, x)
            }
        }
    }
}
