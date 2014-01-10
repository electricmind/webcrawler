package ru.wordmetrix.webcrawler

import java.net.URI
import ru.wordmetrix.vector.Vector
import scala.Option.option2Iterable
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource
import scala.xml.parsing.NoBindingFactoryAdapter
import java.io.CharArrayReader
import scala.xml.Node
import ru.wordmetrix.webcrawler.LinkContext.Feature
import ru.wordmetrix.utils.Html2Ascii
import ru.wordmetrix.features.Features
import ru.wordmetrix.utils.{ CFG, CFGAware, log, debug }
import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
import akka.actor.ActorRef

/*
 * Gather analyzes a page and elicits links and useful load.
 */

object Gather {
    abstract sealed trait GatherMessage

    case class GatherStorage(storage: Storage) extends GatherMessage
    case class GatherStorageAck extends GatherMessage

    case class GatherStop extends GatherMessage

    abstract sealed class GatherSeed(seed: URI) extends GatherMessage

    // Gather data from new page
    case class GatherPage(seed: URI, page: String) extends GatherSeed(seed)

    // Store Intel
    case class GatherIntel[U](seed: URI, load: U) extends GatherSeed(seed)

    // Queue new seed
    case class GatherSeeds(seed: URI, seeds: Set[URI], vector: Vector[String])
        extends GatherSeed(seed)

    // Sample of links for future estimation
    case class GatherLinkContext(seed: URI,
                                 linkcontext: Map[URI, Vector[Feature]])
            extends GatherSeed(seed)

    def props(queue: ActorRef, storage: ActorRef, sample: ActorRef, cfg: CFG): Props =
        Props(new Gather(queue, storage, sample)(cfg))
}

class Gather(
    queue: ActorRef,
    storage: ActorRef,
    sample: ActorRef)(
        implicit val cfg: CFG)
        extends Actor with CFGAware {
    override val name = "Gather"

    import Gather._
    val map = scala.collection.mutable.Set[String]()

    def page2xml_whole(page: WebCrawler.Page) = debug.time("page2xml") {
        (new NoBindingFactoryAdapter).loadXML(
            new InputSource(new CharArrayReader(page.toArray)),
            new SAXFactoryImpl().newSAXParser())
    }

    def page2xml(page: WebCrawler.Page): scala.xml.NodeSeq =
        (page2xml_whole(page) \\ "body")
    //        ((page2xml_whole(page) \\ "div").
    //            filter(
    //                x => x.attribute("id").getOrElse("").toString ==
    //                    "mw-content-text"))

    def xml2seeds(xml: scala.xml.NodeSeq, base: URI) = (xml \\ "a").
        map(x => x.attribute("href")).flatten.
        map(x => WebCrawler.normalize(base, x.toString)).
        filter(x => x.getHost() == "en.wikipedia.org").
        filter(x => {
            val id = x.toString
            if (map contains id) {
                false
            } else {
                map.+=(id)
                true
            }
        }).toSet

    def xml2vector(xml: scala.xml.NodeSeq) =
        Features.fromText(Html2Ascii(xml).dump())

    def xml2intel(xml: scala.xml.NodeSeq) = debug.time("xml2intel") {
        new Html2Ascii(
            xml \\ "div" find (
                x => x.attribute("id").getOrElse("").toString
                    == "mw-content-text"
            ) getOrElse <html></html>
        ).wrap()
    }

    def receive(): Receive = {
        case GatherPage(seed, page) => {
            try {
                val xml = page2xml(page)
                sample ! GatherLinkContext(seed,
                    new LinkContext(seed).extract(page2xml_whole(page)))
                storage ! GatherIntel(seed, xml2intel(xml))
                queue ! GatherSeeds(seed, xml2seeds(xml, seed), xml2vector(xml))
            } catch {
                case x => log("Gathering failed on %s: %s", seed, x)
            }

        }
    }
}
