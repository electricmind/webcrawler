package ru.wordmetrix.webcrawler

import java.net.URI
import scala.Option.option2Iterable
import scala.actors.Actor
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource
import scala.xml.parsing.NoBindingFactoryAdapter
import java.io.CharArrayReader
import ActorDebug._
/*
 * Gather analyzes a page and elicits links and useful load.
 */
class Gather(storage: Storage, queue: Actor)(implicit val cfg: CFG)
        extends Actor with CFGAware {
    override val name = "Gather"

    val map = scala.collection.mutable.Set[(String, String)]()

    def page2xml_whole(page: WebCrawler.Page) =
        (new NoBindingFactoryAdapter).loadXML(
            new InputSource(new CharArrayReader(page.toArray)),
            new SAXFactoryImpl().newSAXParser())

    def page2xml(page: WebCrawler.Page): scala.xml.NodeSeq =
        ((page2xml_whole(page) \\ "div").
            filter(
                x => x.attribute("id").getOrElse("").toString ==
                    "mw-content-text"))

    def xml2seeds(xml: scala.xml.NodeSeq, base: URI) = (xml \\ "a").
        map(x => x.attribute("href")).flatten.
        map(x => base.resolve(x.toString)).
        filter(x => x.getHost() == "en.wikipedia.org").
        filter(x => {
            val id = (x.getHost(), x.getPath())
            if (map contains id) {
                false
            } else {
                map.+=(id)
                true
            }
        }).toSet

    def xml2vector(xml: scala.xml.NodeSeq) = Vector(
        "\\s+".r.split(xml.text).groupBy(x => x).map({
            case (x, y) => (x -> y.length.toDouble)
        }).toList)

    def xml2intell(xml: scala.xml.NodeSeq) = xml.text

    def act() = loop {
        react {
            case (seed: URI, page: WebCrawler.Page) => {
                this.debug("Gather data from %s", seed)
                try {
                    val xml = page2xml(page)
                    storage ! ((seed, xml2intell(xml)))
                    queue ! ((xml2seeds(xml, seed), seed, xml2vector(xml)))
                } catch {
                    case x => log("Gathering failed on %s: %s", seed, x)
                }
            }
        }
    }
}