package ru.wordmetrix.webcrawler

import java.net.URI
import ru.wordmetrix.vector.Vector
import scala.Option.option2Iterable
import scala.actors.Actor
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource
import scala.xml.parsing.NoBindingFactoryAdapter
import java.io.CharArrayReader
import ActorDebug._
import scala.xml.Node
import ru.wordmetrix.webcrawler.LinkContext.Feature
/*
 * Gather analyzes a page and elicits links and useful load.
 */
class Gather(storage: Storage, queue: Actor, sample: SampleHierarchy2PriorityBase)(implicit val cfg: CFG)
        extends Actor with CFGAware {
    override val name = "Gather"

    val map = scala.collection.mutable.Set[String]()

    def page2xml_whole(page: WebCrawler.Page) =
        (new NoBindingFactoryAdapter).loadXML(
            new InputSource(new CharArrayReader(page.toArray)),
            new SAXFactoryImpl().newSAXParser())

    def page2xml(page: WebCrawler.Page): scala.xml.NodeSeq =
        (page2xml_whole(page) \\ "body")
    //        ((page2xml_whole(page) \\ "div").
    //            filter(
    //                x => x.attribute("id").getOrElse("").toString ==
    //                    "mw-content-text"))

    def xml2seeds(xml: scala.xml.NodeSeq, base: URI) = (xml \\ "a").
        map(x => x.attribute("href")).flatten.
        map(x => WebCrawler.normalize(base, x.toString)).
        //        map(x => {println(x); x}).
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
//TODO: improve splitter or just make one in separate package
        
    def xml2vector(xml: scala.xml.NodeSeq) = Vector(
        "\\s+".r.split(xml.text).groupBy(x => x).map({
            case (x, y) => (x -> y.length.toDouble)
        }).toList)

    def xml2intel(xml: scala.xml.NodeSeq) = new Html2Ascii(
        xml \\ "div" filter (
            x => x.attribute("id").getOrElse("").toString == "mw-content-text"
        )
    ).wrap()

    def act() = loop {
        react {
            case (seed: URI, page: WebCrawler.Page) => {
                this.debug("Gather data from %s", seed)
                try {
                    val xml = page2xml(page)

                    //this.debug("%s",xml.map(x => new LinkContext().extract( x )).reduce(_ ++ _))//(x : Map[WebCrawler.Seed,Vector[Feature]],y :Map[WebCrawler.Seed,Vector[Feature]]) => x ++ x))
                    //this.debug("%s",new LinkContext().extract(page2xml_whole(page)))
                    //                    println("!!",(/*(xml2seeds(xml, seed),*/ seed))
                    sample ! new LinkContext(seed).extract(page2xml_whole(page))
                    storage ! ((seed, xml2intel(xml)))
                    queue ! ((xml2seeds(xml, seed), seed, xml2vector(xml)))
                } catch {
                    case x => log("Gathering failed on %s: %s", seed, x)
                }
            }
        }
    }
}