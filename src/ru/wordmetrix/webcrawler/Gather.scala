package ru.wordmetrix.webcrawler

import java.net.URI
import scala.Option.option2Iterable
import scala.actors.Actor
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource
import scala.xml.parsing.NoBindingFactoryAdapter
import java.io.CharArrayReader

class Gather(storage: Storage, queue: Queue)(implicit val cfg: CFG)
        extends Actor {

    def page2xml(page: WebCrawler.Page) = (new NoBindingFactoryAdapter).loadXML(
        new InputSource(new CharArrayReader(page.toArray)),
        new SAXFactoryImpl().newSAXParser())

    def xml2seeds(xml: scala.xml.Node, base : URI) = (xml \\ "a").
        map(x => x.attribute("href")).flatten.
        map(x => base.resolve(x.toString)).toSet

    def xml2vector(xml: scala.xml.Node) = Vector(
        "\\s+".r.split(xml.text).groupBy(x => x).map({
            case (x, y) => (x -> y.length.toDouble)
        }).toList)

    val xml2intell = xml2vector _

    def act() = loop {
        react { 
            case (seed : URI, page: WebCrawler.Page) => {
                println("gather " + seed)
                val xml = page2xml(page)
                storage ! ((seed, xml2intell(xml)))
                
//                println("gathered " + (xml2seeds(xml,seed)))
//                println(xml2vector(xml))
                queue ! ((xml2seeds(xml,seed), seed, xml2vector(xml)))
            }
        }
    }
}