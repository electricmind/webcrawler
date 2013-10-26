package ru.wordmetrix.webcrawler
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource
import scala.xml.parsing.NoBindingFactoryAdapter
import java.io.CharArrayReader
class Html2Ascii(page: scala.xml.NodeSeq) {

    def this(page: String) = this(
        (new NoBindingFactoryAdapter).loadXML(
            new InputSource(new CharArrayReader(page.toArray)),
            new SAXFactoryImpl().newSAXParser()) to //\\ "BODY"
    )

    // q.child.map { case a @ <a>{text}</a> => text + ":" + (a \ "@href").text; case t : xml.Text => t } mkString ""

    val div = Set("p", "h1", "h2", "h3", "h4", "h5", "h6", "div")
    val span = Set("i", "a", "b", "span", "sup")
    val discard = Set("img", "script", "form", "input", "button")
    
    def wrap(size : Int = 72) :String = dump(page).foldLeft(("",0))( {
        case ((s,i),' ' | '\n') if i > size => (s + "\n",0)
        //case ((s,i),'\') if i > size => (s + "\n",0)
        case ((s,i),'\n')  => (s + '\n',0)
        case ((s,i),c)  => (s + c,i+1)
    })._1
    
    def dump(nodes: Seq[scala.xml.Node] = page): String = {
        //println("qq")
        (nodes map {

            //case a @ <a>{ nodes @ _* }</a> => dump(nodes) + ":" + (a \ "@href")
            case <h1>{nodes @ _*}</h1> => "\n = " + dump(nodes) + " =\n"
            case <h2>{nodes @ _*}</h2> => "\n == " + dump(nodes) + " ==\n"
            case <h3>{nodes @ _*}</h3> => "\n === " + dump(nodes) + " ===\n"
            case <h4>{nodes @ _*}</h4> => "\n ==== " + dump(nodes) + " ====\n"
            case <h5>{nodes @ _*}</h5> => "\n ===== " + dump(nodes) + " =====\n"
            case <h6>{nodes @ _*}</h6> => "\n ====== " + dump(nodes) + " ======\n"
            
            case <table>{ tr @ _* }</table> => "\n" + "=" * 60 + "\n" + (tr map {
                case <tr>{ td @ _* }</tr> => td map {
                    case <td>{ nodes @ _* }</td> => dump(nodes) match {
                        case x if x contains "\n" => " -- \n" + x + " --"
                        case x                    => x
                    }
                    case <th>{ nodes @ _* }</th> => dump(nodes) match {
                        case x if x contains "\n" => " -- \n" + x + " --"
                        case x                    => x
                    }
                    case nodes => dump(nodes)
                } mkString (" | ")
                case nodes => dump(nodes)
            } mkString ("\n-\n" * 60)) + "\n" + "=" * 60 + "\n"

            case <br/> => "\n"

            case <ul>{ li @ _* }</ul> => li map {
                case <li>{ nodes @ _* }</li> => "\n - " + dump(nodes) + "\n"
                case nodes                   => dump(nodes)
            } mkString

            case <ol>{ li @ _* }</ol> => li.zipWithIndex.map {
                case (<li>{ nodes @ _* }</li>, i) => "\n " + i + " " + dump(nodes) + "\n"
                case (node, i)                    => dump(node)
            } mkString

            case xml.Text(t) => t

            case xml.Elem(_, label, _, _, nodes @ _*) if div contains label =>
                "\n" + dump(nodes) + "\n"

            case xml.Elem(_, label, _, _, nodes @ _*) if span contains label =>
                dump(nodes)

            case xml.Elem(
                _, label, _, _, nodes @ _*
                ) if discard contains label => ""

            case xml.Elem(_, label, _, _, nodes @ _*) =>
                "== " + label + "==\n" + dump(nodes) + "\n"

            case x => println("??", x.label, x); ""
        }) mkString
    }

}