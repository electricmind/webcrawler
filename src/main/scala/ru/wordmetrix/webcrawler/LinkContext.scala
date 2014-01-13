package ru.wordmetrix.webcrawler
/*
 * LinkContext extracts a context of links from xml page as feature-vector
 */
import WebCrawler.Seed
import ru.wordmetrix.vector._
import java.net.URI
object LinkContext {
    object Feature {
        def apply(x: String) = x.split("=") match {
            case Array("id", v)    => new FeatureId(v)
            case Array("class", v) => new FeatureClass(v)
            case Array(v)          => new FeatureName(v)
        }
    }
    abstract class Feature {
        val order: Int
        val x: String

        override def equals(f: Any) = f match {
            case f: Feature => order == f.order && x == f.x
            case _          => false
        }
    }
    class FeatureName(val x: String) extends Feature {
        val order = 1
        override def toString = """%s""".format(x)
    }

    class FeatureId(val x: String) extends Feature {
        val order = 2
        override def toString = """id="%s"""".format(x)
    }

    class FeatureClass(val x: String) extends Feature {
        val order = 3
        override def toString = """class="%s"""".format(x)
    }

    implicit val featureOrdering = Ordering.fromLessThan[Feature]((x, y) => {
        //println(x,y)
        //x.toString < y.toString
        if (x.order < y.order) {
            true
        } else if (x.order > y.order) {
            false
        } else x.x < y.x

    })

}

class LinkContext(base: URI) {

    import LinkContext._
    type V = Vector[Feature]
    val v = new Vector[Feature]()

    def extract(xml: scala.xml.Node,
                v: V = new Vector[Feature](),
                map: Map[Seed, Vector[Feature]] = Map()): //
                Map[Seed, Vector[Feature]] =
        if (xml.isEmpty) {
            map
        } else {
            (xml \ "_").foldLeft(map)({
                case (map, x) => {
                    val l: List[Feature] = new FeatureName(x.label) :: (
                        (x.attribute("id") match {
                            case Some(x) => {
                                Some(new FeatureId(x.toString))
                            }
                            case None => None
                        }) :: (x.attribute("class") match {
                            case Some(x) => x.toString.split(" ").filterNot(_.startsWith("page")).map(x =>
                                Some(new FeatureClass(x))).toList
                            case None => List[Option[Feature]]()
                        }) flatten);

                    val v1 = v + Vector[Feature](l.map(x => x -> 1.0))
                    extract(x, v1, x.attribute("href") match {
                        case Some(ref) if (new URI(ref.toString).getHost() == base.getHost()) => {
                            val x = WebCrawler.normalize(base.toString.replace("|", "%124"), ref.toString.replace("|", "%124"))

                            map + (x -> (map.get(x) match {
                                case Some(v) => v + v1
                                case None    => v1
                            }))

                        }
                        case None | _ => map
                    })
                }
            })
        }
}
