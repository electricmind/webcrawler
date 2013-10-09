package ru.wordmetrix.webcrawler
/*
 * LinkContext extracts a context of links from xml page as feature-vector
 */
import WebCrawler.Seed
object LinkContext {
    object Feature {
        def apply(x: String) = x.split("=") match {
            case Array("id", v)    => new FeatureId(v)
            case Array("class", v) => new FeatureClass(v)
            case Array("name", v)  => new FeatureName(v)
        }
    }
    abstract class Feature {
        val order: Int
        val x: String
    }
    class FeatureId(val x: String) extends Feature {
        val order = 1
        override def toString = """id="%s"""".format(x)
    }

    class FeatureClass(val x: String) extends Feature {
        val order = 2
        override def toString = """class="%s"""".format(x)
    }

    class FeatureName(val x: String) extends Feature {
        val order = 3
        override def toString = """class="%s"""".format(x)
    }
    implicit val featureOrdering = Ordering.fromLessThan[Feature]((x, y) => {
        if (x.order > y.order) {
            true
        } else if (x.order < y.order) {
            false
        } else x.x > y.x
    })

}

class LinkContext {
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
                    val l: List[Feature] = (x.attribute("id") match {
                        case Some(x) => Some(new FeatureId(x.toString))
                        case None    => None
                    }) :: (x.attribute("class") match {
                        case Some(x) => x.toString.split(" ").map(x =>
                            Some(new FeatureClass(x))).toList
                        case None => List[Option[Feature]]()
                    }) flatten;

                    val v1 = v + Vector(l.map(x => x -> 1.0))
                    extract(x, v1, x.attribute("href") match {
                        case Some(ref) => map + (new Seed(ref.toString) -> v1)
                        case None      => map
                    })
                }
            })
        }
}
