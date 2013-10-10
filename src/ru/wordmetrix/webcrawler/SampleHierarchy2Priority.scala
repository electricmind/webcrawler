/**
 *
 */
package ru.wordmetrix.webcrawler

import scala.actors.Actor
import WebCrawler._
import scala.collection.mutable
import ru.wordmetrix.webcrawler.LinkContext.Feature
import ru.wordmetrix.webcrawler.ActorDebug._
import java.io.FileOutputStream
import java.io.BufferedOutputStream

/**
 * @author cray
 *
 */
class SampleHierarchy2Priority()(implicit val cfg: CFG)
        extends Actor with CFGAware {
    override val name = "SampleH2P"
    val priorities = mutable.Map[Seed, Priority]()
    val vectors = mutable.Map[Seed, Set[Vector[Feature]]]()
    var r = 0
    def act() = {
        loop {
            react {
                case map: Map[Seed, Vector[Feature]] => {
                    this.log("Get map of vectors")
                    for ((seed, vector) <- map) {
                        vectors += (seed -> (vectors.get(seed) match {
                            case Some(vectors) => {
                                vectors + vector
                            }
                            case None          => Set(vector)
                        }))
                    }
                }
                case (seed: Seed, priority: Priority) => {
                    this.log("Get priority, %s", r)
                    if (vectors contains seed) {
                        priorities += (seed -> priority)
                        r = r + 1
                        if (r % 100 == 0) {
                            val dump = new BufferedOutputStream(new FileOutputStream(cfg.sampling))

                            for {
                                (seed, priority) <- priorities.toList.sortBy(_._2)
                                record <- vectors.get(seed)
                                vector <- record
                            } {
                                dump.write("%10s : %s\n".format(priority, vector.map(_._1.toString).filter(! _.startsWith("class=\"page"))).getBytes)
                            }
                            dump.close()
                        }
                    }
                }
            }

        }
    }
}