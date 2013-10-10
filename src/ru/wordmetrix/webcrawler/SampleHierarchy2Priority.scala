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
    var nseeds = Iterator.from(1)
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
                            case None => Set(vector)
                        }))
                    }
                }
                case (seed: Seed, priority: Priority) => {
                    val nseed = nseeds.next
                    this.log("Get priority, %s", nseed)
                    if (vectors contains seed) {
                        priorities += (seed -> priority)
                        if (nseed % 250 == 0) {
                            val dump = new BufferedOutputStream(new FileOutputStream(cfg.sampling))
                            val N = Iterator.from(1)
                            val seed2N = mutable.Map[String, Int]()

                            for {
                                (seed, priority) <- priorities.toList.sortBy(_._2)
                                record <- vectors.get(seed)
                                vector <- record
                            } {

                                //dump.write("%10s : %s\n".format(priority, vector.map(_._1.toString).filter(! _.startsWith("class=\"page"))).getBytes)
                                //dump.write("%s %s\n".format(seed2N,vector.toMap -- seed2N.keySet).getBytes)
                                
                                dump.write("%8.2f : %s\n".format(priority, {
                                    val map : Map[String,Double] = vector.toMap.map({case (x,y) => x.toString -> y}).withDefaultValue(0.0)
                                    
                                    seed2N.toList.sortBy(_._2).map(_._1).map(map) ++
                                        map.filterNot( x => seed2N.contains(x._1)).map({ case (x, y) => { seed2N += (x -> N.next); y } })
                                }.map(x => "%2.0f".format(x)).mkString(" ")).getBytes)
                                
                                dump.write("%s\n".format(seed2N.size).getBytes)
                            }
                            dump.close()
                        }
                    }
                }
            }

        }
    }
}