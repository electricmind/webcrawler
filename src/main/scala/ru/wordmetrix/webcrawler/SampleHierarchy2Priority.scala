/**
 *
 */
package ru.wordmetrix.webcrawler

import scala.collection.mutable

import Gather.GatherLinkContext
import akka.actor.{Actor, Props}
import ru.wordmetrix.smartfile.SmartFile.fromFile
import ru.wordmetrix.utils.{CFG, CFGAware}
import ru.wordmetrix.vector.Vector
import ru.wordmetrix.webcrawler.LinkContext.Feature

/**
 * @author cray
 *
 */
object SampleHierarchy2Priority extends SampleHirarchy2PriorityBase {

  case class SampleHirarchy2PriorityPriority(seed: Seed, priority: Priority)
    extends SampleHirarchy2PriorityMessage

  def props(cfg: CFG): Props =
    Props(new SampleHierarchy2Priority()(cfg))

}

class SampleHierarchy2Priority()(implicit val cfg: CFG)
  extends Actor with CFGAware with SampleHierarchy2PriorityBase {
  override val name = "SampleH2P"

  import Gather._
  import SampleHierarchy2Priority._

  val priorities = mutable.Map[Seed, Priority]()
  val vectors = mutable.Map[Seed, Set[Vector[Feature]]]()

  //TODO: remove nseeds iterator
  val nseeds = Iterator.from(1)

  override def receive(): Receive = super.receive orElse {
    case GatherLinkContext(seed, map) => {
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
    case SampleHirarchy2PriorityPriority(seed, priority) => {
      val nseed = nseeds.next
      this.log("Get priority, %s", nseed)
      if (vectors contains seed) {
        priorities += (seed -> priority)
        if (nseed % 100 == 0) {
          val dump = cfg.sampling.writer

          val N = Iterator.from(1)
          val seed2N = mutable.Map[String, Int]()
          val count = mutable.Map[String, Int]()

          for {
            (seed, priority) <- priorities.toList.sortBy(_._2)
            record <- vectors.get(seed)
            vector <- record
            feature <- vector.toMap.keySet.map(_.toString)
          } {
            count += (feature -> (count.getOrElse(feature, 0) + 1))
          }

          for {
            (seed, priority) <- priorities.toList.sortBy(_._2)
            record <- vectors.get(seed)
            vector <- record
          } {
            // vector.toList().
            val map: Map[String, Double] = vector.toMap.map({
              case (x, y) => x.toString -> y
            }).filter(
                x => count(x._1) > 10).withDefaultValue(0.0)
            //dump.write("%10s : %s\n".format(priority, vector.map(_._1.toString).filter(! _.startsWith("class=\"page"))).getBytes)
            //dump.write("%s %s\n".format(seed2N,map -- seed2N.keySet).getBytes)

            dump.write("%8.2f : %s\n".format(priority, {
              seed2N.toList.sortBy(_._2).map(_._1).map(map) ++
                map.filterNot(x => seed2N.contains(x._1)).
                  map({
                  case (x, y) => {
                    seed2N += (x -> N.next)
                    y
                  }
                })
            }.map(x => "%2.0f".format(x)).mkString(" ")))

            dump.write("%s\n".format(seed2N.size))
          }
          dump.write(
            seed2N.toList.sortBy(_._2).map(_._1).mkString(" ")
          )
          dump.write(("\n"))
          dump.close()
        }
      }
    }
  }

}
 