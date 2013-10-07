package ru.wordmetrix.webcrawler

import scala.actors.Actor
import com.sun.org.apache.xerces.internal.util.URI
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable
import scala.annotation.tailrec
import ActorDebug._
/* 
 * Queue is a system of estimation of priorities and queueing task for webget
 */
class Queue(storage: Storage)(implicit cfg: CFG) extends Actor {
    type V = Vector[String]
    type Seeds = Set[WebCrawler.Seed]
    type Item = (Double, WebCrawler.Seed, Seeds, V)
    //TODO: remove qq
    var qq = 4
    var central: Option[V] = None //Vector(List[(String, Double)]())

    var average = Vector(List[(String, Double)]())

    def priorityCentral(v: V) = {
        central.get.normal * v.normal
    }

    def priorityDiffer(v: V) = {
        v.normal * (central.get.normal - average.normal)
    }

    val queue = new PriorityQueue[Item]()(
        Ordering.fromLessThan((x: Item, y: Item) => x._1 < y._1))

    val dispatcher = new Dispatcher(this) {
        start
    }
    
    def act() = {
        loop {
            react {
                case (seeds: Seeds, seed: WebCrawler.Seed, vector: V) => {
                    log("Get seeds from %s seed", seed)
                    central = central match {
                        case None    => Some(vector)
                        case Some(v) => Some(v)
                    }

                    average = average + vector

                    val queueitem = (priorityDiffer(vector), seed, seeds, vector)

                    log("Priority: %f (%s)", queueitem._1, queueitem._2)
                    if (queue.isEmpty) {
                        queue.enqueue(queueitem)
                        dispatcher ! None
                    } else {
                        queue.enqueue(queueitem)
                    }

                }

                case (seed: WebCrawler.Seed) => {
                    this.debug("%s", seed)
                    dispatcher ! seed
                }

                case (webget: WebGet) => {
                    this.debug("%s", webget)
                    dispatcher ! webget
                }

                case None => {
                    this.debug("%s", None)
                    if (!queue.isEmpty) {
                        val (priority, seed, seeds: Set[WebCrawler.Seed], vector) = queue.dequeue()
                        log("Priority of request: %f (%s)", priority, seed)
                        val c = central match {
                            case None    => vector
                            case Some(v) => v + vector.normal
                        }
                        println("New vector:  %f".format((c.normal - central.get.normal).norm))
                        println("New seeds:  %d".format(seeds.size))
                        central = Some(c)
                        storage ! seed

                        if (seeds.size > 0) {
                            for (seed <- seeds) {
                                this.debug("Sent: %s", seed)
                                dispatcher ! seed
                            }

                            //TODO: Priorities should be recalculated only if central vector deflected from previous state more than difference between them and head of queue
                            if (qq > 0) {
                                qq -= 1
                                log("start calc")
                                val copy = queue.toList.map({ case (priority, seed, seeds, vector) => (priorityDiffer(vector), seed, seeds, vector) })
                                queue.clear
                                queue.enqueue(copy: _*)
                                log("stop calc")
                            }
                        } else {
                            log("Seed %s has no childrens", seed)
                            this ! None
                        }

                    } else {
                        println("Queue was empty")
                    }
                }

            }
        }
    }
}