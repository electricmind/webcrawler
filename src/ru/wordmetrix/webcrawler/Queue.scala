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

    var central : Option[V] = None //Vector(List[(String, Double)]())

    val queue = new PriorityQueue[Item]()(
        Ordering.fromLessThan((x: Item, y: Item) => x._1 < y._1))

    //    @tailrec
    //    private 

    val dispatcher: Actor = Actor.actor {
        def dispatch(webgets: immutable.Queue[WebGet], seeds: immutable.Queue[WebCrawler.Seed]): Unit = {
            log("Dispatch:%s %s",webgets.size,seeds.size)
            Actor.react {
                case seed: WebCrawler.Seed => {
                    this.debug("%s",seed)
                    webgets match {
                        case immutable.Queue() => dispatch(webgets, seeds.enqueue(seed))
                        case webgets => webgets.dequeue match {
                            case (webget, webgets) => {
                                webget ! seed
                                dispatch(webgets, seeds)
                            }
                        }
                    }
                }

                case None => {
                    this.debug("%s",None)
                    seeds match {
                        case immutable.Queue() => this ! None
                        case _                 =>
                    }
                    dispatch(webgets, seeds)
                }
                case webget: WebGet => {
                    this.debug("%s",webget)
                    seeds match {
                        case immutable.Queue() => {
                            this ! None
                            dispatch(webgets.enqueue(webget), seeds)
                        }
                        case seeds => seeds.dequeue match {
                            case (seed, seeds) => {
                                webget ! seed
                                dispatch(webgets, seeds)
                            }
                        }
                    }
                }
            }

        }

        dispatch(immutable.Queue[WebGet](), immutable.Queue[WebCrawler.Seed]())
    }

    def act() = {
        loop {
            react {
                case (seeds: Seeds, seed: WebCrawler.Seed, vector: V) => {
                    log("Get seeds from %s seed", seed)
                    central = central match {
                        case None => Some(vector)
                        case Some(v) => Some(v + vector.normal)
                    }
                    val queueitem = (central.get.normal * vector.normal, seed, seeds, vector)

                    log("Priority: %f",queueitem._1)
                    if (queue.isEmpty) {
                        queue.enqueue(queueitem)
                        dispatcher ! None
                    } else {
                        queue.enqueue(queueitem)
                    }
                }

                case (seed: WebCrawler.Seed) => {
                    this.debug("%s",seed)
                    dispatcher ! seed
                }

                case (webget: WebGet) => {
                    this.debug("%s",webget)
                    dispatcher ! webget
                }

                case None => {
                    this.debug("%s",None)
                    if (!queue.isEmpty) {
                        val (priority, seed, seeds: Set[WebCrawler.Seed], vector) = queue.dequeue()
                        log("Priority of request: %f", priority)
//                        val c = central + vector
//                        central = c

                        //                        println("New vector:  %f".format(c.normal * central.normal))
                        //                        println(seed)
                        //                        println(seeds)
                        storage ! seed 

                        for (seed <- seeds) {
                            this.debug("Sent: %s",seed)
                            dispatcher ! seed
                        }
                    } else {
                        println("Queue was empty")
                    }
                }

            }
        }
    }
}