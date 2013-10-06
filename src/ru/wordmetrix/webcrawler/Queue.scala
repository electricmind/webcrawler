package ru.wordmetrix.webcrawler

import scala.actors.Actor
import com.sun.org.apache.xerces.internal.util.URI
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable
import scala.annotation.tailrec
/* 
 * Queue is a system of estimation of priorities and queueing task for webget
 */
class Queue(storage: Storage)(implicit cfg: CFG) extends Actor {
    type V = Vector[String]
    type Seeds = Set[WebCrawler.Seed]
    type Item = (Double, WebCrawler.Seed, Seeds, V)

    var central = Vector(List[(String, Double)]())

    val queue = new PriorityQueue[Item]()(
        Ordering.fromLessThan((x: Item, y: Item) => x._1 > y._1))

    //    @tailrec
    //    private 

    val dispatcher: Actor = Actor.actor {
        def dispatch(webgets: immutable.Queue[WebGet], seeds: immutable.Queue[WebCrawler.Seed]): Unit = {
            Actor.react {
                case seed: WebCrawler.Seed => {
                    println("Dispatch: " + seed)
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
                    println("dispatch None")
                    seeds match {
                        case immutable.Queue() => this ! None
                        case _                 =>
                    }
                    dispatch(webgets, seeds)
                }
                case webget: WebGet => {
                    println("dispatch: " + webget)
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
                    println("Get seeds")
                    val queueitem = (central.normal * vector.normal, seed, seeds, vector)

                    if (queue.isEmpty) {
                        queue.enqueue(queueitem)
                        dispatcher ! None
                    } else {
                        queue.enqueue(queueitem)
                    }
                }

                case (seed: WebCrawler.Seed) => {
                    println("Get: " + seed)
                    dispatcher ! seed
                }

                case (webget: WebGet) => {
                    println("Get: " + webget)
                    dispatcher ! webget
                }

                case None => {
                    println("Get NONE")
                    if (!queue.isEmpty) {
                        val (_, seed, seeds: Set[WebCrawler.Seed], vector) = queue.dequeue()
                        val c = central + vector

                        //                        println("New vector:  %f".format(c.normal * central.normal))
                        //                        println(seed)
                        //                        println(seeds)
                        central = c

                        for (seed <- seeds) {
                            println("Sent: " + seed)
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