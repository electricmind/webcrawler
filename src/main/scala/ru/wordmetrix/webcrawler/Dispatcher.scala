package ru.wordmetrix.webcrawler

import scala.actors.Actor
import scala.collection.immutable

import ru.wordmetrix.utils.{CFG, CFGAware, log}
import ru.wordmetrix.utils.ActorDebug.actor2ActorDebug

class Dispatcher(queue: Actor)(implicit val cfg: CFG) extends Actor
        with CFGAware {
    override val name = "Dispatcher"
    def dispatch(webgets: immutable.Queue[WebGet],
                 seeds: immutable.Queue[WebCrawler.Seed]): Unit = {
        this.debug("Queues, chickens = %s, seeds = %s", webgets.size, seeds.size)

        react {
            case seed: WebCrawler.Seed => {
                this.debug("Seed %s is offered to download", seed)
                webgets match {
                    case immutable.Queue() =>
                        dispatch(webgets, seeds.enqueue(seed))

                    case webgets => webgets.dequeue match {
                        case (webget, webgets) => {
                           // webget ! seed
                            if (seeds.isEmpty) {
                                queue ! None
                            }
                            dispatch(webgets, seeds)
                        }
                    }
                }
            }

            case None => {
                this.debug("None message has come")
                seeds match {
                    case immutable.Queue() => queue ! None
                    case _                 =>
                }
                dispatch(webgets, seeds)
            }

            case webget: WebGet => {
                this.debug("WebGet has come")
                seeds match {
                    case immutable.Queue() => {
                        //                            this ! None
                        dispatch(webgets.enqueue(webget), seeds)
                    }
                    case seeds => seeds.dequeue match {
                        case (seed, seeds) => {
                            if (seeds.isEmpty) {
                                log("seeds empty")
                                queue ! None
                            }
                            //webget ! seed
                            dispatch(webgets, seeds)
                        }
                    }
                }
            }
        }

    }

    def act() = dispatch(immutable.Queue[WebGet](),
        immutable.Queue[WebCrawler.Seed]())
}
