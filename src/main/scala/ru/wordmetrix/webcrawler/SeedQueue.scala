package ru.wordmetrix.webcrawler

import java.net.URI

import scala.collection.immutable.Queue

import EvaluatePriorityMatrix.EvaluatePriorityMatrixStop
import WebGet.WebGetRequest
import akka.actor.{Actor, ActorRef, Props, actorRef2Scala}
import ru.wordmetrix.utils.{CFG, CFGAware}

/*
 * SeedQueue contains Queue of seeds queued to download.
 */

object SeedQueue {
    abstract sealed trait SeedQueueMessage
    case class SeedQueueRequest(seed: URI) extends SeedQueueMessage
    case class SeedQueueLink(gather: ActorRef) extends SeedQueueMessage
    case object SeedQueueEmpty extends SeedQueueMessage
    case object SeedQueueAvailable extends SeedQueueMessage
    case object SeedQueueGet extends SeedQueueMessage

    def props(webgetqueue: Props, cfg: CFG): Props =
        Props(new SeedQueue(webgetqueue)(cfg))
}

class SeedQueue(webgetprops: Props)(
    implicit val cfg: CFG)
        extends Actor with CFGAware {
    override val name = "SeedQueue"

    import SeedQueue._
    import WebGet._
    import EvaluatePriorityMatrix._
    import scala.collection.immutable.Queue

    def webget() = context.actorOf(webgetprops)

    def receive(): Receive = {
        case SeedQueueLink(gather) =>
            context.become(active(Queue(), gather, cfg.servers))
    }

    def finit(queue: Queue[SeedQueueRequest], gather: ActorRef,
              n: Int): Receive = {
        case msg @ SeedQueueGet =>
            this.log("Get %s in finit", n)
            if (queue.isEmpty) {
                debug("Servers are vacant: %s", n)

                if (n + 1 == cfg.servers) {
                    debug("Command gather to stop")
                    gather ! EvaluatePriorityMatrixStop
                    context.stop(self)
                }
                context.become(finit(queue, gather, n + 1))
            } else queue.dequeue match {
                case (msg @ SeedQueueRequest(seed), queue) => {
                    sender ! msg
                    context.become(finit(queue, gather, n))
                }
            }
    }

    def exhaust(queue: Queue[SeedQueueRequest], gather: ActorRef,
               n: Int): Receive = {
       
        case msg @ SeedQueueGet =>
            if (queue.isEmpty) {
                sender ! SeedQueueEmpty
                if (n + 1 == cfg.servers) {
                    context.parent ! msg
                    gather ! EvaluatePriorityMatrixStopTargeting
                    context.become(active(queue, gather, n + 1))
                } else {
                    context.become(exhaust(queue, gather, n + 1))
                }
            } else queue.dequeue match {
                case (msg @ SeedQueueRequest(seed), queue) => {
                    sender ! WebGetRequest(seed, gather)
                    context.become(exhaust(queue, gather, n))
                }
            }
        
        
    }
    def active(queue: Queue[SeedQueueRequest], gather: ActorRef,
               n: Int): Receive = {
        case EvaluatePriorityMatrixStopTargeting =>
            context.become(exhaust(queue, gather, n))
            
        case EvaluatePriorityMatrixStop =>
            debug("Enter into finit state %s",n)
            if (n == cfg.servers) {
                    debug("Command gather to stop 1")
                    gather ! EvaluatePriorityMatrixStop
                    context.stop(self)
            } else {
               context.become(finit(Queue(), gather, n))
            }
        case msg @ SeedQueueRequest(seed) =>
            if (n > 0) {
                queue.enqueue(msg).dequeue match {
                    case (msg @ SeedQueueRequest(seed), queue) =>
                        webget() ! WebGetRequest(seed, gather)
                        context.become(active(queue, gather, n - 1))
                }
            } else {
                context.become(active(queue.enqueue(msg), gather, n))
            }

        case msg @ SeedQueueGet =>
            if (queue.isEmpty) {
                sender ! SeedQueueEmpty
                context.parent ! msg
                context.become(active(queue, gather, n + 1))
            } else queue.dequeue match {
                case (msg @ SeedQueueRequest(seed), queue) => {
                    sender ! WebGetRequest(seed, gather)
                    context.become(active(queue, gather, n))
                }
            }

        case SeedQueueAvailable if (n > 0) =>
            this.log("Available")
            sender ! SeedQueueGet

    }
}
