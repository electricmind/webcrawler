package ru.wordmetrix.webcrawler

import java.net.URI
import akka.actor.{ Actor, ActorRef, Props, actorRef2Scala }
import ru.wordmetrix.utils.{ CFG, CFGAware }
import ru.wordmetrix.utils.ActorDebug.actor2ActorDebug
import akka.actor.PoisonPill

/*
 * SeedQueue contains Queue of seeds queued to download.
 */

object SeedQueue {
    abstract sealed trait SeedQueueMessage
    case class SeedQueueRequest(seed: URI) extends SeedQueueMessage
    case class SeedQueueLink(gather: ActorRef) extends SeedQueueMessage
    case class SeedQueueEmpty extends SeedQueueMessage
    case class SeedQueueAvailable extends SeedQueueMessage
    case class SeedQueueGet extends SeedQueueMessage

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

    

    def receive() : Receive = {
        case SeedQueueLink(gather) => context.become(active(Queue(), gather, cfg.servers))    
    }
    
    def finit(queue: Queue[SeedQueueRequest], gather: ActorRef, n: Int): Receive = {
        case msg @ SeedQueueGet =>
            this.log("Get %s in finit", n)
            if (queue.isEmpty) {
                if (n + 1 == cfg.servers) {
                    gather ! EvaluatePriorityMatrixStop
                    context.stop(self)
                }
                context.become(finit(queue, gather, n + 1), false)
            } else queue.dequeue match {
                case (msg @ SeedQueueRequest(seed), queue) => {
                    sender ! msg
                    context.become(finit(queue, gather, n), false)
                }
            }
    }

    def active(queue: Queue[SeedQueueRequest], gather: ActorRef, n: Int): Receive = {
        case EvaluatePriorityMatrixStop =>
            context.become(finit(Queue(), gather, n), false)

        case msg @ SeedQueueRequest(seed) =>
            this.log("Request")

            if (n > 0) {
                println(1)
                queue.enqueue(msg).dequeue match {
                    case (msg @ SeedQueueRequest(seed), queue) =>
                        webget() ! WebGetRequest(seed, gather)
                        context.become(active(queue, gather, n - 1), false)
                }
            } else {
                context.become(active(queue.enqueue(msg), gather, n), false)
            }

        case msg @ SeedQueueGet =>
            this.log("Get %s %s", n, queue.isEmpty)

            if (queue.isEmpty) {
                sender ! SeedQueueEmpty
                println(context.parent,msg,"QQ")
                context.parent ! msg
                context.become(active(queue, gather, n + 1), false)
            } else queue.dequeue match {
                case (msg @ SeedQueueRequest(seed), queue) => {
                    sender ! WebGetRequest(seed, gather)
                    println(sender,msg)
                    context.become(active(queue, gather, n), false)
                }
            }

        case SeedQueueAvailable if (n > 0) =>
            this.log("Available")

            sender ! SeedQueueGet

        case msg =>
            this.log("Unknown message: %s", msg)

    }
}
