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
    case class SeedQueueRequest(seed: URI, gather: ActorRef) extends SeedQueueMessage

    // case class SeedQueuePostpone(seed: URI) extends SeedQueueSeed(seed)
    //case class SeedQueueAck extends SendQueueMessage

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
    import EvaluatePriorityMatrix._
    import scala.collection.immutable.Queue

    def webget() = context.actorOf(webgetprops)

    def receive(): Receive = {
        case msg @ SeedQueueRequest(seed, gather) =>
            webget() ! msg
            context.become(
                active(Queue[SeedQueueRequest](), sender, gather, cfg.servers),
                false
            )
    }

    def finit(queue: Queue[SeedQueueRequest], source: ActorRef, gather : ActorRef, n: Int) : Receive = {
        case msg @ SeedQueueGet =>
            this.log("Get %s in finit", n)
            if (queue.isEmpty) {
               if (n+1 == cfg.servers) {
                   gather ! EvaluatePriorityMatrixStop
                   context.stop(self)
               }
               context.become(finit(queue, source, gather, n+1), false)
            } else queue.dequeue match {
                case (msg @ SeedQueueRequest(seed, gather), queue) => {
                    sender ! msg
                    context.become(finit(queue, source, gather, n), false)
                }
            }

        
    }
    
    def active(queue: Queue[SeedQueueRequest], source: ActorRef, gather : ActorRef, n: Int): Receive = {
        case EvaluatePriorityMatrixStop =>
            context.become(finit(Queue(), source, gather, n),false)
            
        //TODO: gather can be moved here            
        case msg @ SeedQueueRequest(seed, gather) =>
            this.log("Request")

            if (n > 0) {
                queue.enqueue(msg).dequeue match {
                    case (msg @ SeedQueueRequest(seed, gather), queue) =>
                        webget() ! msg
                        context.become(active(queue, source, gather, n - 1), false)
                }
                //sender ! SeedQueueGet
            } else {
                context.become(active(queue.enqueue(msg), source, gather,  n), false)
            }

        case msg @ SeedQueueGet =>
            this.log("Get %s", n)

            if (queue.isEmpty) {
                sender ! SeedQueueEmpty
                source ! msg
                context.become(active(queue, source, gather, n + 1), false)
            } else queue.dequeue match {
                case (msg @ SeedQueueRequest(seed, gather), queue) => {
                    sender ! msg
                    context.become(active(queue, source, gather, n), false)
                }
            }

        case SeedQueueAvailable if (n > 0) =>
            this.log("Available")

            sender ! SeedQueueGet

        case msg =>
            this.log("Unknown message: %s", msg)

    }
}
