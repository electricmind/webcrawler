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

    def props(webgetqueue: Props, cfg: CFG, active: Boolean = false): Props =
        Props(if (active) new SeedQueueActive(webgetqueue)(cfg) else new SeedQueue(webgetqueue)(cfg))
}

class SeedQueueActive(webgetprops: Props)(implicit cfg: CFG)
        extends SeedQueue(webgetprops) {

    import SeedQueue._
    import scala.collection.immutable.Queue

    //    context.become(active(Queue[SeedQueueRequest](),cfg.servers), false)
/*
    override def active(queue: Queue[SeedQueueRequest], source: ActorRef, n: Int): Receive = {
        case msg @ SeedQueueRequest(seed, gather) => if (n > 0) {
            queue.enqueue(msg).dequeue match {
                case (msg @ SeedQueueRequest(seed, gather), queue) =>
                    webget() ! msg
                    context.become(active(queue, source, n - 1), false)
            }
        } else {
            context.become(active(queue.enqueue(msg), source, n), false)
        }

        case msg @ SeedQueueGet =>
            if (queue.isEmpty) {
                sender ! SeedQueueEmpty
                source ! msg
                context.become(active(queue, source, n + 1), false)
            } else queue.dequeue match {
                case (msg @ SeedQueueRequest(seed, gather), queue) => {
                    sender ! msg
                    context.become(active(queue, source, n), false)
                }
            }

        case SeedQueueAvailable if (n < cfg.servers) =>
            sender ! SeedQueueGet

    } */
}

class SeedQueue(webgetprops: Props)(
    implicit val cfg: CFG)
        extends Actor with CFGAware {
    override val name = "SeedQueue"

    import SeedQueue._
    import scala.collection.immutable.Queue

    def webget() = context.actorOf(webgetprops)

    override
    def postStop() = {
        this.log("stop")
        for (child <- context.children) {
            child ! PoisonPill
        }
    }
    
    def receive(): Receive = {
        case msg @ SeedQueueRequest(seed, gather) =>
            webget() ! msg
            context.become(
                active(Queue[SeedQueueRequest](), sender, cfg.servers),
                false
            )

    }

    def active(queue: Queue[SeedQueueRequest], source: ActorRef, n: Int): Receive = {
        case msg @ SeedQueueRequest(seed, gather) =>
                                                this.log("Request")

            if (n > 0) {
            queue.enqueue(msg).dequeue match {
                case (msg @ SeedQueueRequest(seed, gather), queue) =>
                    webget() ! msg
                    context.become(active(queue, source, n - 1), false)
            }
            //sender ! SeedQueueGet
        } else {
            context.become(active(queue.enqueue(msg), source, n), false)
        }
        case PoisonPill =>
            this.log("get poison")
            
        case msg @ SeedQueueGet =>
                                    this.log("Get %s",n)

            if (queue.isEmpty) {
                sender ! SeedQueueEmpty
                source ! msg
                context.become(active(queue, source, n + 1), false)
            } else queue.dequeue match {
                case (msg @ SeedQueueRequest(seed, gather), queue) => {
                    sender ! msg
                    context.become(active(queue, source, n), false)
                }
            }

        case SeedQueueAvailable if (n > 0) =>
                        this.log("Available")

            sender ! SeedQueueGet
            
        case msg =>
            this.log("Unknown message: %s", msg)

    }
}
