package ru.wordmetrix.webcrawler

import java.net.URI

import akka.actor.{ Actor, ActorRef, Props, actorRef2Scala }
import ru.wordmetrix.utils.{ CFG, CFGAware }
/*
 * SeedQueue contains Queue of seeds queued to download.
 */

object SeedQueue {
    abstract sealed trait SeedQueueMessage
    case class SeedQueueRequest(seed: URI, gather : ActorRef) extends SeedQueueMessage

    // case class SeedQueuePostpone(seed: URI) extends SeedQueueSeed(seed)
    //case class SeedQueueAck extends SendQueueMessage

    case class SeedQueueEmpty extends SeedQueueMessage
    case class SeedQueueGet extends SeedQueueMessage

    def props(webgetqueue: Props, cfg: CFG): Props =
        Props(new SeedQueue(webgetqueue)(cfg))
}

class SeedQueue(webgetprops: Props)(
    implicit val cfg: CFG)
        extends Actor with CFGAware {
    override val name = "Gather"

    import SeedQueue._
    import scala.collection.immutable.Queue

    def webget() = context.actorOf(webgetprops)

    def receive(): Receive = {
        case msg @ SeedQueueRequest(seed, gather) =>
            webget() ! msg
            context.become(
                active(Queue[SeedQueueRequest](), sender, cfg.servers),
                false
            )
    }

    def active(queue: Queue[SeedQueueRequest], source: ActorRef, n: Int): Receive = {
        case msg @ SeedQueueRequest(seed,gather) => if (n > 0) {
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
    }
}
