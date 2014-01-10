package ru.wordmetrix.webcrawler

import java.net.URI

import akka.actor.{ Actor, ActorRef, Props, actorRef2Scala }
import ru.wordmetrix.utils.{ CFG, CFGAware }
/*
 * SeedQueue contains Queue of seeds queued to download.
 */

object SeedQueue {
    abstract sealed trait SeedQueueMessage
    case class SeedQueueRequest(seed: URI) extends SeedQueueMessage

    // case class SeedQueuePostpone(seed: URI) extends SeedQueueSeed(seed)
    //case class SeedQueueAck extends SendQueueMessage

    case class SeedQueueEmpty extends SeedQueueMessage
    case class SeedQueueGet extends SeedQueueMessage

    def props(webgetqueue: Props, cfg: CFG): Props =
        Props(new SeedQueue(webgetqueue)(cfg))
}

class SeedQueue(webgetqueueprops: Props)(
    implicit val cfg: CFG)
        extends Actor with CFGAware {
    override val name = "Gather"

    import SeedQueue._
    import scala.collection.immutable.Queue

    def webget() = context.actorOf(webgetqueueprops)

    def receive(): Receive = {
        case msg @ SeedQueueRequest(seed) =>
            webget() ! msg
            context.become(
                active(Queue[URI](), sender, cfg.servers),
                false
            )
    }

    def active(queue: Queue[URI], source: ActorRef, n: Int): Receive = {
        case SeedQueueRequest(seed) => if (n > 0) {
            queue.enqueue(seed).dequeue match {
                case (seed, qu) =>
                    webget() ! SeedQueueRequest(seed)
                    context.become(active(qu, source, n - 1), false)
            }
        } else {
            context.become(active(queue.enqueue(seed), source, n), false)
        }

        case SeedQueueGet =>
            if (queue.isEmpty) {
                sender ! SeedQueueEmpty
                source ! SeedQueueEmpty
                context.become(active(queue, source, n + 1), false)
            } else queue.dequeue match {
                case (seed, qu) => {
                    sender ! SeedQueueRequest(seed)
                    context.become(active(qu.enqueue(seed), source, n), false)
                }
            }
    }
}
