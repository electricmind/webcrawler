package ru.wordmetrix.webcrawler
/**
 * An actor that evaluates priorities of links on the base of content of pages
 * that contains these links
 *
 * @author Elec
 * @version 1.1
 */
import java.net.URI

import scala.collection.mutable.PriorityQueue

import Gather.GatherLink
import SampleHierarchy2Priority.SampleHirarchy2PriorityPriority
import SeedQueue.{ SeedQueueAvailable, SeedQueueGet, SeedQueueLink, SeedQueueRequest }
import Storage.{ StorageSign, StorageVictim }
import WebCrawler.{ Seed, Word }
import akka.actor.{ Actor, Props, actorRef2Scala }
import ru.wordmetrix.utils.{ CFG, CFGAware }
//     import scalaz._
//     import std.option._, std.list._

object EvaluatePriorityMatrix {
    abstract sealed class EvaluatePriorityMatrixMessage

    case class EvaluatePriorityMatrixSeed(seed: URI)
        extends EvaluatePriorityMatrixMessage

    case class EvaluatePriorityMatrixStop extends EvaluatePriorityMatrixMessage
    object PQ {
        import scalaz._
        import std.option._, std.list._
        def apply[U]()(implicit o: scalaz.Order[U]) = Heap.fromData(List[U]())
        def apply[U](i1: U)(implicit o: scalaz.Order[U]) = Heap.fromData(List[U](i1))
        def apply[U](x1: U, x2: U, xs: U*)(implicit f: scalaz.Foldable[List], o: scalaz.Order[U]) =
            xs.foldLeft(Heap.fromData(List(x1, x2))) {
                case (heap, x) => heap.insert(x)
            }
        def apply[U](i1: U, heap: Heap[U])(implicit o: scalaz.Order[U]) = heap.insert(i1)
        def unapply[U](heap: Heap[U]) =
            if (heap.isEmpty) None else Some((heap.minimum, heap.deleteMin))
    }
    /**
     * Define an EvaluatePriorityMatrix
     *
     * @param storage    A storage for pages;
     * @param gather     An actor that elicits data from pages;
     * @param seedqueue  A dispatcher of requests;
     * @param sample     An actor that maintains a sample of mapping content of
     *                   links to priorities;
     * @param cfg        A configure object;
     * @return           An props for EvaluatePriorityMatrix.
     */
    def props(storage: Props, gather: Props, seedqueue: Props, sample: Props,
              cfg: CFG): Props =
        Props(
            new EvaluatePriorityMatrix(storage, gather, seedqueue, sample)(cfg))
}

class EvaluatePriorityMatrix(storageprop: Props,
                             gatherprop: Props,
                             seedqueueprop: Props,
                             sampleprop: Props)(implicit cfg: CFG) extends Actor
        with CFGAware {
    override val name = "Evaluate . Matrix"

    type Priority = Double
    type V = ru.wordmetrix.vector.Vector[Word]
    type Item = (Priority, Seed)
    type VItem = (Priority, V)

    import EvaluatePriorityMatrix._
    val ns = Iterator.from(1)

    val queue = new PriorityQueue[Item]()(
        Ordering.fromLessThan((x: Item, y: Item) => x._1 < y._1))

    val gather = context.actorOf(gatherprop, "Gather")

    val seedqueue = context.actorOf(seedqueueprop, "SeedQueue")

    val storage = context.actorOf(storageprop, "Storage")

    val sample = context.actorOf(sampleprop, "Sample")

    gather ! GatherLink(storage, sample)

    seedqueue ! SeedQueueLink(gather)

    storage ! StorageVictim(seedqueue)

    var factor = new V(List())

    var newfactor = new V(List())

    var target = new TargetVector[String](n = cfg.targets)

    var average = new AverageVector[String]()

    var limit = 0.90

    var priorities = Map[Seed, (Priority, Set[Seed])]()

    var vectors = Map[Seed, (V, Set[Seed])]()

    /*
     * Calculate priorities all of the seeds that are in queue by multiplying 
     * factor of new seed on all of the seeds that sourced from it.
     * 
     */
    def calculate(factor: V, vectors: Map[Seed, (V, Set[Seed])]) =
        vectors.map({
            case (seed, (vector, seeds)) => {
                val priority = vector * factor.normal
                seeds.map((_, priority))
            }
        }).flatten.groupBy(x => x._1).map({
            case (seed, ps) =>
                seed -> ps.map(_._2)
        }).map({
            case (seed, ps) =>
                seed -> ((combinepolicy(ps), priorities(seed)._2))
        })

    def combinepolicy(priorities: Iterable[Double]) = priorities.max

    var amount = 4

    var central = new V(List())

    def enqueue(seeds: Set[Seed], seed: Seed, v: V) = time("enqueue") {
        vectors = vectors + (seed -> (v, seeds))
        for (item <- seeds) {
            val qq = (
                item -> (
                    (combinepolicy(
                        priorities.getOrElse(
                            item,
                            (0d, Set[Seed]())
                        )._2.map(
                                x => vectors(x)._1 * factor) + v * factor
                    ),
                        priorities.getOrElse(item, (0d, Set[Seed]()))._2 + seed
                    )
                )
            )
            priorities = priorities + qq
        }

        queue.clear()

        for ((seed, (p, seeds)) <- priorities) {
            queue.enqueue((p, seed))
        }
    }
    /**
     * Estimate how seed was relevant
     *
     * @return target, average, (new)factor
     */
    def estimate(seed: Seed, v: V) = time(
        "estimate, average size: %s, target size: %s / %s".format(
            average.vector.size, target.average.vector.size, target.vs.length)) {
            average = average + v
            target = target + (v, {
                debug("accepted %s with %s in %s", seed,
                    v * target.average.normal, target.priority())
                storage ! StorageSign(seed)
            })

            newfactor = target.normal - average.normal

            debug("direction = %s %s",
                (target.normal - average.normal) * central, factor * central)

            debug("limit? %s < %s",
                newfactor.normal * factor.normal, limit
            )
        }

    def receive(): Receive = {
        case EvaluatePriorityMatrixSeed(seed: Seed) => {
            log("Initial seed: %s", seed)
            seedqueue ! SeedQueueRequest(seed)
            context.become(phase_initialization, false)
        }
    }

    def phase_initialization(): Receive = {

        case msg @ Gather.GatherLinkContext(_, _) => sample ! msg

        case Gather.GatherSeeds(seed, seeds, v) => {
            ns.next()
            //Initialization
            val v1 = v.normal
            log("Initial phase, v = %s, n = %s, seed = %s",
                v.norm, seeds.size, seed)
            central = v1
            target = target + v1
            average = average + v1

            // I need it only to do deterministic test
            for (seed <- seeds.toList.sorted) {
                seedqueue ! SeedQueueRequest(seed)
            }

            storage ! StorageSign(seed)
            log("Start targeting " + target.vs.length)
            context.become(phase_targeting, false)
        }
    }

    def phase_targeting(): Receive = {
        //Accumulate initial vectors for target
        case SeedQueueGet => {
            log("Targeting impossible, too little casualties")
            //TODO: Initial SEED can contain too little links for estimation phase
        }

        case Gather.GatherSeeds(seed, seeds, v) => {
            debug(s"Targeting phase $seed")
            ns.next()

            estimate(seed, v.normal)
            log("Targeting phase, attitude = %s, n = %s, " +
                "seed = %s",
                newfactor * central, seeds.size, seed)

            debug("target - average| = %s",
                (target.normal - average.normal).norm)

            debug("target*central = %s",
                target.normal * central)

            factor = newfactor
            debug("factor*central = %s (required %s)",
                factor * central, cfg.targeting)

            enqueue(seeds, seed, v)
            queue.clear() //TODO: Why?

            if (factor * central > cfg.targeting) {
                priorities = calculate(newfactor, vectors)
                //target = new TargetVectorCluster[String](target, n = cfg.targets)

                log("Turn into estimation phase")
                seedqueue ! SeedQueueAvailable
                context.become(phase_estimating, false)
            }
        }

    }

    def phase_estimating(): Receive = {
        case EvaluatePriorityMatrixStop =>
            context.system.shutdown()

        case Gather.GatherSeeds(seed, seeds, v) => {
            if (ns.next() > cfg.limit) {
                log("Limit has been reached")
                sample ! EvaluatePriorityMatrixStop
                seedqueue ! EvaluatePriorityMatrixStop
            } else {
                log("Estimating phase,  attitude = %s, priority = %s, seed = %s",
                    newfactor * central, factor * v.normal, seed)
                estimate(seed, v.normal)
                if (newfactor.normal * factor.normal < limit) {
                    debug("Priorities should be recalculated")
                    priorities = calculate(newfactor, vectors)
                    factor = newfactor
                }

                enqueue(seeds, seed, v)
                sample ! SampleHirarchy2PriorityPriority(seed, factor * v.normal)
                seedqueue ! SeedQueueAvailable
            }
        }

        case SeedQueueGet => {
            debug("Get dispather request %s", sender)
            if (!queue.isEmpty) {
                val (p, seed) = queue.dequeue
                val (_, seeds) = priorities(seed)
                priorities = priorities - seed
                vectors = vectors ++ {
                    seeds.filter(vectors contains _).map(x => x -> {
                        val (vector, seeds) = vectors(x)
                        (vector, seeds - seed)
                    })
                }
                log("Request, priority = %s for %s : %s", p,
                    seed, seeds.headOption.getOrElse("empty"))
                //TODO: check continue of estimation  phase
                sender ! SeedQueueRequest(seed)
            } else {
                debug("Queue was empty")
            }
        }
        case x => debug("!!!! Unknown message %s from %s",
            x, sender)

    }
}
