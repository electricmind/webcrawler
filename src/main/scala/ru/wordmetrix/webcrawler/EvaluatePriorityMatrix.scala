package ru.wordmetrix.webcrawler

import java.net.URI

import scala.collection.immutable.SortedSet

import Gather.GatherLink
import SampleHierarchy2Priority.SampleHirarchy2PriorityPriority
import SeedQueue.{ SeedQueueAvailable, SeedQueueGet, SeedQueueLink, SeedQueueRequest }
import Storage.{ StorageSign, StorageVictim }
import WebCrawler.{ Seed, Word }
import akka.actor.{ Actor, Props, actorRef2Scala }
import ru.wordmetrix.utils.{ CFG, CFGAware }

object EvaluatePriorityMatrix {
    abstract sealed class EvaluatePriorityMatrixMessage

    case class EvaluatePriorityMatrixSeed(seed: URI)
        extends EvaluatePriorityMatrixMessage

    case class EvaluatePriorityMatrixStopTargeting
        extends EvaluatePriorityMatrixMessage
    case class EvaluatePriorityMatrixStop extends EvaluatePriorityMatrixMessage

    /**
     * Extension of SortedSet to use as priority queue
     */
    object PQ {
        def apply[U]()(implicit o: Ordering[U]) = SortedSet[U]()
        def apply[U](i1: U)(implicit o: Ordering[U]) = SortedSet[U](i1)
        def apply[U](x1: U, x2: U, xs: U*)(implicit o: Ordering[U]) =
            xs.foldLeft(SortedSet[U](x1, x2)) {
                case (map, x) => map + (x)
            }
        def apply[U](i1: U, map: SortedSet[U])(implicit o: Ordering[U]) = map + (i1)
        def unapply[U](map: SortedSet[U]) =
            map.headOption match {
                case None    => None
                case Some(x) => Some((x, map.tail))
            }
    }

    implicit class PQEx[U](map: SortedSet[U]) {
        def insert(x: U) = map + (x)
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

    val gather = context.actorOf(gatherprop, "Gather")

    val seedqueue = context.actorOf(seedqueueprop, "SeedQueue")

    val storage = context.actorOf(storageprop, "Storage")

    val sample = context.actorOf(sampleprop, "Sample")

    gather ! GatherLink(storage, sample)

    seedqueue ! SeedQueueLink(gather)

    storage ! StorageVictim(seedqueue)

    /*
     * Calculate priorities all of the seeds that are in queue by multiplying 
     * factor of new seed on all of the seeds that sourced from it.
     * 
     */

    def calculate(factor: V, vectors: Map[Seed, (V, Set[Seed])],
                  priorities: Map[Seed, (Priority, Set[Seed])]) =
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

    def enqueue(seeds: Set[Seed], factor: V, seed: Seed, v: V,
                vectors: Map[Seed, (V, Set[Seed])],
                priorities: Map[Seed, (Priority, Set[Seed])]) = {
        val vectors1 = vectors + (seed -> (v, seeds))

        seeds.foldLeft(priorities) {
            case (priorities, seed) =>
                priorities + (
                    seed -> (
                        (combinepolicy(
                            priorities.getOrElse(
                                seed,
                                (0d, Set[Seed]())
                            )._2.map(
                                    x => vectors(x)._1 * factor) + v * factor
                        ),
                            priorities.getOrElse(seed, (0d, Set[Seed]()))._2 + seed
                        )
                    )
                )
        } match {
            case priorities =>
                (vectors, priorities, priorities.foldLeft(PQ[Item]) {
                    case (queue, ((seed, (p, seeds)))) => queue insert (-p, seed)
                })
        }
    }
    /**
     * Estimate how seed was relevant
     *
     * @return target, average, (new)factor
     */
    def estimate(seed: Seed, v: V, target: TargetVector[String],
                 average: AverageVector[String]) = {
        val average1 = average + v
        val target1 = target + (v, {
            debug("accepted %s with %s in %s", seed,
                v * target.average.normal, target.priority())
            storage ! StorageSign(seed)
        })

        (target1, average1, target1.normal - average1.normal)
    }

    def receive(): Receive = {
        case EvaluatePriorityMatrixSeed(seed: Seed) => {
            log("Initial seed: %s", seed)
            seedqueue ! SeedQueueRequest(seed)
            context.become(phase_initialization)
        }
    }

    /**
     * Initialization Phase: download initial page(s)
     */
    def phase_initialization(): Receive = {

        case msg @ Gather.GatherLinkContext(_, _) => sample ! msg

        case Gather.GatherSeeds(seed, seeds, v) => {
            ns.next()
            //Initialization
            val v1 = v.normal
            log("Initial phase, v = %s, n = %s, seed = %s",
                v.norm, seeds.size, seed)

            val central = v1

            val target = new TargetVector[String](n = cfg.targets) + v1
            val average = new AverageVector[String](v1)

            // I need it only to do deterministic test
            for (seed <- seeds.toList.sorted) {
                seedqueue ! SeedQueueRequest(seed)
            }

            seedqueue ! EvaluatePriorityMatrixStopTargeting
            storage ! StorageSign(seed)

            log("Start targeting " + target.vs.length)
            context.become(phase_targeting(central, target, average,
                Map[Seed, (V, Set[Seed])](),
                Map[Seed, (Priority, Set[Seed])]()))
        }
    }

    /**
     * Targeting Phase: accumulate sample of pages until target is locked
     */

    def phase_targeting(central: V, target: TargetVector[String],
                        average: AverageVector[String],
                        vectors: Map[Seed, (V, Set[Seed])],
                        priorities: Map[Seed, (Priority, Set[Seed])]): Receive = {
        case EvaluatePriorityMatrixStopTargeting => {
            log("Targeting impossible, too little casualties")
            //context.stop(self)
            context.system.shutdown
        }

        case Gather.GatherSeeds(seed, seeds, v) => {
            debug(s"Targeting phase $seed")
            ns.next()

            val (target1, average1, factor) =
                estimate(seed, v.normal, target, average)

            debug("factor*central = %s (required %s)",
                factor * central, cfg.targeting)
            debug("central = %s, factor = %s", central, factor)

            val (vectors1, priorities1, _) =
                enqueue(seeds, factor, seed, v, vectors, priorities)

            if (factor * central > cfg.targeting) {
                val priorities2 = calculate(factor, vectors1, priorities1)

                log("Turn into estimation phase")
                seedqueue ! SeedQueueAvailable
                context.become(phase_estimating(central, target1, average1,
                    vectors1, priorities2, factor, PQ[Item]))
            } else {
                context.become(phase_targeting(central, target1, average1,
                    vectors1, priorities1))
            }
        }
    }

    /**
     * Estimation Phase: estimate gotten pages and request new one on priority
     * base.
     */

    def phase_estimating(central: V,
                         target: TargetVector[String], average: AverageVector[String],
                         vectors: Map[Seed, (V, Set[Seed])],
                         priorities: Map[Seed, (Priority, Set[Seed])], factor: V,
                         queue: SortedSet[Item]): Receive = {
        case EvaluatePriorityMatrixStop =>
            debug("ActorSystem shutdown")
            context.system.shutdown()

        case Gather.GatherSeeds(seed, seeds, v) => {
            val n = ns.next()
            log("Seed #%s: %s %s", n, seed, cfg.limit)
            if (n > cfg.limit) {
                log("Limit has been reached")
                sample ! EvaluatePriorityMatrixStop
                seedqueue ! EvaluatePriorityMatrixStop
            } else {

                val (target1, average1, newfactor) =
                    estimate(seed, v.normal, target, average)

                val priorities2 =
                    if (newfactor.normal * factor.normal < cfg.limit) {
                        debug("Priorities should be recalculated")
                        calculate(factor, vectors, priorities)
                    } else priorities

                val (vectors1, priorities1, queue1) =
                    enqueue(seeds, factor, seed, v, vectors, priorities)
                debug("Queue size: %s", queue.size)
                sample ! SampleHirarchy2PriorityPriority(seed, factor * v.normal)
                seedqueue ! SeedQueueAvailable

                context.become(phase_estimating(central, target1, average1,
                    vectors1, priorities1, newfactor, queue1))
            }
        }

        case SeedQueueGet => {
            debug("Get dispather request %s", sender)
            queue match {
                case PQ((priority, seed), queue) =>
                    val (_, seeds) = priorities(seed)

                    val priorities1 = priorities - seed
                    val vectors1 = vectors ++ {
                        seeds.filter(vectors contains _).map(x => x -> {
                            val (vector, seeds) = vectors(x)
                            (vector, seeds - seed)
                        })
                    }
                    log("Request, priority = %s for %s : %s", priority,
                        seed, seeds.headOption.getOrElse("empty"))

                    //TODO: check continue of estimation  phase
                    sender ! SeedQueueRequest(seed)
                    context.become(phase_estimating(central, target, average,
                        vectors, priorities, factor, queue))

                case _ => {
                    debug("Queue was empty")
                }
            }

        }

        case x => debug("!!!! Unknown message %s from %s", x, sender)
    }
}

