package ru.wordmetrix.webcrawler
/**
 * EvaluatePriorityMatrix is an implementation of strategy that estimates
 * possible semantic deposit of future web pages into common asset. It uses two
 * abstraction:
 *
 *  - SemanticEstimator estimates web pages (seeds) on the base of their
 *  content.
 *
 *  - NetworkEstimator propagates semantic estimation through the net of web
 *  pages.
 */
import java.net.URI
import scala.collection.immutable.SortedSet
import Gather.{ GatherLink, GatherLinkContext, GatherSeeds }
import SampleHierarchy2Priority.SampleHirarchy2PriorityPriority
import SeedQueue.{ SeedQueueAvailable, SeedQueueGet, SeedQueueLink, SeedQueueRequest }
import Storage.{ StorageSign, StorageVictim }
import WebCrawler.{ Seed, Word }
import akka.actor.{ Actor, Props, actorRef2Scala }
import ru.wordmetrix.utils.{ CFG, CFGAware, debug }
import EvaluatePriorityMatrix._
import akka.actor.ActorRef

object EvaluatePriorityMatrix {
    abstract sealed class EvaluatePriorityMatrixMessage

    type Priority = Double
    type V = ru.wordmetrix.vector.Vector[Word]
    type Item = (Priority, Seed)
    type VItem = (Priority, V)

    case class EvaluatePriorityMatrixSeed(seed: URI)
        extends EvaluatePriorityMatrixMessage

    case class EvaluatePriorityMatrixStopTargeting
        extends EvaluatePriorityMatrixMessage
    case class EvaluatePriorityMatrixStop extends EvaluatePriorityMatrixMessage

    /**
     * Extension of SortedSet to use as priority queue
     */
    object PQ {
        implicit val o = Ordering[Item].reverse
        def apply() = SortedSet[Item]()
        def apply(i1: Item) = SortedSet[Item](i1)
        def apply(x1: Item, x2: Item, xs: Item*) =
            xs.foldLeft(SortedSet[Item](x1, x2)) {
                case (map, x) => map + (x)
            }
        def apply(i1: Item, map: SortedSet[Item]) = map + (i1)
        def unapply(map: SortedSet[Item]) =
            map.headOption match {
                case None    => None
                case Some(x) => Some((x, map.tail))
            }
    }

    implicit class PQEx(set: SortedSet[Item]) {
        def insert(x: Item) = set + (x)
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

case class SemanticEstimator(central: V, target: TargetVector[String],
                             average: AverageVector[String]) {

    /**
     * Estimate how seed was relevant
     *
     * @return target, average, (new)factor
     */
    def estimate(seed: Seed, v: V, storage: ActorRef)(implicit cfg: CFG): SemanticEstimator = {
        val average1 = average + v
        val target1 = target + (v, {
            val pv = v * target.average.normal;
            val p = target.priority()

            debug("Seed %s was accepted as target %s, it's %s < %s",
                seed, target.vs.length,
                p, pv)
            //TODO: we should factor out storage                
            storage ! StorageSign(seed)
        })

        copy(target = target1, average = average1)
    }

    def factor = target.normal - average.normal

}

case class NetworkEstimator(
        val vectors: Map[Seed, (V, Set[Seed])] = Map[Seed, (V, Set[Seed])](),
        val priorities: Map[Seed, (Priority, Set[Seed])] = Map[Seed, (Priority, Set[Seed])](),
        val pfactor: V = ru.wordmetrix.vector.Vector[String]())(implicit cfg: CFG) {
    import EvaluatePriorityMatrix._

    type PQQ = SortedSet[Item]

    /**
     * Update priority queue with new priorities
     * 
     * @param queue  - A queue
     * @return       - An updated queue
     */
    
    def queue(queue: PQQ = PQ()): PQQ =
        priorities.foldLeft(PQ()) {
            case (queue, ((seed, (p, seeds)))) => queue insert (p, seed)
        }

    /**
     * Calculate new matrix of actual
     * priorities all of the seeds that are in queue by multiplying
     * factor of new seed on all of the seeds that sourced from it.
     *
     * @param factor   Current factor of similarity
     * @return         New matrix
     */

    def calculate(factor: V) = copy(
        priorities = vectors.map({
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
        }),
        pfactor = factor
    )

    /**
     *  The rule of combination of priorities two adjacent vertices on
     *  the neighborhood graph.
     */
    protected def combinepolicy(priorities: Iterable[Double]) = priorities.max

    /**
     * Add new seed to known network
     *
     * @param seeds    New set of seed associated with given seed
     * @param seed     New seed
     * @param factor   Current factor of similarity
     * @param v        Content vector of seed
     * @return         New matrix
     */
    def update(seeds: Set[Seed], factor: V, source_seed: Seed, v: V): NetworkEstimator = {
        vectors + (source_seed -> (v, seeds)) match {
            case vectors =>
                copy(vectors = vectors,
                    priorities = seeds.foldLeft(priorities) {
                        case (priorities, seed) =>
                            priorities + (
                                seed -> (
                                    (combinepolicy(
                                        priorities.getOrElse(
                                            seed,
                                            (0d, Set[Seed]())
                                        )._2.map(x => vectors(x)._1 * factor)
                                            + v * factor
                                    ),
                                        priorities.getOrElse(seed, (0d, Set[Seed]()))._2
                                        + source_seed
                                    )
                                )
                            )
                    }
                )
        }
    }

    def check(factor: V) = if (factor.normal * pfactor < cfg.prioriting) {
        debug("Priorities should be recalculated")
        calculate(factor)
    } else this

    def eliminate(seed: Seed): NetworkEstimator = {
        val (_, seeds) = priorities(seed)
        val priorities1 = priorities - seed
        val vectors1 = vectors ++ {
            seeds.filter(vectors contains _).map(x => x -> {
                val (vector, seeds) = vectors(x)
                (vector, seeds - seed)
            })
        }
        copy(priorities = priorities1, vectors = vectors1)
    }
}

class EvaluatePriorityMatrix(storageprop: Props,
                             gatherprop: Props,
                             seedqueueprop: Props,
                             sampleprop: Props)(implicit cfg: CFG) extends Actor
        with CFGAware {
    override val name = "Evaluate . Matrix"

    import EvaluatePriorityMatrix._
    val ns = Iterator.from(1)

    val gather = context.actorOf(gatherprop, "Gather")

    val seedqueue = context.actorOf(seedqueueprop, "SeedQueue")

    val storage = context.actorOf(storageprop, "Storage")

    val sample = context.actorOf(sampleprop, "Sample")

    gather ! GatherLink(storage, sample)

    seedqueue ! SeedQueueLink(gather)

    storage ! StorageVictim(seedqueue)

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

        case msg @ GatherLinkContext(_, _) => sample ! msg

        case GatherSeeds(seed, seeds, v) => {
            ns.next()
            val v1 = v.normal
            log("Initial phase, n = %s, seed = %s", seeds.size, seed)
            val sense = SemanticEstimator(
                central = v1,
                target = new TargetVector[String](n = cfg.targets) + v1,
                average = new AverageVector[String](v1)
            )

            // I need it only to do deterministic test
            for (seed <- seeds.toList.sorted) {
                seedqueue ! SeedQueueRequest(seed)
            }

            seedqueue ! EvaluatePriorityMatrixStopTargeting
            storage ! StorageSign(seed)

            log("Start targeting " + sense.target.vs.length)

            context.become(
                phase_targeting(
                    sense,
                    NetworkEstimator()
                )
            )
        }
    }

    /**
     * Targeting Phase: accumulate sample of pages until target is locked
     */

    def phase_targeting(sense: SemanticEstimator,
                        network: NetworkEstimator): Receive = {
        case EvaluatePriorityMatrixStopTargeting => {
            log("Targeting impossible, too little casualties")
            //context.stop(self)
            context.system.shutdown
        }

        case Gather.GatherSeeds(seed, seeds, v) => {
            val n = ns.next()
            debug("Targeting with (%d) %s %s %s", n, seed, seeds.size,
                network.vectors.size)

            val sense1 = sense.estimate(seed, v.normal, storage)

            val network1 = network.update(seeds, sense1.factor, seed, v)

            log("Check if %s > %s (direction is collinear to specimen)",
                sense1.factor * sense1.central, cfg.targeting)

            if (sense1.factor * sense1.central > cfg.targeting) {
                log("Turn into estimation phase")
                val network2 = network1.calculate(sense1.factor)
                seedqueue ! SeedQueueAvailable

                context.become(
                    phase_estimating(sense1, network, network.queue())
                )
            } else {
                context.become(
                    phase_targeting(sense1, network1)
                )
            }
        }
    }

    /**
     * Estimation Phase: estimate gotten pages and request new one on priority
     * base.
     */

    def phase_estimating(sense: SemanticEstimator,
                         //factor: V,
                         network: NetworkEstimator,
                         queue: SortedSet[Item]): Receive = {

        case EvaluatePriorityMatrixStop =>
            debug("ActorSystem shutdown")
            context.system.shutdown()

        case GatherSeeds(seed, seeds, v) => {
            val n = ns.next()
            log("Estimation of seed #%s (%s): %s, queue = %s",
                n, cfg.limit, seed, queue.size)
            if (n > cfg.limit) {
                log("Limit has been reached")
                sample ! EvaluatePriorityMatrixStop
                seedqueue ! EvaluatePriorityMatrixStop
            } else {
                val sense1 = sense.estimate(seed, v.normal, storage)

                debug("Check if %s > %s (direction is collinear to specimen)",
                    sense.factor * sense.central, cfg.targeting)

                debug("Priorities actual while %s > %s",
                    sense1.factor.normal * sense.factor.normal, cfg.prioriting)

                val network1 = network.check(sense1.factor.normal)
                val network2 = network1.update(seeds, sense.factor, seed, v)
                val queue2 = network2.queue(queue)

                sample ! SampleHirarchy2PriorityPriority(seed,
                    sense.factor * v.normal)

                seedqueue ! SeedQueueAvailable

                context.become(phase_estimating(sense1, network2, queue2))
            }
        }

        case SeedQueueGet => {
            debug("Get dispather request %s", sender)
            queue match {
                case PQ((priority, seed), queue) =>
                    log("Request priority = %s for %s", priority, seed)
                    //TODO: check continue of estimation  phase

                    sender ! SeedQueueRequest(seed)
                    context.become(phase_estimating(
                        sense, network.eliminate(seed), queue
                    ))
                case _ => {
                    debug("Queue was empty")
                }
            }
        }
        case x => debug("!!!! Unknown message %s from %s", x, sender)
    }
}
