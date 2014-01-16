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
            new EvaluatePriorityMatrix(
                storage, gather, seedqueue, sample,
                new NetworkEstimator()(cfg)
            )(
                cfg,
                v => new SemanticEstimator(v)(cfg)
            )
        )
}

abstract class SemanticEstimatorBase[SE <: SemanticEstimatorBase[SE]] {
    def estimate(seed: Seed, v: V, storage: ActorRef): SE

    def factor: V

    val central: V

    val size: Int
}

abstract trait NetworkEstimatorBase[U <: NetworkEstimatorBase[U]] {
    def queue(queue: SortedSet[Item] = PQ()): SortedSet[Item]
    def calculate(factor: V): U
    def update(seeds: Set[Seed], factor: V, source_seed: Seed, v: V): U
    def check(factor: V): U
    def eliminate(seed: Seed): U
    val size: Int
}

class EvaluatePriorityMatrix[NE <: NetworkEstimatorBase[NE], SE <: SemanticEstimatorBase[SE]](storageprop: Props,
                                                                                              gatherprop: Props,
                                                                                              seedqueueprop: Props,
                                                                                              sampleprop: Props,
                                                                                              networkestimator: NE)(implicit cfg: CFG, factoryse: V => SE) extends Actor
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

        case Gather.GatherSeeds(seed, seeds, v) => {
            ns.next()
            log("Initial phase, n = %s, seed = %s", seeds.size, seed)
            val sense = factoryse(v.normal)

            // I need it only to do deterministic test
            for (seed <- seeds.toList.sorted) {
                seedqueue ! SeedQueueRequest(seed)
            }

            seedqueue ! EvaluatePriorityMatrixStopTargeting
            storage ! StorageSign(seed)

            log("Start targeting " + sense.size)

            context.become(
                phase_targeting(
                    sense,
                    networkestimator
                )
            )
        }
    }

    /**
     * Targeting Phase: accumulate sample of pages until target is locked
     */

    def phase_targeting(sense: SE, network: NE): Receive = {
        case EvaluatePriorityMatrixStopTargeting => {
            log("Targeting impossible, too little casualties")
            //context.stop(self)
            context.system.shutdown
        }

        case Gather.GatherSeeds(seed, seeds, v) => {
            val n = ns.next()

            debug("Targeting with (%d) %s %s %s", n, seed, seeds.size,
                network.size)

            sense.estimate(seed, v.normal, storage) match {
                case sense =>
                    val network1 = network.update(seeds, sense.factor, seed, v)

                    log("Check if %s > %s (direction is collinear to specimen)",
                        sense.factor * sense.central, cfg.targeting)

                    if (sense.factor * sense.central > cfg.targeting) {
                        log("Turn into estimation phase")
                        val network2 = network1.calculate(sense.factor)

                        seedqueue ! SeedQueueAvailable

                        context.become(
                            phase_estimating(sense, network2, network2.queue())
                        )

                    } else {
                        context.become(
                            phase_targeting(sense, network1)
                        )
                    }
            }
        }
    }
    /**
     * Estimation Phase: estimate gotten pages and request new one on priority
     * base.
     */
    def phase_estimating(sense: SE,
                         //factor: V,
                         network: NE,
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

                val network1 = network.check(sense1.factor.normal).update(seeds,
                    sense.factor, seed, v)

                val queue2 = network1.queue(queue)

                sample ! SampleHirarchy2PriorityPriority(seed,
                    sense.factor * v.normal)

                seedqueue ! SeedQueueAvailable

                context.become(phase_estimating(sense1, network1, queue2))
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
    }
}
