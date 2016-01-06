package ru.wordmetrix.webcrawler

/**
 * EvaluatePriorityMatrix is an implementation of strategy that estimates
 * possible semantic deposit of future web pages into common asset. It uses two
 * abstraction:
 *
 * - SemanticEstimator estimates web pages (seeds) on the base of their
 * content.
 *
 * - NetworkEstimator propagates semantic estimation through the net of web
 * pages.
 */

import java.net.URI
import scala.util.Random._
import scala.collection.immutable.SortedSet
import Gather.{GatherLink, GatherLinkContext, GatherSeeds, GatherAllow}
import SampleHierarchy2Priority.SampleHirarchy2PriorityPriority
import SeedQueue.{
SeedQueueAvailable,
SeedQueueGet,
SeedQueueLink,
SeedQueueRequest
}
import Storage.{StorageSign, StorageVictim}
import akka.actor.{Actor, Props, actorRef2Scala}
import ru.wordmetrix.utils.{CFG, CFGAware, debug}
import ru.wordmetrix.smartfile.SmartFile.fromFile
import EvaluatePriorityMatrix._
import akka.actor.ActorRef
import ru.wordmetrix.features.Features
import ru.wordmetrix.webcrawler.GMLStorage._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

object EvaluatePriorityMatrix {

  abstract sealed class EvaluatePriorityMatrixMessage

  case class EvaluatePriorityMatrixSeed(seeds: Set[Seed])
    extends EvaluatePriorityMatrixMessage

  case object EvaluatePriorityMatrixStopTargeting
    extends EvaluatePriorityMatrixMessage

  case object EvaluatePriorityMatrixDump
    extends EvaluatePriorityMatrixMessage

  case object EvaluatePriorityMatrixStop extends EvaluatePriorityMatrixMessage

  /**
   * Define an EvaluatePriorityMatrix
   *
   * @param storage    A storage for pages;
   * @param gather     An actor that elicits data from pages;
   * @param seedqueue  A dispatcher of requests;
   * @param sample     An actor that maintains a sample of mapping content of
   *                   links to priorities;
   * @param gml        An actor that maintains a storage that dumps network
   *                   into gml;
   * @param cfg        A configure object;
   * @return           An props for EvaluatePriorityMatrix.
   */
  def props(storage: Props, gather: Props, seedqueue: Props, sample: Props,
            gml: Props, linkedvectors: Props, cfg: CFG): Props =
    Props(
      new EvaluatePriorityMatrix(
        storage, gather, seedqueue, sample, gml, linkedvectors,
        new NetworkEstimator()(cfg)
      )(
          cfg,
          v => new SemanticEstimator(v)(cfg)
        )
    )

  /**
   * Extension of SortedSet to use as priority queue
   */
  object PQ {
    implicit val o = Ordering.fromLessThan[Item]({
      case ((p1, u1), (p2, u2)) =>
        if (p1 == p2)
          u1 > u2
        else
          p1 < p2
    }).reverse

    def apply() = SortedSet[Item]()

    def unapply(map: SortedSet[Item]) =
      map.headOption match {
        case None => None
        case Some(x) => Some((x, map - x))
      }
  }

  implicit class PQEx(set: SortedSet[Item]) {
    def insert(x: Item) = set + (x)
  }

  case class RevMap[F](val map: Map[F, Int] = Map[F, Int](),
                       val rmap: Map[Int, F] = Map[Int, F](),
                       n: Int = 0) {
    def update(word: F): (Int, RevMap[F]) = {
      map.get(word) match {
        case Some(x) => (x, this)
        case None =>
          val x = n + 1
          (x, copy(
            map = map + (word -> x),
            rmap = rmap + (x -> word),
            x))
      }
    }

    def update(xs: Set[F])(implicit cfg: CFG): (Set[Int], RevMap[F]) =
      (xs.foldLeft(Set[Int](), this) {
        case ((set, index), (x)) =>
          index.update(x) match {
            case (n, index) => (set + n, index)
          }
      })

    def decode(xs: Iterable[Int])(implicit cfg: CFG) = xs.map(x => rmap(x))

    def decode(x: Int)(implicit cfg: CFG) = rmap(x)
  }

}

class EvaluatePriorityMatrix[NE <: NetworkEstimatorBase[NE], SE <: SemanticEstimatorBase[SE]](storageprop: Props,
                                                                                              gatherprop: Props,
                                                                                              seedqueueprop: Props,
                                                                                              sampleprop: Props,
                                                                                              gmlprop: Props,
                                                                                              linkedvectorsprop: Props,
                                                                                              networkestimator: NE)(implicit cfg: CFG, factoryse: V => SE) extends Actor
with CFGAware {
  override val name = "Evaluate . Matrix"

  import context.dispatcher
  import EvaluatePriorityMatrix._

  val ns = Iterator.from(1)

  val gather = context.actorOf(gatherprop, "Gather")

  val seedqueue = context.actorOf(seedqueueprop, "SeedQueue")

  val storage = context.actorOf(storageprop, "Storage")

  val sample = context.actorOf(sampleprop, "Sample")

  val gml = context.actorOf(gmlprop, "GML")

  val linkedvectors = context.actorOf(linkedvectorsprop, "LinkedVectors")

  gather ! GatherLink(storage, sample, gml, linkedvectors)

  seedqueue ! SeedQueueLink(gather)

  storage ! StorageVictim(seedqueue)

  def receive(): Receive = ({
    case EvaluatePriorityMatrixSeed(seeds: Set[Seed]) => {
      for (seed <- seeds) {
        log("Initial seed: %s", seed)
        seedqueue ! SeedQueueRequest(seed)
      }

      context.become(phase_initialization(seeds.size, AverageVector[Word](), Set()))
    }
  }: Receive) orElse common()

  def common(): Receive = {
    case msg@GatherAllow(seeds) =>
      debug("allow seeds: %s", seeds)
      gather ! msg
  }

  /**
   * Initialization Phase: download initial page(s)
   */
  def phase_initialization(n: Int, central: AverageVector[Word], init_seeds: Set[Seed]): Receive = {

    case msg@GatherLinkContext(_, _) => sample ! msg

    case Gather.GatherSeeds(seed, seeds, v) => {
      ns.next()
      log("Initial phase, n = %s size = %s, seed = %s", n, seeds.size, seed)

      storage ! StorageSign(seed)

      if (n > 1) {
        context.become(phase_initialization(n - 1, central + v, init_seeds ++ seeds))

      } else {
        for (seed <- shuffle(init_seeds ++ seeds).toList) {
          seedqueue ! SeedQueueRequest(seed)
        }
        seedqueue ! EvaluatePriorityMatrixStopTargeting

        val sense = factoryse((central + v).normal)

        log("Start targeting " + sense.size)

        context.become(
          phase_targeting(
            sense,
            networkestimator,
            RevMap[Seed]()
          )
        )
      }
    }
  }

  /**
   * Targeting Phase: accumulate sample of pages until target is locked
   */

  def phase_targeting(sense: SE, network: NE, index: RevMap[Seed]): Receive = {
    case EvaluatePriorityMatrixStopTargeting => {
      log("Targeting impossible, too little casualties")
      //context.stop(self)
      context.system.terminate()
    }

    case Gather.GatherSeeds(seed, seeds, v) => {
      val n = ns.next()

      debug("Targeting with (%d) %s %s %s", n, seed, seeds.size,
        network.size)

      index.update(seed) match {
        case (id, index) => index.update(seeds) match {
          case (ids, index) =>
            sense.estimate(id, v.normal, {
              debug("Seed %s was accepted as target", seed)
              storage ! StorageSign(seed)
            }) match {
              case sense =>
                val network1 = network.update(ids, sense.factor, id, v)

                log("Check if %s > %s (direction is collinear to specimen)",
                  sense.factor * sense.central, cfg.targeting)

                if (sense.factor * sense.central > cfg.targeting) {
                  log("Turn into estimation phase")

                  val network2 = network1.calculate(sense.factor)

                  seedqueue ! SeedQueueAvailable
                  self ! EvaluatePriorityMatrixDump
                  context.become(
                    phase_estimating(sense, network2, network2.queue(), index)
                  )

                } else {
                  context.become(
                    phase_targeting(sense, network1, index)
                  )
                }
            }
        }
      }
    }
  }

  /**
   * Estimation Phase: estimate gotten pages and request new one on priority
   * base.
   */
  def phase_estimating(sense: SE,
                       network: NE,
                       queue: SortedSet[Item], index: RevMap[Seed]): Receive = {

    case EvaluatePriorityMatrixStop =>
      debug("ActorSystem shutdown")
      context.system.shutdown()

    case GatherSeeds(seed, seeds, v) => {
      val n = ns.next()

      log("Estimation of seed #%s (%s): %s, queue = %s",
        n, cfg.limit, seed, queue.size)

      sense match {
        case sense: SemanticEstimator if (n % 500 == 0) =>
          gml ! GMLStorageEstimator(sense)
        case _ =>
      }

      if (n > cfg.limit) {
        log("Limit has been reached")
        implicit val timeout = Timeout(5 seconds)

        val f = gather ? Gather.GatherDecode(sense.factor)
        f.onSuccess {
          case x: VS => (cfg.path / "vocabulary.dat") write (x)
        }

        sample ! EvaluatePriorityMatrixStop
        seedqueue ! EvaluatePriorityMatrixStop
        sense match {
          case sense: SemanticEstimator =>
            gml ! GMLStorageEstimator(sense)
          case _ =>
        }
        gml ! EvaluatePriorityMatrixStop
        linkedvectors ! EvaluatePriorityMatrixStop
      } else {
        index.update(seed) match {
          case (id, index) => index.update(seeds) match {
            case (ids, index) =>
              val sense1 = sense.estimate(id, v.normal, {
                debug("Seed %s was accepted as target", seed)
                storage ! StorageSign(seed)
              })

              debug("Check if %s > %s (direction is collinear to specimen)",
                sense.factor * sense.central, cfg.targeting)

              debug("Priorities actual while %s > %s",
                sense1.factor.normal * sense.factor.normal, cfg.prioriting)

              val network1 = network.check(sense1.factor.normal)
                .update(ids, sense.factor, id, v)

              val queue2 = network1.queue(queue)

              sample ! SampleHirarchy2PriorityPriority(seed,
                sense.factor * v.normal)

              seedqueue ! SeedQueueAvailable

              context.become(phase_estimating(sense1, network1, queue2, index))
          }
        }
      }
    }

    case SeedQueueGet => {
      debug("Get dispather request %s", sender)

      queue match {
        case PQ((priority, seed), queue) =>
          log("Request priority = %s for %s", priority, seed)
          //TODO: check continue of estimation  phase
          sender ! SeedQueueRequest(index.decode(seed))
          context.become(phase_estimating(
            sense, network.eliminate(seed), queue, index
          ))
        case _ => {
          debug("Queue was empty")
        }
      }
    }
  }
}

abstract class SemanticEstimatorBase[SE <: SemanticEstimatorBase[SE]] {
  def estimate(seed: SeedId, v: V, callback: => Unit): SE

  def factor: V

  val central: V

  val size: Int
}

abstract trait NetworkEstimatorBase[U <: NetworkEstimatorBase[U]] {
  def queue(queue: SortedSet[Item] = PQ()): SortedSet[Item]

  def calculate(factor: V): U

  def update(seeds: Set[SeedId], factor: V, source_seed: SeedId, v: V): U

  def check(factor: V): U

  def eliminate(seed: SeedId): U

  val size: Int
}
 
