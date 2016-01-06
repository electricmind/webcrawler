package ru.wordmetrix.webcrawler

import EvaluatePriorityMatrix._
import WebCrawler._
import scala.collection.immutable.SortedSet
import ru.wordmetrix.utils.CFG
import ru.wordmetrix.utils.debug

/**
 * An implementation of NetworkEstimator that propagates estimation of semantic
 * similarity throughout known part of network trying to determine how suitable
 * new vertices could be.
 *
 * @author Elec
 *
 */
class NetworkEstimator(val vectors: Map[SeedId, (V, Set[SeedId])] = Map[SeedId, (V, Set[SeedId])](),
                       val priorities: Map[SeedId, (Priority, Set[SeedId])] = Map[SeedId, (Priority, Set[SeedId])](),
                       val pfactor: V = ru.wordmetrix.vector.Vector[Word]()(0.0001d, implicitly[Ordering[Word]]))(implicit cfg: CFG) extends NetworkEstimatorBase[NetworkEstimator] {

  import EvaluatePriorityMatrix._

  implicit def accuracy = cfg.accuracy

  type PQQ = SortedSet[Item]

  val size = vectors.size

  def copy(vectors: Map[SeedId, (V, Set[SeedId])] = vectors,
           priorities: Map[SeedId, (Priority, Set[SeedId])] = priorities,
           pfactor: V = pfactor)(implicit cfg: CFG) =
    new NetworkEstimator(vectors, priorities, pfactor)

  /**
   * Update priority queue with new priorities
   *
   * @param queue  - A queue
   * @return       - An updated queue
   */

  def queue(queue: PQQ = PQ()): PQQ = {
    //I need  this botch for awhile, but better queue is needed as well instead of ignoring argument
    val queue = if (cfg.use_breadthsearch)
      SortedSet[Item]()(Ordering.fromLessThan[Item]({
        case ((p1, u1), (p2, u2)) =>
          u1 > u2
      }).reverse)
    else PQ()

    priorities.foldLeft(queue) {
      case (queue, ((seed, (p, seeds)))) => queue insert(p, seed)
    }

  }

  /**
   * Calculate new matrix of actual
   * priorities all of the seeds that are in queue by multiplying
   * factor of new seed on all of the seeds that sourced from it.
   *
   * @param factor   Current factor of similarity
   * @return         New matrix
   */
  // Theoretically, we can propagate priority a few iteration that allows
  // to pass through "technical pages", like indexes. Page priority would
  // become priority of page + priority that comes to page thru the links.
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
        seed -> ((
          combinepolicy(ps) /* + self-priority if it is known */ ,
          priorities(seed)._2
          ))
    }),
    pfactor = factor
  )

  /**
   * The rule of combination of priorities two adjacent vertices on
   * the neighborhood graph.
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
  def update(seeds: Set[SeedId], factor: V, source_seed: SeedId, v: V): NetworkEstimator = {
    vectors + (source_seed ->(v, seeds)) match {
      case vectors =>
        copy(vectors = vectors,
          priorities = seeds.foldLeft(priorities) {
            case (priorities, seed) =>
              priorities + (
                seed -> (
                  (combinepolicy(
                    priorities.getOrElse(
                      seed,
                      (0d, Set[SeedId]())
                    )._2.map(x => vectors(x)._1 * factor)
                      + v * factor // + ,_1 to propagate
                  ),
                    priorities.getOrElse(seed, (0d, Set[SeedId]()))._2
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

  def eliminate(seed: SeedId): NetworkEstimator = {
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
