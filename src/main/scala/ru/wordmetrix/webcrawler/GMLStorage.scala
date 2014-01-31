package ru.wordmetrix.webcrawler

import Gather.GatherIntel

import scala.util.Random.shuffle
import akka.actor.{ Actor, ActorRef, Props }
import ru.wordmetrix.smartfile.SmartFile.fromFile
import ru.wordmetrix.utils._
import ru.wordmetrix.utils.impl._
import scala.concurrent.Future
import ru.wordmetrix.smartfile.SmartFile._
import scala.util.Try
import akka.pattern.pipe
import EvaluatePriorityMatrix._

import Math._

object GMLStorage {

    type SE = SemanticEstimator

    sealed abstract class GMLStorageMessage

    case class GMLStorageFinished
        extends GMLStorageMessage

    case class GMLStorageSeed(seed: Seed, seeds: Set[Seed], v: V)
        extends GMLStorageMessage

    //    case class GMLStorageEstimator[SE <: SemanticEstimatorBase[SE]](estimator: SE)
    case class GMLStorageEstimator(estimator: SE)
        extends GMLStorageMessage

    def props(cfg: CFG): Props =
        Props(new GMLStorage()(cfg))

    implicit class GMLStorageStateDump(state: GMLStorageState) {
        def dump(estimator: SemanticEstimatorBase[_])(
            implicit cfg: CFG): String = {
            val edges = Iterator.from(1)

            s"""
        graph
        [
        Creator WebCrawler
        directed 1
        ${
                (for {
                    (id, (v, ids)) <- state.matrix
                    uri <- state.revmap.rmap.get(id)

                    if ids.size > 0 || cfg.with_incomplete
                } yield {
                    s"""
                node 
                [
                  id ${id}
                  label "${Storage.seedToFilename(uri)}"
                  similarity ${estimator.central.normal * v.normal}
                  priority ${estimator.factor.normal * v.normal}
                  dimension ${v.size}
                  length ${v.norm} 
                ]
                """
                }) mkString ("\n")
            }
        ${
                (for {
                    (id1, (v1, ids1)) <- state.matrix
                    id2 <- ids1
                    if (state.matrix contains id2) || cfg.with_incomplete
                    (v2, ids2) <- state.matrix.get(id2)
                } yield {
                    s"""
                edge 
                [
                  id ${edges.next}
                  source ${id1}
                  target ${id2}
                  value ${(v1 - v2).norm}
                  angle ${v1.normal * v2.normal}
                  exist ${(state.matrix contains id2)}
                  propagate ${id2 < id1}
                  
                ]
                """
                }) mkString ("\n")
            }
        
        ]
        """.split("\n").map(_.trim).mkString("\n")
        }

        def statistic[SE <: SemanticEstimatorBase[SE]](estimator: SE)(
            implicit cfg: CFG): String = {
 
            s"""
               Size of Network : ${state.matrix.size}
               Density of Network : ${state.density_net()} 

               Density of Cloud : ${state.density_cloud()}

               Deviation from central : ${
                state.deviation_central(estimator.central)
            } 

            ${
                estimator match {
                    case e: SemanticEstimator => s"""
                       Deviation from target : ${
                        state.deviation_target(e.target)
                    }

                       Deviation from average : ${
                        state.deviation_average(e.target)
                    }
                          target - central deviation : ${
                        (e.target.normal - e.central.normal).sqr
                    }

                          target - average deviation : ${
                        (e.target.normal - state.average.normal).sqr
                    }

                       """
                    case _ => ""
                }
            }

               Accumulated priority : ${state.accumulated_priority(estimator)}
               """.split("\n").map(_.trim).filter(_.nonEmpty).mkString("\n")
        }
    }
}

class GMLStorage()(implicit val cfg: CFG) extends Actor with CFGAware {
    override val name = "GMLStorage"

    import GMLStorage._

    import context.dispatcher

    def receive(): Receive = {
        case GMLStorageSeed(seed, seeds, v) => {
            log("Initial seed")
            context.become(collect(GMLStorageState(seed, seeds, v)), false)
        }
    }

    def collect(state: GMLStorageState): Receive = {
        case GMLStorageSeed(seed, seeds, v) => {
            debug("One more  seed")

            context.become(collect(state.update(seed, seeds, v)), false)
        }

        case EvaluatePriorityMatrixStop =>
            context.stop(self)

        case GMLStorageEstimator(estimator) => {
            Future({
                log("Dump of network initiated")

                (cfg.path / "network.gml").write(
                    state.dump(estimator)
                )

                (cfg.path / s"statistic.${state.matrix.size}.txt").write(
                    state.statistic(estimator)
                )

                debug("Dump of network completed")
                GMLStorageFinished
            }) pipeTo self
            context.become(dump(state, None), false)
        }
    }

    def dump(state: GMLStorageState,
             estimator: Option[SemanticEstimatorBase[_]]): Receive = {

        case GMLStorageSeed(seed, seeds, v) =>
            context.become(dump(state.update(seed, seeds, v), estimator), false)

        case GMLStorageEstimator(estimator: SemanticEstimatorBase[_]) => {
            context.become(dump(state, Some(estimator)), false)
        }

        case EvaluatePriorityMatrixStop =>
            context.stop(self)

        case GMLStorageFinished => {
            estimator match {
                case Some(estimator: SE) =>
                    self ! GMLStorageEstimator(estimator)
                case None =>
            }
            context.become(collect(state), false)
        }
    }
}

object GMLStorageState {
    def apply(seed: Seed, seeds: Set[Seed],
              v: V)(implicit cfg: CFG): GMLStorageState =
        GMLStorageState(
            EvaluatePriorityMatrix.RevMap[Seed](), Map()
        ).update(seed, seeds, v)
}

case class GMLStorageState(
        val revmap: EvaluatePriorityMatrix.RevMap[Seed],
        val matrix: Map[SeedId, (V, Set[SeedId])])(implicit val cfg: CFG) {

    def update(seed: Seed, seeds: Set[Seed], v: V) = {
        val (id, revmap1) = revmap.update(seed)
        val (ids, revmap2) = revmap1.update(seeds)

        copy(revmap = revmap2, matrix = matrix + (id -> (v, ids)))
    }

    def density_net() = 
        (for {
            (seed1, (v1, seeds)) <- matrix
            seed2 <- seeds
            (v2, _) <- matrix.get(seed2)
        } yield {
            val d = (v1.normal - v2.normal).sqr
            debug("density net = %s", d)
            d
        }).scanLeft((0.0, 0))({
            case ((ds, n), d) => (ds + d, n + 1)
        }).last match {
            case (ds, n) => ds / n
        }
    

    def density_cloud(): Double =
        debug.time(s"Density of cloud of ${matrix.size} vectors") {
            (for {
                List(seed1, seed2) <- shuffle(
                    matrix.keys.toList.combinations(2).toList).toIterator
                (v1, _) <- matrix.get(seed1)
                (v2, _) <- matrix.get(seed2)
            } yield { (v1.normal - v2.normal).sqr }).scanLeft((0.01, 0))({
                case ((ds, n), d) => (ds + d, n + 1)
            }).map({
                case (ds, n) =>
                    val dsn = ds / n
                    debug("density cloud = %s", dsn)
                    dsn
            }).drop(100).sliding(2).dropWhile({
                case List(x1, x2) =>
                    abs(x2 - x1) / x1 > 0.001
            }).take(1).toList.headOption.getOrElse(List(0.0, 0.0)).head
        }

    def deviation_central(v1: V) =
        (for {
            (seed1, (v2, seeds)) <- matrix
        } yield {
            val d = (v1.normal - v2.normal).sqr
            d
        }).sum / matrix.size

    def deviation_target(target: TargetVector[Word]) =
        deviation_central(target.normal)

    def deviation_average(target: TargetVector[Word]) =
        deviation_central(average)

    def accumulated_priority(estimator: SemanticEstimatorBase[_]) =
        matrix.map({
            case (_, (v, _)) => estimator.factor.normal * v.normal
        }).sum / matrix.size

    lazy val average = matrix.map({
        case (_, (v, _)) => v.normal
    }).reduce(_ + _).normal
}
