package ru.wordmetrix.webcrawler

import Gather.GatherIntel

import akka.actor.{ Actor, ActorRef, Props }
import ru.wordmetrix.smartfile.SmartFile.fromFile
import ru.wordmetrix.utils._
import scala.concurrent.Future
import ru.wordmetrix.smartfile.SmartFile._
import scala.util.Try
import akka.pattern.pipe
import EvaluatePriorityMatrix._

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
        def dump(estimator: SemanticEstimatorBase[_]): String = {
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

                    if ids.size > 0 //TODO: && cfg.dumpincomplete
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
                    if (state.matrix contains id2) //TODO: cfg.dumpincomplete
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
        """".split("\n").map(_.trim).mkString("\n")
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

        case GMLStorageEstimator(estimator: SemanticEstimatorBase[_]) => {
            Future({
                log("Dump of network initiated")

                (cfg.path / "network.gml").write(
                    state.dump(estimator)
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
}
