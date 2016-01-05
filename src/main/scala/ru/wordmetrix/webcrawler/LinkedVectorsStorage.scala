package ru.wordmetrix.webcrawler

import scala.util.Random.shuffle

import EvaluatePriorityMatrix._
import akka.actor.{ Actor, ActorRef, Props }
import akka.pattern.pipe
import ru.wordmetrix.smartfile.SmartFile.{ fromFile, _ }
import ru.wordmetrix.utils._
import ru.wordmetrix.utils.impl._

object LinkedVectorsStorage {
    type SE = SemanticEstimator

    sealed abstract class LinkedVectorsStorageMessage

    case object LinkedectorsStorageFinished
        extends LinkedVectorsStorageMessage

    case class LinkedVectorsStorageSeed(seed: Seed, seeds: Set[Seed], v: VS)
        extends LinkedVectorsStorageMessage

    def props(cfg: CFG): Props =
        Props(new LinkedVectorsStorage()(cfg))
}

class LinkedVectorsStorage()(implicit val cfg: CFG) extends Actor with CFGAware {
    override val name = "LinkedVectorsStorage"

    import LinkedVectorsStorage._

    import context.dispatcher

    def receive(): Receive = { case _ => }

    context.become(collect(LinkedVectorsStorageState()))

    def collect(state: LinkedVectorsStorageState): Receive = {
        case LinkedVectorsStorageSeed(seed, seeds, v) => {
            val (state1, id, ids) = state.update(seed, seeds)

            (cfg.path / "vectors" / "matrix.dat").append write (
                s"$id : ${ids.mkString(" ")}\n"
            )

            (cfg.path / "vectors" / "map.lst").append write (
                s"$id : $seed\n"
            )

            (cfg.path / "vectors" / s"$id.dat").write(v)

            context.become(collect(state1))
        }

        case EvaluatePriorityMatrixStop =>
            context.stop(self)
    }
}

case class LinkedVectorsStorageState(
        val revmap: EvaluatePriorityMatrix.RevMap[Seed] = RevMap[Seed]())(implicit val cfg: CFG) {

    def update(seed: Seed, seeds: Set[Seed]) = {
        val (id, revmap1) = revmap.update(seed)
        val (ids, revmap2) = revmap1.update(seeds)

        (copy(revmap = revmap2), id, ids)
    }
}
