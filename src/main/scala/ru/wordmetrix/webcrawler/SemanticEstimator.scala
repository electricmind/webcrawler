package ru.wordmetrix.webcrawler

import WebCrawler._
import Storage._
import ru.wordmetrix.utils.CFG
import EvaluatePriorityMatrix._
import akka.actor.ActorRef
import ru.wordmetrix.utils.debug

class SemanticEstimator(val central: V, val target: TargetVector[String],
                        val average: AverageVector[String]) extends SemanticEstimatorBase[SemanticEstimator]() {

    def this(central: V)(implicit cfg: CFG) = this(
        central = central,
        target = new TargetVector[String](n = cfg.targets) + central,
        average = new AverageVector[String](central)
    )

    val size = target.vs.length
    
    def copy(central: V = central, target: TargetVector[String] = target,
             average: AverageVector[String] = average) =
        new SemanticEstimator(central, target, average)
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
