/**
 *
 */
package ru.wordmetrix.webcrawler

import EvaluatePriorityMatrix.EvaluatePriorityMatrixStop
import akka.actor.{ Actor, Props }
import ru.wordmetrix.utils._

trait SampleHirarchy2PriorityBase {
    abstract class SampleHirarchy2PriorityMessage
}

trait SampleHierarchy2PriorityBase extends Actor with CFGAware {
    implicit val cfg: CFG

    def receive(): Receive = {
        case EvaluatePriorityMatrixStop => {
            context.stop(self)
        }
    }
}

object SampleHierarchy2PriorityStub extends SampleHirarchy2PriorityBase {
    def props(cfg: CFG) =
        Props(new SampleHierarchy2PriorityStub()(cfg))
}

class SampleHierarchy2PriorityStub(
    implicit val cfg: CFG) extends SampleHierarchy2PriorityBase
