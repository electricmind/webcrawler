/**
 *
 */
package ru.wordmetrix.webcrawler

import akka.actor.Actor
import ru.wordmetrix.utils.CFG
import akka.actor.Props
import EvaluatePriorityMatrix._
import ru.wordmetrix.utils.CFGAware
import ru.wordmetrix.utils.ActorDebug.actor2ActorDebug

trait SampleHirarchy2PriorityBase {
    abstract class SampleHirarchy2PriorityMessage
}

trait SampleHierarchy2PriorityBase extends Actor with CFGAware {
    implicit val cfg : CFG
    
    def receive(): Receive = {
        case EvaluatePriorityMatrixStop => {
            this.log("Stop")
            context.stop(self)
        }
    }
}

object SampleHierarchy2PriorityStub extends SampleHirarchy2PriorityBase {
    def props(cfg: CFG) =
        Props(new SampleHierarchy2PriorityStub()(cfg))
}

class SampleHierarchy2PriorityStub(implicit val cfg : CFG) extends SampleHierarchy2PriorityBase
