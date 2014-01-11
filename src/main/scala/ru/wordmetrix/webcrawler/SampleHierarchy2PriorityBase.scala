/**
 *
 */
package ru.wordmetrix.webcrawler

import akka.actor.Actor
import ru.wordmetrix.utils.CFG
import akka.actor.Props



trait SampleHirarchy2PriorityBase {
    abstract class SampleHirarchy2PriorityMessage
}

trait SampleHierarchy2PriorityBase extends Actor

object SampleHierarchy2PriorityStub  extends SampleHirarchy2PriorityBase {
    
    def props(cfg : CFG) = 
        Props(new SampleHierarchy2PriorityStub())
}

class SampleHierarchy2PriorityStub  extends SampleHierarchy2PriorityBase {
    def receive(): Receive = { case _ => {}}
}
