/**
 *
 */
package ru.wordmetrix.webcrawler

import akka.actor.Actor



trait SampleHirarchy2PriorityBase {
    abstract class SampleHirarchy2PriorityMessage
}

trait SampleHierarchy2PriorityBase extends Actor

class SampleHierarchy2PriorityStub  extends SampleHierarchy2PriorityBase {
    def receive(): Receive = { case _ => {}}
}
