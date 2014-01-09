/**
 *
 */
package ru.wordmetrix.webcrawler
import WebCrawler._
import scala.actors.Actor
import ru.wordmetrix.utils.{CFG, CFGAware, log, debug}
import ru.wordmetrix.utils.ActorDebug.actor2ActorDebug

trait SampleHierarchy2PriorityBase extends Actor

class SampleHierarchy2PriorityStub  extends SampleHierarchy2PriorityBase {
    def act {}
}
