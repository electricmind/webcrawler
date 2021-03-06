package ru.wordmetrix.webcrawler

import akka.actor.{ActorSystem, Props, actorRef2Scala}
import ru.wordmetrix.utils._
import ru.wordmetrix.webcrawler.Gather.GatherAllow

/*
 * WebCrawler is a base web crawler application.
 */

object WebCrawler extends App {

  override def main(args: Array[String]) {
    implicit val cfg = CFG(args.toList)
    val system = ActorSystem("mySystem")

    val storageprop = Storage.props(cfg)
    val sampleprop: Props = if (cfg.ish2p)
      SampleHierarchy2Priority.props(cfg)
    else
      SampleHierarchy2PriorityStub.props(cfg)

    val gatherprop: Props = Gather.props(cfg)

    val webgetprop = WebGet.props(cfg)
    val seedqueueprop = SeedQueue.props(webgetprop, cfg)

    val gmlprop = GMLStorage.props(cfg)

    val linkedvectorsprop = LinkedVectorsStorage.props(cfg)

    val queueprop: Props = EvaluatePriorityMatrix.props(
      storageprop, gatherprop, seedqueueprop, sampleprop, gmlprop,
      linkedvectorsprop, cfg
    )

    val queue = system.actorOf(queueprop, "queue")

    for (seed <- cfg.hosts) {
      queue ! GatherAllow(seed)
    }

    queue ! EvaluatePriorityMatrix.EvaluatePriorityMatrixSeed(cfg.seeds.toSet)
  }
}
