package ru.wordmetrix.webcrawler

import java.net.URI
import akka.actor.{ActorSystem, Props, actorRef2Scala}
import ru.wordmetrix.utils._

/*
 * WebCrawler is a base web crawler application.
 */

object WebCrawler extends App {

    type Seed = URI
    type Page = String
    type Intel = String
    type Word = String
    type Priority = Double
 
// ActorSystem is a heavy object: create only one per application
    def normalize(s: String): URI = normalize(new URI(s))

    def normalize(base: String, s: String): URI =
        normalize(new URI(base), new URI(s))

    def normalize(base: URI, s: String): URI = normalize(base, new URI(s))

    def normalize(base: URI, uri: URI): URI =
        normalize(base.resolve(uri))

    def normalize(uri: URI) =
        new URI(uri.getScheme(), uri.getHost(), uri.getPath(), null).normalize
        
    //new URI(uri.getScheme() + ":" + java.net.URLEncoder.encode(uri.getSchemeSpecificPart())).normalize 

    override def main(args: Array[String]) {
        implicit val cfg = CFG1(args.toList)
        val system = ActorSystem("mySystem")

        val storageprop = Storage.props(cfg)
        
        //val queue = new Queue(storage)
        
        val sampleprop: Props = if (cfg.ish2p)
            SampleHierarchy2Priority.props(cfg)
        else
            SampleHierarchy2PriorityStub.props(cfg)
        // TODO: move out nulls
            
            // queue storage sample
        val gatherprop : Props  = Gather.props(cfg)
        
        //queue
        val webgetprop = WebGet.props(cfg)
        val seedqueueprop = SeedQueue.props(webgetprop, cfg)
        val queueprop : Props = EvaluatePriorityMatrix.props(storageprop, gatherprop, seedqueueprop, sampleprop, cfg)
        
        val queue = system.actorOf(queueprop, "queue")
        
        for (seed <- cfg.seeds) {
            queue ! EvaluatePriorityMatrix.EvaluatePriorityMatrixSeed(seed)
        }
    }
}
