package ru.wordmetrix.webcrawler

import java.net.URI
/*
 * WebCrawler is a base web crawler application.
 */

object WebCrawler extends App {
    
    type Seed = URI
    type Page = String
    type Intell = String
    type Word = String
    type Priority = Double
    type Vector = List[(Word,Priority)]
    
    def normalize(s : String) : URI = normalize(new URI(s))

    def normalize(base : String, s : String) : URI = 
        normalize(new URI(base), new URI(s))

    def normalize(base : URI, s : String) : URI = normalize(base, new URI(s))

    def normalize(base : URI, uri : URI) : URI = 
        normalize(base.resolve(uri))
    
    def normalize(uri : URI) = 
        new URI(uri.getScheme() + ":" + uri.getSchemeSpecificPart()).normalize
    
    override
    def main(args: Array[String]) {
        implicit val cfg = CFG(args.toList)
        val storage = new Storage()
        //val queue = new Queue(storage)
        val sample = new SampleHierarchy2Priority()
        val queue = new EvaluatePriorityMatrix(storage,sample)
        val gather = new Gather(storage, queue, sample)
//        val webget = new WebGet( queue, gather)
        
        storage.start
        queue.start
        gather.start
        sample.start
//        webget.start

        for (i <- 1 to cfg.servers) {
            val webget = new WebGet(queue, gather)
            webget.start
            queue ! webget
        }
        
        for (seed <- cfg.seeds)  {
            queue ! seed
        }
    }
}
