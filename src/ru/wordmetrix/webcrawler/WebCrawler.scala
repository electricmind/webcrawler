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
    type Vector = List[(Word,Double)]
    
    override
    def main(args: Array[String]) {
        implicit val cfg = CFG(args.toList)
        val storage = new Storage()
        //val queue = new Queue(storage)
        val queue = new EvaluatePriorityMatrix(storage)
        val gather = new Gather(storage, queue)
//        val webget = new WebGet( queue, gather)
        
        storage.start
        queue.start
        gather.start
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
