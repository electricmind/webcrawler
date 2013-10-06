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
    
    val rkey = """-(.*)""".r
    
    def parse(args : List[String], cfg : CFG = CFG(), seeds : List[Seed] = List()) : (CFG, List[Seed]) = args match {
//        case rkey(key) :: value :: args => parse(args,cfg(key, value),seeds)
        case seed :: args => parse(args, cfg, new URI(seed) :: seeds)
        case List() => (cfg, seeds)
    }
    override
    def main(args: Array[String]) {
        implicit val (cfg,seeds) = parse(args.toList)
        val storage = new Storage()
        val queue = new Queue(storage)
        val gather = new Gather(storage, queue)
//        val webget = new WebGet( queue, gather)
        
        storage.start
        queue.start
        gather.start
//        webget.start

        for (i <- 1 to 10) {
            val webget = new WebGet(queue, gather)
            webget.start
            queue ! webget
        }
        
        for (seed <- seeds) {
            queue ! seed
        }
    }
}
