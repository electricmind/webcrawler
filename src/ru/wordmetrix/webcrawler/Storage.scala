package ru.wordmetrix.webcrawler

import scala.actors.Actor

class Storage()(implicit val cfg : CFG) extends Actor {
    def act() = loop {
        react {
            case seed : WebCrawler.Seed => {
                //TODO: mark page as significant
                
            }
            
            case (seed : WebCrawler.Seed, intell : WebCrawler.Intell) => {
                //TODO : save page in storage
            }
        }
    }

}