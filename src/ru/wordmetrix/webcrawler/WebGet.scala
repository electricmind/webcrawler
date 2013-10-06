package ru.wordmetrix.webcrawler

import scala.actors.Actor
/*
 * WebGet sucks an information from webpages 
 */
class WebGet(queue: Queue, gather: Gather)(implicit cfg: CFG) extends Actor {
    def act() = {
        loop {
            react {
                case seed: WebCrawler.Seed => {
                    println("webget: " + seed)
                    val connection = seed.toURL.openConnection()
                    try {
                    connection.getContentType().split(";").head match {
                        case "text/html" => {
                            gather ! (
                                seed,
                                io.Source.fromInputStream(
                                    connection.getInputStream()).
                                    getLines().mkString(""))
                        }
                        case _ => None
                    }
                    Thread.sleep(200)
                } catch { case x => println(x)}
                    queue ! this
                }
            }
        }
    }
}