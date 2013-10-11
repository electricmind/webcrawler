package ru.wordmetrix.webcrawler

import scala.collection.mutable.PriorityQueue
import scala.actors.Actor
import WebCrawler.Seed

abstract class EvaluatePriority()(implicit cfg: CFG) extends Actor {
    type Item = (Double, Seed, Seeds, V)
    type V = Vector[String]
    type Seeds = Set[Seed]

    val queue = new PriorityQueue[Item]()(
        Ordering.fromLessThan((x: Item, y: Item) => x._1 < y._1))

    val previous = new PriorityQueue[Item]()(
        Ordering.fromLessThan((x: Item, y: Item) => x._1 < y._1))

    def evaluate(item: Item): Item
    def add(v : V) : Unit
    def consume(v : V) : Unit

    def act() = loop {
        react {
            case "clone" =>
                queue.map(previous.enqueue(_))
                queue.clear()
                this ! "calc"

            case "calc" if previous.isEmpty => this ! "clone"

            case "calc" if !previous.isEmpty => {
                queue.enqueue(evaluate(previous.dequeue()))
            }

            case (seed: Seed, seeds: Seeds, v: V) => {
          
                add(v)
                queue.enqueue(evaluate((0, seed, seeds, v)))
            }

            case dispatcher: Dispatcher => {
                val (priority, seed, seeds, v) = queue.dequeue()
                consume(v)
                // TODO: elicit new seeds from priority queue
                // dispatcher ! seeds
            }
        }
    }
}
