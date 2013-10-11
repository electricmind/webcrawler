package ru.wordmetrix.webcrawler

import WebCrawler.Seed
import scala.actors.Actor
import scala.collection.mutable.PriorityQueue

//TODO: EvaluatePriorityLinear()
abstract class EvaluatePriorityLinear()(implicit cfg: CFG) extends Actor {
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
                // TODO: add new seed into priority queue
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

