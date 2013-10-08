package ru.wordmetrix.webcrawler

import scala.actors.Actor
import WebCrawler.{ Seed, Word }
import scala.collection.mutable.PriorityQueue
import ActorDebug._

class EvaluatePriorityMatrix(storage: Storage)(implicit cfg: CFG) extends Actor {
    type Priority = Double
    type V = Vector[Word]
    type Item = (Priority, Seed)
    type VItem = (Priority, V)

    var priorities = Map[Seed, (Priority, Set[Seed])]()

    var vectors = Map[Seed, (V, Set[Seed])]()

    val queue = new PriorityQueue[Item]()(
        Ordering.fromLessThan((x: Item, y: Item) => x._1 < y._1))

    class AverageVector(v: V = new V(List())) {
        def +(v1: V) = new AverageVector(v + v1)
        def -(v1: V) = new AverageVector(v - v1)
        def vector = v.normal
        def normal = v.normal
    }

    class TargetVector(average: AverageVector = new AverageVector(),
                       val vs: List[(Priority, V)] = List[(Priority, V)](),
                       n: Int = 9) {
        log("vs: %s", vs.length)

        def factory(average: AverageVector, vs: List[(Priority, V)]) =
            new TargetVector(average, vs.map(_._2).map(x => (average.normal * x, x)))

        def +(v1: V): TargetVector = {
            val priority = v1.normal * average.normal
            if (vs.length > n) {
                ((priority, v1) :: vs).sortBy(_._1) match {
                    case (p, v) :: vs => if (v == v1) {
                        this
                    } else {
                        factory(average - v + v1, vs)
                    }
                }
            } else {
                factory(average + v1, (priority, v1) :: vs)
            }
        }
        def priority = vs.headOption match {
            case Some((p, v)) => p
            case None         => 0d
        }

        def normal = average.normal
    }

    val dispatcher = new Dispatcher(this) {
        start
    }
    var factor = new V(List())

    var newfactor = new V(List())

    var target = new TargetVector()

    var average = new AverageVector()

    var limit = 0.01

    def calculate(factor: V, vectors: Map[Seed, (V, Set[Seed])]) =
        vectors.map({
            case (seed, (vector, seeds)) => {
                val priority = vector * factor.normal
                seeds.map((_, priority))
            }
        }).flatten.groupBy(_._1).map({
            case (seed, ps) =>
                seed -> ps.map(_._2)
        }).map({
            case (seed, ps) =>
                seed -> ((combinepolicy(ps), priorities.getOrElse(seed, (0d, Set[Seed]()))._2))
        })

    def combinepolicy(priorities: Iterable[Double]) = {
        //                log("combine %s %s", priorities.size, priorities.max)
        priorities.max
    }

    var amount = 4

    def act() = loop {
        react {

            case (seed: Seed) => {
                this.log("Inital seed: %s", seed)
                dispatcher ! seed
            }
            case (seeds: Set[Seed], seed: Seed, v: V) => {
                this.log("Seed & seeds: %s", seed)
                val v1 = v.normal
                if (factor.norm < 0.0008) {
                    factor = v1
                }
                average = average + v1
                target = target + v1

                if (target.vs.length == 1) {
                    for (seed <- seeds) {
                        dispatcher ! seed
                    }
                } else {
                    this.log("priority %s : %s > %s = %s (%s)", factor * target.normal, v1 * factor, target.priority, v1 * factor > target.priority, target.vs.length)
                    this.log("average * target, newfactor * factor: %s %s", average.normal * target.normal, newfactor.norm)

                    if (v1 * factor >= target.priority) {
                        storage ! seed
                    }

                    newfactor = (target.normal - average.normal) * -1.0

                    vectors = vectors + (seed -> (v1, seeds))

                    for (item <- seeds) {
                        priorities = priorities + (
                            item -> (
                                (
                                    {
                                        val q = combinepolicy(priorities.getOrElse(item, (0d, Set[Seed]()))._2
                                            .map(x => vectors(x)._1 * factor) + v1 * factor); /* log("combine qq:%s %s %s",q,item,seed); */ q
                                    },
                                    priorities.getOrElse(item, (0d, Set[Seed]()))._2 + seed)))
                    }

                    this.log("norm & limit %s %s", newfactor.norm, Math.abs(newfactor.normal * factor.normal))
                    if (newfactor.norm > 0.1 && Math.abs(newfactor.normal * factor.normal) > limit && amount > 0) {
                        amount -= 1;
                        log("do priority")
                        priorities = calculate(newfactor, vectors)
                        factor = newfactor
                    }

                    if (queue.isEmpty) {
                        this.log("Ask dispatcher")
                        this ! dispatcher
                    }
                    queue.clear()
                    for ((seed, (p, seeds)) <- priorities) {
                        queue.enqueue((p, seed))
                    }
                }
            }

            case dispatcher: Dispatcher => {
                this.log("Get dispather request")
                if (!queue.isEmpty) {
                    val (p, seed) = queue.dequeue
                    val (_, seeds) = priorities(seed)
                    priorities = priorities - seed
                    vectors = vectors ++ {
                        seeds.filter(vectors contains _).map(x => x -> {
                            val (vector, seeds) = vectors(x)
                            (vector, seeds - seed)
                        })
                    }
                    this.log("Probability %s for %s : %s", p, seed, seeds)
                    dispatcher ! seed
                }
            }

            case None => {
                this.log("Get None request ")
                this ! dispatcher
            }

            case webget: WebGet => dispatcher ! webget
            case x              => this.log("Unknown message %s from %s", x, sender)

        }
    }
}