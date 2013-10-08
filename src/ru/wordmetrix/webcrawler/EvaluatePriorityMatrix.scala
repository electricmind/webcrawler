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

    var mode = 0

    var factor = new V(List())

    var newfactor = new V(List())

    var target = new TargetVector()

    var average = new AverageVector()

    var limit = 0.90

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
                seed -> ((combinepolicy(ps), priorities(seed) /*.getOrElse(seed, (0d, Set[Seed]()))*/ ._2))
        })

    def combinepolicy(priorities: Iterable[Double]) = priorities.max

    var amount = 4

    var central = new V(List())

    def enqueue(seeds: Set[Seed], seed: Seed, v: V) = {
        vectors = vectors + (seed -> (v, seeds))
        for (item <- seeds) {
            priorities = priorities + (
                item -> (
                    (combinepolicy(
                        priorities.getOrElse(
                            item,
                            (0d, Set[Seed]())
                        )._2.map(
                                x => vectors(x)._1 * factor) + v * factor
                    ),
                        priorities.getOrElse(item, (0d, Set[Seed]()))._2 + seed
                    )
                )
            )
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

    def act() = loop {
        react {
            case (seed: Seed) => {
                this.log("Inital seed: %s", seed)
                dispatcher ! seed
            }

            case (seeds: Set[Seed], seed: Seed, v: V) => mode = mode match {
                case 0 => {
                    //Initialization
                    val v1 = v.normal
                    this.log("Seed & seeds (0): %s", seed)
                    for (seed <- seeds) {
                        dispatcher ! seed
                    }
                    central = v1
                    target = target + v1
                    average = average + v1
                    storage ! seed
                    1
                }

                case 1 => {
                    //Accumulate initial vectors for target
                    val v1 = v.normal
                    this.log("Seed & seeds (1): %s", seed)
                    target = target + v1
                    average = average + v1

                    if (v1 * factor >= target.priority) {
                        storage ! seed
                    }

                    newfactor = target.normal - average.normal

                    this.debug("target size = %s",target.vs.length)
                    
                    this.debug("mark |target - average| = %s",
                        (target.normal - average.normal).norm)

                    this.debug("mark target*central = %s",
                        target.normal * central)

                    this.debug("mark  direction = %s",
                        (target.normal - average.normal) * central)

                    factor = newfactor
                    enqueue(seeds, seed, v1)
                    
                    queue.clear()

                    if (factor * central > 0.01 ) {
                        priorities = calculate(newfactor, vectors)
                        2
                    } else {
                        1
                    }
                }

                case 2 => {
                    //Do work
                    this.log("Seed & seeds (2) priority: (%s) %s", factor*v.normal, seed)

                    val v1 = v.normal
                    average = average + v1
                    target = target + v1

                    if (v1 * factor >= target.priority) {
                        storage ! seed
                    }

                    //newfactor = //average.normal -v1 //target.normal //
                    newfactor = target.normal - average.normal

                    this.debug("direction = %s %s",
                        (target.normal - average.normal) * central, factor * central)

                    this.debug("limit? %s < %s",
                            newfactor.normal * factor.normal, limit
                        )

                    if (newfactor.normal * factor.normal < limit) {
                        log("do priority")
                        priorities = calculate(newfactor, vectors)
                        factor = newfactor
                    }

                    enqueue(seeds, seed, v1)
                    2
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
//                this.log("Get None request ")
                this ! dispatcher
            }

            case webget: WebGet => dispatcher ! webget
            case x              => this.log("Unknown message %s from %s", x, sender)

        }
    }
}