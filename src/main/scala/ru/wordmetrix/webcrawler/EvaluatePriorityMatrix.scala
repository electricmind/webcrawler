package ru.wordmetrix.webcrawler

import ru.wordmetrix.utils.{ CFG, CFGAware, log }
import ru.wordmetrix.utils.ActorDebug.actor2ActorDebug
import WebCrawler.{ Seed, Word }
import scala.collection.mutable.PriorityQueue
import ru.wordmetrix.vector.Vector
import ru.wordmetrix.utils.debug
import java.io.CharArrayReader
import java.net.URI
import scala.Option.option2Iterable
import scala.xml.parsing.NoBindingFactoryAdapter
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import org.xml.sax.InputSource
import akka.actor.{ Actor, ActorRef, Props, actorRef2Scala }
import ru.wordmetrix.features.Features
import ru.wordmetrix.utils.{ CFG, CFGAware, Html2Ascii, debug, log }
import ru.wordmetrix.vector.Vector
import ru.wordmetrix.webcrawler.LinkContext.Feature
import akka.actor.Kill
import akka.actor.PoisonPill

object EvaluatePriorityMatrix {
    abstract sealed class EvaluatePriorityMatrixMessage

    case class EvaluatePriorityMatrixSeed(seed: URI) extends EvaluatePriorityMatrixMessage
    case class EvaluatePriorityMatrixStop extends EvaluatePriorityMatrixMessage

    def props(storage: Props, gather: Props, seedqueue: Props, sample: Props, cfg: CFG): Props =
        Props(new EvaluatePriorityMatrix(storage, gather, seedqueue, sample)(cfg))
}

class EvaluatePriorityMatrix(storageprop: Props,
                             gatherprop: Props,
                             seedqueueprop: Props,
                             sampleprop: Props)(implicit cfg: CFG) extends Actor
        with CFGAware {
    override val name = "Evaluate . Matrix"

    type Priority = Double
    type V = ru.wordmetrix.vector.Vector[Word]
    type Item = (Priority, Seed)
    type VItem = (Priority, V)

    import EvaluatePriorityMatrix._
    import SeedQueue._
    import Gather._
    import Storage._
    import SampleHierarchy2Priority._
    val ns = Iterator.from(1)
    var priorities = Map[Seed, (Priority, Set[Seed])]()

    var vectors = Map[Seed, (V, Set[Seed])]()

    val queue = new PriorityQueue[Item]()(
        Ordering.fromLessThan((x: Item, y: Item) => x._1 < y._1))

    val gather = context.actorOf(gatherprop, "Gather")

    val seedqueue = context.actorOf(seedqueueprop, "SeedQueue")
    

    val storage = context.actorOf(storageprop, "Storage")

    val sample = context.actorOf(sampleprop, "Sample")

    gather ! GatherLink(storage, sample)
    
    seedqueue ! SeedQueueLink(gather)

    storage ! StorageVictim(seedqueue)

    var factor = new V(List())

    var newfactor = new V(List())

    var target = new TargetVector[String](n = cfg.targets)

    var average = new AverageVector[String]()

    var limit = 0.90

    def calculate(factor: V, vectors: Map[Seed, (V, Set[Seed])]) =
        vectors.map({
            case (seed, (vector, seeds)) => {
                val priority = vector * factor.normal
                seeds.map((_, priority))
            }
        }).flatten.groupBy(x => x._1).map({
            case (seed, ps) =>
                seed -> ps.map(_._2)
        }).map({
            case (seed, ps) =>
                seed -> ((combinepolicy(ps), priorities(seed) /*.getOrElse(seed, (0d, Set[Seed]()))*/ ._2))
        })

    def combinepolicy(priorities: Iterable[Double]) = priorities.max

    var amount = 4

    var central = new V(List())

    def enqueue(seeds: Set[Seed], seed: Seed, v: V) = debug.time("enqueue") {
        vectors = vectors + (seed -> (v, seeds))
        for (item <- seeds) {
            val qq = (
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
            //            this.log("QQ %s %s",qq._1,qq._2)
            priorities = priorities + qq
        }

        /*        if (queue.isEmpty) {
            this.log("Empty queue, beg dispatcher for webget")
            this ! dispatcher
        }*/

        queue.clear()

        for ((seed, (p, seeds)) <- priorities) {
            queue.enqueue((p, seed))
        }
    }

    def estimate(seed: Seed, v: V) = debug.time("estimate, average size: %s, target size: %s / %s".format(average.vector.size, target.average.vector.size, target.vs.length)) {
        average = average + v
        target = target + (v, {
            this.debug("accepted %s with %s in %s", seed, v * target.average.normal, target.priority())
            storage ! StorageSign(seed)
        })

        newfactor = target.normal - average.normal

        this.debug("direction = %s %s",
            (target.normal - average.normal) * central, factor * central)

        this.debug("limit? %s < %s",
            newfactor.normal * factor.normal, limit
        )
    }

    def receive(): Receive = {
        case EvaluatePriorityMatrixSeed(seed: Seed) => {
            this.log("Initial seed: %s", seed)
            //gather ! GatherLink(storage, sample)
            seedqueue ! SeedQueueRequest(seed)
            context.become(phase_initialization, false)
        }
    }

    def phase_initialization(): Receive = {
        
        case msg @ Gather.GatherLinkContext(_,_) =>  sample ! msg
        
        case Gather.GatherSeeds(seed, seeds, v) => {
            ns.next()
            //Initialization
            val v1 = v.normal
            this.log("Initial phase, v = %s, n = %s, seed = %s",
                v.norm, seeds.size, seed)
            central = v1
            target = target + v1
            average = average + v1

            for (seed <- seeds.toList.sorted) {
                seedqueue ! SeedQueueRequest(seed)
            }

            storage ! StorageSign(seed)
            this.log("Start targeting " + target.vs.length)
            context.become(phase_targeting, false)

        }
    }

    def phase_targeting(): Receive = {
        //Accumulate initial vectors for target
        case SeedQueueGet => {
            this.log("Targeting impossible, too little casualties")
            //TODO: Initial SEED can contain too little links for estimation phase
        }

        case Gather.GatherSeeds(seed, seeds, v) => {
            this.debug(s"Targeting phase $seed")
            ns.next()

            estimate(seed, v.normal)
            this.log("Targeting phase, attitude = %s, n = %s, " +
                "seed = %s",
                newfactor * central, seeds.size, seed)

            this.debug("target - average| = %s",
                (target.normal - average.normal).norm)

            this.debug("target*central = %s",
                target.normal * central)

            factor = newfactor
            this.debug("factor*central = %s (required %s)",
                factor * central, cfg.targeting)

            enqueue(seeds, seed, v)
            queue.clear() //TODO: Why?

            if (factor * central > cfg.targeting) {
                priorities = calculate(newfactor, vectors)
                //target = new TargetVectorCluster[String](target, n = cfg.targets)

                this.log("Turn into estimation phase")
                /*for (seed <- priorities.keys.take(10)) {
                     //val (p, seed) = queue.dequeue
                     //val (_, seeds) = priorities(seed)
                     //priorities = priorities - seed
                     this.log("webget %s", seed)
                     context.actorOf(WebGet.props(cfg)) ! SeedQueueRequest(seed,gather)
                }*/
                seedqueue ! SeedQueueAvailable
                context.become(phase_estimating, false)
            }
        }
    }

    def phase_estimating(): Receive = {
        case EvaluatePriorityMatrixStop =>
            context.system.shutdown()
            //context.stop(self)
            
        case Gather.GatherSeeds(seed, seeds, v) => {
            //Do work
            if (ns.next() > cfg.limit) {
                this.log("Limit has been reached")
                //context.system.shutdown()         
                sample ! EvaluatePriorityMatrixStop
                seedqueue ! EvaluatePriorityMatrixStop
            } else {
                this.log("Estimating phase,  attitude = %s, priority = %s, seed = %s",
                    newfactor * central, factor * v.normal, seed)
                estimate(seed, v.normal)
                if (newfactor.normal * factor.normal < limit) {
                    this.debug("Priorities should be recalculated")
                    priorities = calculate(newfactor, vectors)
                    factor = newfactor
                }

                enqueue(seeds, seed, v)
                sample ! SampleHirarchy2PriorityPriority(seed, factor * v.normal)
                seedqueue ! SeedQueueAvailable
            }
        }

        case SeedQueueGet => {
            this.debug("Get dispather request %s", sender)
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
                this.log("Request, priority = %s for %s : %s", p,
                    seed, seeds.headOption.getOrElse("empty"))
                //TODO: check continue of estimation  phase
                sender ! SeedQueueRequest(seed)
            } else {
                this.debug("Queue was empty")
            }
        }
        case x => this.debug("!!!! Unknown message %s from %s",
            x, sender)

    }
}
