package ru.wordmetrix.webcrawler

import Gather.GatherIntel
import akka.actor.{ Actor, ActorRef, Props }
import ru.wordmetrix.smartfile.SmartFile.fromFile
import ru.wordmetrix.smartfile.SmartFileAppend
import ru.wordmetrix.utils._

object Storage {
    sealed abstract class StorageMessage

    case class StorageSign(seed: Seed) extends StorageMessage
    case class StorageCompleted() extends StorageMessage
    case class StorageVictim(victim: ActorRef) extends StorageMessage

    def seedToFilename(seed: Seed) =
        """[/:\\]""".r.replaceAllIn(
            """https?://""".r.replaceFirstIn(seed.toString, ""), "-"
        ) match {
                case x if x.length > 120 => x.slice(0, 120) +
                x.slice(0, 120).hashCode.toString
                case x => x
            }

    def props(cfg: CFG): Props =
        Props(new Storage()(cfg))
}

class Storage()(implicit val cfg: CFG) extends Actor with CFGAware {
    override val name = "Storage"

    import Storage._
    import Gather._

    val ns = Iterator.from(1)

    def seedToFilename(seed: Seed) = Storage.seedToFilename(seed)

    def receive(): Receive = {
        case StorageVictim(victim) =>
            context.become(active(victim), false)
    }

    def active(victim: ActorRef): Receive = {
        case StorageSign(seed) => {
            log("Datum %s seemed significant", seed)

            seedToFilename(seed) match {
                case name =>

                    cfg.path / "tmp" / name copyTo cfg.path / name
            }

        }

        case GatherIntel(seed, intel: String) => {
            val n = ns.next()
            log("Datum %04d %s has come", n, seed)

//            val map: List[String] = try {
//                cfg.map readLines () toList
//            } catch {
//                case x => List()
//            }

//            cfg.map.write(
//                s"${seedToFilename(seed)} : $seed" :: map mkString ("\n")
//            )

            cfg.map.append.write(s"${seedToFilename(seed)} : $seed\n")

            cfg.path / "tmp" / seedToFilename(seed) write (intel)
        }
    }
}
