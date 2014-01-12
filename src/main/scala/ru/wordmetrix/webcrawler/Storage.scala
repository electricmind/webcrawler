package ru.wordmetrix.webcrawler

import Gather.GatherIntel
import akka.actor.{Actor, Props, actorRef2Scala}
import ru.wordmetrix.smartfile.SmartFile.fromFile
import ru.wordmetrix.utils.{CFG, CFGAware}
import ru.wordmetrix.utils.ActorDebug.actor2ActorDebug
import ru.wordmetrix.utils.Use.anyToUse
import ru.wordmetrix.utils.log
import akka.actor.PoisonPill
import akka.actor.ActorRef
import akka.actor.Kill

object Storage {
     sealed abstract class StorageMessage

     case class StorageSign(seed : WebCrawler.Seed) extends StorageMessage
     case class StorageCompleted() extends StorageMessage
     case class StorageVictim(victim : ActorRef) extends StorageMessage
     
     def props(cfg: CFG): Props =
        Props(new Storage()(cfg))
}


class Storage()(implicit val cfg: CFG) extends Actor with CFGAware {
    override val name = "Storage"
        
    import Storage._    
    import Gather._    
        
    val ns = Iterator.from(1)
    
    def seedToFilename(seed: WebCrawler.Seed) = """[/:\\]""".r.replaceAllIn("""https?://""".r.replaceFirstIn(seed.toString, ""), "-") match {
        case x if x.length > 120 => x.slice(0, 120) +
            x.slice(0, 120).hashCode.toString
        case x => x
    }
    
    def receive() : Receive = {
        case StorageVictim(victim) => 
            context.become(active(victim),false)
    } 
    
    def active(victim : ActorRef) : Receive = {
            case StorageSign(seed) => {
                this.log("Datum %s seemed significant", seed)
                
                seedToFilename(seed) use {
                  name => cfg.path / "tmp" / name copyTo cfg.path / name
                }
            }

            case GatherIntel(seed, intel : String) => {
                val n = ns.next()
                this.log("%04d (%04d)- Datum %s has come", n, cfg.limit, seed)
                
                cfg.path / "tmp" / seedToFilename(seed) write(intel)

                if (n > cfg.limit) {
                    //victim ! StorageCompleted
                    //victim ! PoisonPill
                    //System.exit(0)
                }
            }

            case x => log("Storage got something strange: %s", x)
    }
}
