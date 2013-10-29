package ru.wordmetrix.webcrawler

import java.io.File
import scala.actors.Actor

import java.net.URI
/*
 * CFG: Object that holds a set of the parameters of current session.
 */

object CFG {
    val rkey = """-(.+)""".r

    val default = Map(
        "path" -> new File("/tmp/webcrawler"),
        "isdebug" -> false,
        "ish2p" -> false,
        "servers" -> 2,
        "targets" -> 9,
        "targeting" -> 0.01,
        "sampling" -> new File("sampling.lst"),
        "amount" -> 1000,
        "sigma" -> 1.0)

    def apply(): CFG = this(List())
    
    def apply(args: Array[String]): CFG = CFG(args.toList)

    def apply(list: List[String], map: Map[String, Any] = default,
              seeds: List[URI] = List()): CFG = list match {
        case rkey("h") :: list =>
            for ((key, value) <- default) {
                println(" -%s = %s".format(key, value))
            }
            scala.sys.exit
            CFG(list, map, seeds)
        case rkey("p") :: path :: list =>
            CFG(list, map + ("path" -> new File(path)), seeds)

        case rkey("d") :: list =>
            CFG(list, map + ("isdebug" -> true), seeds)

        case rkey("ish2p") :: list =>
            CFG(list, map + ("ish2p" -> true), seeds)

        case rkey("n") :: value :: list =>
            CFG(list, map + ("servers" -> value.toInt), seeds)

        case rkey("tl") :: value :: list =>
            CFG(list, map + ("targeting" -> value.toDouble), seeds)

        case rkey("ts") :: value :: list =>
            CFG(list, map + ("targets" -> value.toInt), seeds)

        case rkey("ps") :: path :: list =>
            CFG(list, map + ("samping" -> new File(path)), seeds)

        case rkey("amount") :: value :: list =>
            CFG(list, map + ("amount" -> value.toInt), seeds)

        case rkey("sigma") :: value :: list =>
            CFG(list, map + ("sigma" -> value.toDouble), seeds)

        case rkey(x) :: list => {
            println("Unknown key %s".format(x))
            CFG(list, map, seeds)
        }

        case seed :: list =>
            CFG(list, map, new URI(seed) :: seeds)

        case List() => new CFG(
            map("path").asInstanceOf[File],
            map("sampling").asInstanceOf[File] match {
                case x if x.isAbsolute => x
                case x                 => new File(map("path").asInstanceOf[File], x.getPath)
            },
            map("isdebug").asInstanceOf[Boolean],
            map("ish2p").asInstanceOf[Boolean],
            map("servers").asInstanceOf[Int],
            map("targeting").asInstanceOf[Double],
            map("targets").asInstanceOf[Int],
            map("amount").asInstanceOf[Int],
            map("sigma").asInstanceOf[Double],
            seeds.reverse)
    }
}
/*
 *[04:02] Lynne: a tin of striped paint
*/
class CFG(val path: File, val sampling: File, val isdebug: Boolean, 
          val ish2p: Boolean, val servers: Int,
          val targeting: Double, val targets: Int, val amount: Int, 
          val sigma: Double, val seeds: List[URI]) {}

object debug {
    def apply(format: String, p: Any*)(implicit cfg: CFG) = {
        if (cfg.isdebug) println("  " + format.format(p: _*))
    }

    def apply(actor: CFGAware, format: String, p: Any*)(implicit cfg: CFG) = {
        if (cfg.isdebug) println("  - %20s: ".format(actor.name.slice(0, 20)) + format.format(p: _*))
    }
    def trace[B](s : String = "%s")(f : => B)(implicit cfg : CFG) : B = {
        val outcome = f
        apply(s,outcome)
        outcome
    }
    def time[B](s : String)(f : => B)(implicit cfg : CFG) : B = {
        val t = System.currentTimeMillis() 
        val outcome = f
        apply("%s : %d".format(s,(System.currentTimeMillis() - t) / 1))
        outcome
    }
}

trait CFGAware {
    val name = "CFGAware"
}
object log {
    def apply(format: String, p: Any*)(implicit cfg: CFG) = {
        println({ if (cfg.isdebug) "* " else "" } + format.format(p: _*))
    }

    def apply(actor: CFGAware, format: String, p: Any*)(implicit cfg: CFG) = {
        println({ if (cfg.isdebug) "* " else "" } + "- %20s: ".format(actor.name.slice(0, 20)) + format.format(p: _*))
    }
}

object ActorDebug {
    implicit def actor2ActorDebug(actor: CFGAware) = new ActorDebug(actor)
}
//import ru.wordmetrix.

class ActorDebug(actor: CFGAware) {
    def debug(format: String, p: Any*)(implicit cfg: CFG) =
        ru.wordmetrix.webcrawler.debug(actor, format, p: _*)
    def log(format: String, p: Any*)(implicit cfg: CFG) =
        ru.wordmetrix.webcrawler.log(actor, format, p: _*)
}
