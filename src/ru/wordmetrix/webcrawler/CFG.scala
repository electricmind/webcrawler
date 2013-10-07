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
        "servers" -> 2)

    def apply() : CFG = this(List())
    
    def apply(args: Array[String]): CFG = CFG(args.toList)

    def apply(list: List[String], map: Map[String, Any] = default,
              seeds: List[URI] = List()): CFG = list match {
        case rkey("p") :: path :: list =>
            CFG(list, map + ("path" -> new File(path)), seeds)

        case rkey("d") :: list =>
            CFG(list, map + ("isdebug" -> true), seeds)

        case rkey("n") :: value :: list =>
            CFG(list, map + ("servers" -> value.toInt), seeds)

        case rkey(x) :: list => {
            println("Unknown key %s".format(x))
            CFG(list, map, seeds)
        }
        
         case seed :: list =>
         CFG(list, map, new URI(seed) :: seeds)

       case List() => new CFG(
            map("path").asInstanceOf[File],
            map("isdebug").asInstanceOf[Boolean],
            map("servers").asInstanceOf[Int],
            seeds.reverse)
    }
}

class CFG(val path: File, val isdebug: Boolean, val servers: Int, val seeds: List[URI]) {}

object debug {
    def apply(format: String, p: Any*)(implicit cfg: CFG) = {
        if (cfg.isdebug) println(format.format(p: _*))
    }

    def apply(actor: Actor, format: String, p: Any*)(implicit cfg: CFG) = {
        if (cfg.isdebug) println("In " + actor + ": " + format.format(p: _*))
    }
}


object log {
    def apply(format: String, p: Any*)(implicit cfg: CFG) = {
        println({ if (cfg.isdebug) "->>> " else "" } + format.format(p: _*))
    }

    def apply(actor : Actor, format: String, p: Any*)(implicit cfg: CFG) = {
        println({ if (cfg.isdebug) "->>> " else "" } + "In " + actor + format.format(p: _*))
    }
}

object ActorDebug {
    implicit def actor2ActorDebug(actor: Actor) = new ActorDebug(actor)
}

class ActorDebug(actor: Actor) {
    def debug(format: String, p: Any*)(implicit cfg: CFG) = 
        ru.wordmetrix.webcrawler.debug(actor, format, p: _*)
    def log(format: String, p: Any*)(implicit cfg: CFG) = 
        ru.wordmetrix.webcrawler.log(actor, format, p: _*)
}
