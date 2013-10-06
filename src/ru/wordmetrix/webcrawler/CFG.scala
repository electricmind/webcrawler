package ru.wordmetrix.webcrawler

import java.io.File
import scala.actors.Actor
/*
 * CFG: Object that holds a set of the parameters of current session.
 */


object CFG {
   def apply() = {
       new CFG(new File("/tmp/webcrawler"),false)
   }
}

case class CFG(val path : File, val isdebug : Boolean) {
  def apply(key: String, value : String) = key match {
      case "p" => CFG(new File(value), isdebug)
      case "d" => CFG(path, value == "t")
  }
}

object debug {
    def apply(format : String, p : Any*)(implicit cfg : CFG) = {
        if (cfg.isdebug) println(format.format(p:_*))
    }

    def apply(actor : Actor, format : String, p : Any*)(implicit cfg : CFG) = {
        if (cfg.isdebug) println( "In " + actor + ": " + format.format(p:_*))
    }
}

object ActorDebug {
    implicit def actor2ActorDebug(actor : Actor) = new ActorDebug(actor)
}

import debug.apply

class ActorDebug(actor : Actor) {
    def debug(format : String, p : Any*)(implicit cfg : CFG) = apply(actor, format, p:_*)
}

object log {
    def apply(format : String, p : Any*)(implicit cfg : CFG) = {
         println({if (cfg.isdebug) "->>> " else ""} +  format.format(p:_*))
    }
}
