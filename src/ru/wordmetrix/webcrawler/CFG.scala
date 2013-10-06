package ru.wordmetrix.webcrawler

import java.io.File
/*
 * CFG: Object that holds a set of the parameters of current session.
 */


object CFG {
   def apply() = {
       new CFG(new File("/tmp/webcrawler"))
   }
}

case class CFG(val path : File) {
  def apply(key: String, value : String) = key match {
      case "p" => CFG(new File(value)) 
  }
}

