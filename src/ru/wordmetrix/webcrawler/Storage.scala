package ru.wordmetrix.webcrawler

import scala.actors.Actor
import ActorDebug._
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.FileInputStream

class Storage()(implicit val cfg: CFG) extends Actor with CFGAware {
    override val name = "Storage"
    var amount = 0
    def seedToFilename(seed: WebCrawler.Seed) = """[/:\\]""".r.replaceAllIn("""https?://""".r.replaceFirstIn(seed.toString, ""), "-") match {
        case x if x.length > 120 => x.slice(0, 120) +
            x.slice(0, 120).hashCode.toString
        case x => x
    }

    def act() = loop {
        react {
            case seed: WebCrawler.Seed => {
                this.log("Datum %s seemed significant", seed)
                val name = seedToFilename(seed)
                val fin = new FileInputStream(new File(new File(cfg.path, "tmp"), name))
                val buf = new Array[Byte](fin.available())
                fin.read(buf)
                fin.close
                val fout = new FileOutputStream(new File(cfg.path, name))
                fout.write(buf)
                fout.close()
            }

            case (seed: WebCrawler.Seed, intel: WebCrawler.Intel) => {
                this.log("Datum %s has come", seed)

                val file = new OutputStreamWriter(
                    new FileOutputStream(
                        new File(new File(cfg.path, "tmp"), seedToFilename(seed))))

                file.write(intel)
                file.close()
                amount += 1
                if (amount >= cfg.amount) {
                    System.exit(0)
                }
            }

            case x => log("Store got something strange: %s", x)
        }
    }

}