package ru.wordmetrix.webcrawler

import java.io.File
import java.io.FileOutputStream
import java.io.FileInputStream
object SmartFile {
    implicit def file2SmartFile(f: File) = new SmartFile(f)
    implicit def string2SmartFile(s : String) = new SmartFile(new File(s))
    implicit def string2Array(s: String): Array[Byte] = s.toArray.map(c => c.toByte)
    implicit def smartfile2File(sf : SmartFile) = sf.file

}
import SmartFile._

class SmartFile(val file: File) {
    def /(f: SmartFile) = new SmartFile(new File(file, f.file.toString))
    def /(f: String) = new SmartFile(new File(file, f))
    def /(f: Int) = new SmartFile(new File(file, f.toString))
    def /(f: File) = new SmartFile(new File(file, f.toString))

    def write(ss: Traversable[String]): Unit =
        write(ss.mkString("\n"))

    def write(a: Array[Byte]) = {
        file.getParentFile().mkdirs() //    path.mkdirs()
        val fn = new FileOutputStream(file)
        fn.write(a)
        fn.close()
    }

    def read() = {
        val fin = new FileInputStream(file)
        val buf = new Array[Byte](fin.available())
        fin.read(buf)
        fin.close()
        buf
    }

    def readLines() = io.Source.fromFile(file).getLines

    def copyTo(foutname: SmartFile) = {
        println("%s -> %s".format(file, foutname))
        foutname.write(file.read())
    }

    def write(v: Vector[String]): Unit = write(v.toList.sortBy(-_._2).map {
        case (x, y) => "%-40s : %4.3f".format(x, /*xstring2word.inverted(x)x,*/ y)
    })

    override def toString = file.toString

}
