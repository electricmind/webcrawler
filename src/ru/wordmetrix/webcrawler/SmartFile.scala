package ru.wordmetrix.webcrawler

import java.io.File
import java.io.FileOutputStream
import java.io.FileInputStream
import SmartFile._
import java.io.FileWriter
import java.io.FileReader
import java.io.File
import java.io.ObjectOutputStream
import java.io.ObjectInputStream

object SmartFile {

    implicit def fromFile(f: File) = new SmartFile(f)
    implicit def fromString(s : String) = new SmartFile(new File(s))
    implicit def string2Array(s: String): Array[Byte] = s.toArray.map(c => c.toByte)
    implicit def toFile(sf : SmartFile) = sf.file
}

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

    def writer = new FileWriter(file)
//    def reader = new FileReader(file)
    
    def read() = {
        val fin = new FileInputStream(file)
        val buf = new Array[Byte](fin.available())
        fin.read(buf)
        fin.close()
        buf
    }

    def cache[O](ob : => O) = try {
        val fi = new ObjectInputStream(new FileInputStream(file))
        val o = fi.readObject().asInstanceOf[O]
        fi.close()
        o
    } catch {
        case x : Throwable =>
            val fo = new ObjectOutputStream(new FileOutputStream(file))
            fo.writeObject(ob)
            fo.close()
            ob
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
