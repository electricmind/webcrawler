package ru.wordmetrix.webcrawler

import ru.wordmetrix.utils.CFG
import ru.wordmetrix.smartfile.SmartFile.fromFile
import scala.util.Try
import scala.annotation.tailrec
import ru.wordmetrix.vector.Vector
import scala.util.Random
import ru.wordmetrix.vector.VectorHASH
import java.io.File
import ru.wordmetrix.utils.debug

object TuneVocabulary {

    def learn(net: VS, delta: VS, v1: Set[String], v2: Set[String], haslink: Boolean) = {
        val v = VectorHASH((v1 & v2).map(x => x -> 1.0).toList)
        if (haslink) {
            if (net * v > 0) {
                (delta, true)
            } else {
                (delta + v, false)
            }
        } else {
            if (net * v <= 0) {
                (delta, true)
            } else {
                (delta - v, false)
            }
        }
    }

    def learnstage(net: VS, sample: List[(Set[String], Set[String], Boolean)]) = {
        sample.foldLeft((VectorHASH.empty[String], 0)) {
            case ((delta, err), (v1, v2, haslink)) =>
                learn(net, delta, v1, v2, haslink) match {
                    case (delta, true)  => (delta, err)
                    case (delta, false) => (delta, err + 1)
                }
        } match {
            case (delta, err) => (net + delta * 0.01, err)
        }
    }

    def sample(matrix: List[(Int, Int)], vectors: Map[Int, Set[String]]): List[(Set[String], Set[String], Boolean)] = {
        val keys = vectors.keys.toArray
        val m = matrix.toSet
        Random.shuffle(
            (1 to matrix.length).map(
                x => (Random.nextInt(keys.size), Random.nextInt(keys.size))
            ).filterNot(m contains _).map({
                    case (x, y) => (vectors(keys(x)), vectors(keys(y)), false)
                }) ++ matrix.map({ case (x, y) => (vectors(x), vectors(y), true) }) toList
        )
    }

    def learnprocess(net: VS, sample: List[(Set[String], Set[String], Boolean)], test: List[(Set[String], Set[String], Boolean)], n: Int, bestnet: VS, besterr: Int): VS = if (n > 0) {
        (learnstage(net, Random.shuffle(sample) take 100000) match {
            case (net, err) => (VectorHASH(net.map({ case (x, y) => (x, if (y == 0) y else if (y > 0) y - 0.0001 * y else y + 0.0001 * y) }).toList), err)
        }) match {
            case (lnet, err) if err > 0 =>
                val size = 100.0 / Math.min(100000, sample.size)
                println(f"Error = ${err * size}%4.2f%% (${besterr * size}%4.2f%%, ${learnstage(net, test)._2 * 100.0 / test.size}%4.2f%%) delta = ${lnet.normal * net.normal}%f")
                if (besterr < err)
                    learnprocess(lnet, sample, test, n - 1, bestnet, besterr)
                else
                    learnprocess(lnet, sample, test, n - 1, lnet, err)
            case (net, err) if err < besterr => net
            case (net, err)                  => bestnet
        }
    } else net

    def readVectors(root: File) = (for {
        Array(s1, _) <- (root / "matrix.dat" readLines).toList.map(x => x.split(":").map(x => x.trim))
        node1 <- Try(s1.toInt).toOption.toList
    } yield {
        node1 -> (root / s"$node1.dat" readLines).toList.map(x =>
            x.split(":") match {
                case Array(x, _) => Some(x.trim)
                case _           => None
            }
        ).flatten.toSet
    }) toMap

    def readMatrix(root: File, vectors: Map[Int, Set[String]]) =
        for {
            Array(s1, nodes) <- (root / "matrix.dat" readLines).toList.map(x => x.split(":").map(x => x.trim))
            node1 <- Try(s1.toInt).toOption.toList
            if vectors contains node1
            node2 <- nodes.split(" ").toList.map(
                x => Try[Int](x.trim().toInt).toOption
            ).flatten
            if vectors contains node2
        } yield (node1, node2)

    def main(argv: Array[String]) {
        implicit val cfg = CFG(argv.toList)
        val root = cfg.path / "vectors"
        val vectors = readVectors(root)
        val matrix: List[(Int, Int)] = readMatrix(root, vectors)

        val (s, t) = sample(matrix, vectors) take 500000 match {
            case s => s.splitAt(s.size * 90 / 100)
        }
        println(s"Amount of pairs = ${s.size}")

        val net = learnprocess(VectorHASH(), s, t, 100, VectorHASH(),
            Int.MaxValue)
        cfg.path / "vocabulary_tuned.dat" write net
    }
}

object sample {
    def main(argv: Array[String]) {
        implicit val cfg = CFG(argv.toList)
        val root = cfg.path / "vectors"
        val vectors = TuneVocabulary.readVectors(root)
        debug("%d vectors loaded", vectors.size)

        val matrix: List[(Int, Int)] =
            TuneVocabulary.readMatrix(root, vectors)

        debug("matrix loaded")

        val sample: List[(Set[String], Set[String], Boolean)] =
            TuneVocabulary.sample(matrix, vectors) 

        debug("%d samples loaded", sample.size)

        val index = Random.shuffle(sample.foldLeft(Set[String]()) {
            case (set, (v1, v2, _)) => set | v1 | v2
        }.toList).take(40000).zipWithIndex.toMap

        debug("%d words indexed", index.size)
        
        new File(".") / "index.dat" write {
            index.toList.sortBy(_._2) map  {
                case (w,n) => f"${n}%04d : ${w}%s"
            } mkString("\n")
        } 
        
        def count(v : Set[String]) = v.filter(index contains _).size
        
        val sample_filtered = sample.filter({case (v1,v2,_) => 
            count(v1) > 10 && count(v2) > 10 && count(v1 & v2) > 5})

        debug("%d samples filtered", sample_filtered.size)

        val sample_sparse = sample.filter({case (v1,v2,_) => 
            count(v1) > 10 && count(v2) > 10 && count(v1 & v2) > 5     
        }).zipWithIndex.map({case ((v1,v2,_),n) =>
            (v1 & v2).map(index.get).flatten.map((n+1,_))
        }).flatten
        
        debug("%d samples sparsed", sample_sparse.size)
        
        def mapx(v1: Set[String], v2: Set[String]) = {
            val is = (v1 & v2).map(index.get).flatten
            (for {
                i <- 1 to index.size
            } yield {
                if (is(i)) "1" else "0"
            }).mkString(" ")
        }

        val file = new File(".") / "sample.dat"

        file.write(s"""
# Created by Octave 3.2.4, Sun Feb 16 14:54:23 2014 MSK <cray@cray-To-be-filled-by-O-E-M>
# name: sample
# type: struct
# length: 2
# name: X
# type: cell
# rows: 1
# columns: 1
# name: <cell-element>
# type: sparse matrix
# nnz: ${sample_sparse.length}
# rows: ${sample_filtered.length}
# columns: ${index.size}
""")
        for ((i, j) <- sample_sparse.sortBy({case (x,y) => (y,x)})) {
            file.append.write(s"$i $j 1\n")
        }
        file.append.write(s"""
# name: Y
# type: cell
# rows: 1
# columns: 1
# name: <cell-element>
# type: matrix
# rows: ${sample_filtered.length}
# columns: 1
""")

        for ((_, _, islink) <- sample_filtered) {
            file.append.write(if (islink) "1\n" else "0\n")
        }
    }
}