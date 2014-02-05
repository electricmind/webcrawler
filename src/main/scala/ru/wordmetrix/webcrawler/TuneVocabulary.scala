package ru.wordmetrix.webcrawler

import ru.wordmetrix.utils.CFG
import ru.wordmetrix.smartfile.SmartFile.fromFile
import scala.util.Try
import scala.annotation.tailrec
import ru.wordmetrix.vector.Vector
import scala.util.Random
import ru.wordmetrix.vector.VectorHASH

object TuneVocabulary {

    def learn(net: VS, delta : VS, v1: Set[String], v2: Set[String], haslink: Boolean) = {
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
            case (delta, err) => (net + delta*0.01, err)
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

    def learnprocess(net: VS, sample: List[(Set[String], Set[String], Boolean)], n: Int, bestnet: VS, besterr: Int): VS = if (n > 0) {
        (learnstage(net, sample) match {
            case (net, err) => (VectorHASH(net.map({case (x,y) => (x, if (y==0) y else if (y>0) y-0.0001*y else y+0.0001*y)}).toList),err)
        }) match {
            case (lnet, err) if err > 0 =>
                println(f"Error = ${err}%04d delta = ${lnet.normal * net.normal}%f")
                if (besterr < err)
                    learnprocess(lnet, sample, n - 1, bestnet, besterr)
                else
                    learnprocess(lnet, sample, n - 1, lnet, err)
            case (net, err) if err < besterr => net
            case (net, err)                  => bestnet
        }
    } else net

    def main(argv: Array[String]) {
        implicit val cfg = CFG(argv.toList)

        val root = cfg.path / "vectors"
        val vectors = (for {
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

        val matrix: List[(Int, Int)] = for {
            Array(s1, nodes) <- (root / "matrix.dat" readLines).toList.map(x => x.split(":").map(x => x.trim))
            node1 <- Try(s1.toInt).toOption.toList
            if vectors contains node1
            node2 <- nodes.split(" ").toList.map(
                x => Try[Int](x.trim().toInt).toOption
            ).flatten
            if vectors contains node2
        } yield (node1, node2)
        val s = sample(matrix, vectors) take 200000
        println(s.size)
        val net = learnprocess(VectorHASH(), s, 100, VectorHASH(), Int.MaxValue)
        root / "vocabulary_tuned.dat" write net
    }
}