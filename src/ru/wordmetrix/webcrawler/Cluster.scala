package ru.wordmetrix.webcrawler

object Clusters {
    def apply[F, V](tree: TreeApproximator[F, V]) = {
        tree.sliding(2).map({
            case (v1, value1) :: (v2, value2) :: _ =>
                ((v1 - v2).sqr, ((v1, value1), (v2, value2)))
        }).toList.sortBy(_._1).foldLeft(new Clusters[F]()) {
            case (cs, (distance, ((v1, va1), (v2, va2)))) => cs + (v1, v2)
        }
    }
    def pairs[F](vectors: scala.collection.immutable.Vector[Vector[F]]) = {
        vectors.sliding(2).filter(_.length == 2).map {
            case scala.collection.immutable.Vector(v1, v2) => (v1, v2)
        }
    }
    def average(x: scala.collection.immutable.Vector[Double]) = x.sum
}
import Clusters._

class Cluster[F](val vector: scala.collection.immutable.Vector[Vector[F]],
                 val squares: Double = 0d) extends Iterable[Vector[F]] {
    def iterator = vector.iterator

    def this(vector: scala.collection.immutable.Vector[Vector[F]]) = this(
        vector.toVector,
        average(pairs(vector).map({ case (x, y) => (x - y).sqr }).toVector)
    )

    def this(vs: Vector[F]*) = this(vs.toVector)
    //    def this() = this(scala.collection.immutable.Vector())

    def edge = Set(vector.head) | Set(vector.last)

    override def head = vector.head
    override def last = vector.last

    val coef = 2

    def n = vector.length - 1
    def dispersion: Double = if (n < 1) 0 else squares / n 

    private def factory(v: scala.collection.immutable.Vector[Vector[F]], distance: Double) = new Cluster[F](
        v, squares + distance*distance
    )

    def :+(v: Vector[F]) = factory(vector :+ v, (vector.last - v).norm)

    def +:(v: Vector[F]) = factory(v +: vector, (vector.head - v).norm)

    def check(distance: Double) = {
        println("distance, dispersion %s %s".format(distance, dispersion))
        distance * distance < dispersion * coef * coef
    }
    
    def unionIfCheck(that: Cluster[F]) = (this.last - that.head).norm match {
        case x => if (check(x) && that.check(x))
            Some(union(that, x))
        else
            None
    }

    def union(that: Cluster[F]): Cluster[F] = union(that, (last - that.head).norm)

    def union(that: Cluster[F], distance: Double) = new Cluster[F](
        vector ++ that.vector,
        squares + that.squares + distance * distance
    )
}

class Clusters[F](
        val heads: Map[Vector[F], Cluster[F]] = Map[Vector[F], Cluster[F]](),
        val lasts: Map[Vector[F], Cluster[F]] = Map[Vector[F], Cluster[F]]()) extends Iterable[Cluster[F]] {

    def iterator = heads.values.toIterator

    def +(v1: Vector[F], v2: Vector[F]): Clusters[F] = {
        (heads.get(v2), lasts.get(v1)) match {
            case (Some(c1), Some(c2)) => c1.unionIfCheck(c2) match {
                case Some(c) => new Clusters(
                    (heads - v2) + (c.head -> c),
                    (lasts - v1) + (c.last -> c)
                )
                case None => this
            }

            case (Some(c1), None) => (v1 +: c1) match {
                case c => new Clusters(
                    (heads - v2) + (c.head -> c),
                    lasts
                )
            }

            case (None, Some(c2)) => (c2 :+ v2) match {
                case c => new Clusters(
                    heads,
                    (lasts - v1) + (c.last -> c)
                )
            }
        }
    }
}