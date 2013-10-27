package ru.wordmetrix.webcrawler

object Clusters {
    type V[V] = scala.collection.immutable.Vector[V]
    def V = scala.collection.immutable.Vector

    type Item[F, V] = (Vector[F], V)
    type Pair[F, V] = (Item[F, V], Item[F, V])

    private def fromPairs[F, V](pairs: Iterable[Pair[F, V]]) = pairs.foldLeft(new Clusters[F]()) {
        case (cs, ((v1, va1), (v2, va2))) => cs + (v1, v2)
    }

    def apply[F, V](tree: Iterable[Item[F, V]]): Clusters[F] = {
        fromPairs(pairs(tree))
    }

    def apply[F](chain: Seq[Vector[F]]): Clusters[F] = apply(chain.zipWithIndex)

    def pairs[F, V](tree: Iterable[(Vector[F], V)]) = tree.sliding(2).map({
        case (v1, value1) :: (v2, value2) :: _ =>
            Some(((v1 - v2).sqr, ((v1, value1), (v2, value2))))
        case _ => None
    }).flatten.toList.sortBy(_._1).map(_._2).toSeq

    def pairs[F](vectors: Seq[Vector[F]]) = {
        vectors.sliding(2).filter(_.length == 2).map {
            case scala.collection.immutable.Vector(v1, v2) => (v1, v2)
        }
    }
    def average(x: V[Double]) = x.sum
}
import Clusters._

class Cluster[F](val vector: V[Vector[F]],
                 val squares: Double = 0d) extends Iterable[Vector[F]] {
    def iterator = vector.iterator

    def this(vector: V[Vector[F]]) = this(
        vector.toVector,
        average(pairs(vector).map({ case (x, y) => (x - y).sqr }).toVector)
    )

    def this(vs: Vector[F]*) = this(vs.toVector)

    def edge = Set(vector.head) | Set(vector.last)

    override def head = vector.head
    override def last = vector.last

    val coef = 3

    def n = vector.length - 1
    def dispersion: Double = if (n < 1) 0 else squares / n

    private def factory(v: V[Vector[F]], distance: Double) = new Cluster[F](
        v, squares + distance * distance
    )

    def :+(v: Vector[F]) = factory(vector :+ v, (vector.last - v).norm)

    def +:(v: Vector[F]) = factory(v +: vector, (vector.head - v).norm)

    def check(distance: Double) = {
        n < 1 || distance * distance < dispersion * coef * coef
    }

    def unionIfCheck(that: Cluster[F]) = (this.last - that.head).norm match {
        case x => if (check(x) && that.check(x)) {
            println("join")
            
            Some(union(that, x))
        } else {
            println("discard")
            None
        }
    }

    def union(that: Cluster[F]): Cluster[F] = union(that, (last - that.head).norm)

    def union(that: Cluster[F], distance: Double) = {
        println("Union this: " +  vector )
        println("Union that: " +  that.vector )
        new Cluster[F](
        vector ++ that.vector,
        squares + that.squares + distance * distance
    )}
}

class Clusters[F](
    val heads: Map[Vector[F], Cluster[F]] = Map[Vector[F], Cluster[F]](),
    val lasts: Map[Vector[F], Cluster[F]] = Map[Vector[F], Cluster[F]]())
        extends Iterable[Cluster[F]] {

    def iterator = heads.values.toIterator

    def +(v1: Vector[F], v2: Vector[F]): Clusters[F] = {
        (heads.get(v2), lasts.get(v1)) match {
            case (Some(c1), Some(c2)) =>
                println(1,v1,v2)
                c2.unionIfCheck(c1) match {
                    case Some(c) => new Clusters(
                        (heads - v2) + (c.head -> c),
                        (lasts - v1) + (c.last -> c)
                    )
                    case None => println("!!"); this
                }

            case (Some(c1), None) =>
                println(2,v1,v2)
                (v1 +: c1) match {
                    case c => new Clusters(
                        (heads - v2) + (c.head -> c),
                        lasts + (c.last -> c)
                    )
                }

            case (None, Some(c2)) =>
                println(3,v1,v2)

                (c2 :+ v2) match {
                    case c => new Clusters(
                        heads + (c.head -> c),
                        (lasts - v1) + (c.last -> c)
                    )
                }

            case (None, None) =>
                println(4,v1,v2)

                new Cluster(v1, v2) match {
                    case c => new Clusters(
                        heads + (v1 -> c),
                        lasts + (v2 -> c)
                    )
                }
        }
    }
}
