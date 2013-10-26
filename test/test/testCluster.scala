package test
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ru.wordmetrix.webcrawler._
import java.net.URL
import java.net.URI
import scala.util.Random.{ nextBoolean, nextGaussian }
import Math.abs
class testCluster extends FlatSpec with Matchers {

    val v1 = Vector(1 -> 1, 4 -> 0)
    val v21 = Vector(1 -> 1, 5 -> 1)
    val v2 = Vector(2 -> 1, 4 -> 0)
    val v3 = Vector(1 -> 1, 2 -> 1)
    val v4 = Vector(3 -> 1, 4 -> 0)
    val v14 = Vector(3 -> 1, 4 -> 1)
    val v5 = Vector(1 -> 1, 3 -> 1)
    val v6 = Vector(2 -> 1, 3 -> 1)
    val v16 = Vector(2 -> 1, 3 -> 1, 4 -> 1)
    val v7 = Vector(1 -> 1, 2 -> 1, 3 -> 1)
    val v30 = Vector(4 -> 1, 5 -> 1)
    val v60 = Vector(5 -> 1, 6 -> 1)
    val v31 = Vector(1 -> 1, 4 -> 1, 5 -> 1)
    val v32 = Vector(2 -> 1, 4 -> 1, 5 -> 1)

    type V = scala.collection.immutable.Vector[Vector[Int]]
    val V = scala.collection.immutable.Vector

    def dist[F](vs: Vector[F]*) = vs.toList.sliding(2) map {
        case v1 :: v2 :: _ => (v1 - v2).norm
    }

    def av[F](vs: Vector[F]*) = dist(vs: _*).toList match {
        case ds => ds.sum / ds.length
    }

    def d[F](vs: Vector[F]*) = (dist(vs: _*).toList, av(vs: _*)) match {
        case (ds, av) =>  ds.map(x => Math.pow(x - av, 2)).sum / ds.length
    }

    "A cluster" should "be created" in {
        new Cluster().vector should be(V()) 
        new Cluster().dispersion should be(0.0)

        new Cluster(V(v1)).vector should be(V(v1))
        new Cluster(V(v1)).dispersion should be(0.0)
        new Cluster(V(v1)).average should be(0.0)
        new Cluster(V(v1)).average_of_squares should be(0.0)

        new Cluster(V(v1, v2)).vector should be(V(v1, v2))
        new Cluster(V(v1, v2)).dispersion should be(0.0 plusOrMinus 1e-10)
        new Cluster(V(v1, v2)).average should be((v1 - v2).norm)
        new Cluster(V(v1, v2)).average_of_squares should be((v1 - v2).sqr)

        new Cluster(V(v1, v2, v3)).vector should be(V(v1, v2, v3))
        new Cluster(V(v1, v2, v3)).dispersion should be(d(v1, v2, v3) plusOrMinus 0.00001)
        new Cluster(V(v1, v2, v3)).average should be(av(v1,v2,v3)*2)

        new Cluster(v1, v2, v3).vector should be(V(v1, v2, v3))
        new Cluster(v1, v2, v3).dispersion should be(d(v1, v2, v3) plusOrMinus 0.00001)
        new Cluster(v1, v2, v3).average should be(av(v1,v2,v3)*2)

        new Cluster(V(v1, v2, v3, v4)).vector should be(V(v1, v2, v3, v4))
        new Cluster(V(v1, v2, v3, v4)).dispersion should be(d(v1, v2, v3, v4) plusOrMinus 0.00001)
        new Cluster(V(v1, v2, v3, v4)).average should be(av(v1,v2,v3,v4)*3)
    }

    "A cluster" should "be extendable with :+" in {
        new Cluster().vector should be(V()) 
        new Cluster().dispersion should be(0.0)

        new Cluster(v1).vector should be(V(v1))
        new Cluster(v1).dispersion should be(0.0)
        new Cluster(v1).average should be(0.0)
        new Cluster(v1).average_of_squares should be(0.0)

        (new Cluster(v1) :+ v2).vector should be(V(v1, v2))
        (new Cluster(v1) :+ v2).dispersion should be(0.0 plusOrMinus 1e-10)
        (new Cluster(v1) :+ v2).average should be((v1 - v2).norm)
        (new Cluster(v1) :+ v2).average_of_squares should be((v1 - v2).sqr plusOrMinus 0.0001)

        (new Cluster(v1) :+ v2 :+ v3).vector should be(V(v1, v2, v3))
        (new Cluster(v1) :+ v2 :+ v3).dispersion should be(d(v1, v2, v3) plusOrMinus 0.00001)
        (new Cluster(v1) :+ v2 :+ v3).average should be(av(v1,v2,v3)*2)

        (new Cluster(v1) :+ v2 :+ v3 :+ v4).vector should be(V(v1, v2, v3, v4))
        (new Cluster(v1) :+ v2 :+ v3 :+ v4).dispersion should be(d(v1, v2, v3, v4) plusOrMinus 0.00001)
        (new Cluster(v1) :+ v2 :+ v3 :+ v4).average should be(av(v1,v2,v3,v4)*3)
    }
    
    "A cluster" should "be extendable with +:" in {
        new Cluster().vector should be(V()) 
        new Cluster().dispersion should be(0.0)

        new Cluster(v1).vector should be(V(v1))
        new Cluster(v1).dispersion should be(0.0)
        new Cluster(v1).average should be(0.0)
        new Cluster(v1).average_of_squares should be(0.0)

        (v1 +: new Cluster(v2)).vector should be(V(v1, v2))
        (v1 +: new Cluster(v2)).dispersion should be(0.0 plusOrMinus 1e-10)
        (v1 +: new Cluster(v2)).average should be((v1 - v2).norm)
        (v1 +: new Cluster(v2)).average_of_squares should be((v1 - v2).sqr plusOrMinus 0.0001)

        (v1 +: v2 +: new Cluster(v3)).vector should be(V(v1, v2, v3))
        (v1 +: v2 +: new Cluster(v3)).dispersion should be(d(v1, v2, v3) plusOrMinus 0.00001)
        (v1 +: v2 +: new Cluster(v3)).average should be(av(v1,v2,v3)*2)

        (v1 +: v2 +: v3 +: new Cluster(v4)).vector should be(V(v1, v2, v3, v4))
        (v1 +: v2 +: v3 +: new Cluster(v4)).dispersion should be(d(v1, v2, v3, v4) plusOrMinus 0.00001)
        (v1 +: v2 +: v3 +: new Cluster(v4)).average should be(av(v1,v2,v3,v4)*3)
    }
    
    "A cluster" should "have union method" in {
        (new Cluster(v1) union new Cluster(v2)).vector should be(V(v1, v2))
        (new Cluster(v1) union new Cluster(v2)).dispersion should be(0.0 plusOrMinus 1e-10)
        (new Cluster(v1) union new Cluster(v2)).average should be((v1 - v2).norm)
        (new Cluster(v1) union new Cluster(v2)).average_of_squares should be((v1 - v2).sqr plusOrMinus 0.0001)

        (new Cluster(V(v1, v2)) union new Cluster(v3)).vector should be(V(v1, v2, v3))
        (new Cluster(V(v1, v2)) union new Cluster(v3)).dispersion should be(d(v1, v2, v3) plusOrMinus 0.00001)
        (new Cluster(V(v1, v2)) union new Cluster(v3)).average should be(av(v1,v2,v3)*2)

        (new Cluster(V(v1, v2)) union new Cluster(V(v3,v4))).vector should be(V(v1, v2, v3, v4))
        (new Cluster(V(v1, v2)) union new Cluster(V(v3,v4))).dispersion should be(d(v1, v2, v3, v4) plusOrMinus 0.00001)
        (new Cluster(V(v1, v2)) union new Cluster(V(v3,v4))).average should be(av(v1,v2,v3,v4)*3)
    }
    
    "A clusters" should "union if check passed" in {
        println(new Cluster(v3, v1, v3).average)
        println(new Cluster(v2, v1, v3).last)
        println(new Cluster(v2, v1, v3).head)
        println(new Cluster(v3, v2, v3).average)
        
        (new Cluster(v2, v1, v3) unionIfCheck new Cluster(v2, v1, v3)) should not be(None)
        (new Cluster(v1, v1, v3) unionIfCheck new Cluster(v6, v16, v16)) should be(None)

        (new Cluster(v1, v3) unionIfCheck new Cluster(v3, v2)) should not be(None)
        (new Cluster(v1, v3) unionIfCheck new Cluster(v6, v16)) should be(None)
    }
    
}
