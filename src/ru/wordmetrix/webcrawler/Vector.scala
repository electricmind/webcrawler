package ru.wordmetrix.webcrawler

import scala.collection.TraversableProxy
import scala.math.Ordering.StringOrdering

object Vector {
    
    def apply[F](list : List[(F,Double)])(implicit ord : Ordering[F]) = 
        new Vector(
                list.groupBy(_._1).map({case (x,y) => x -> y.map(_._2).sum}).toList.sortBy(_._1)
            ) 
    def apply[F](pairs : (F,Double)*)(implicit ord : Ordering[F]) : Vector[F] = apply(pairs.toList)
}

class Vector[F](val self: List[(F, Double)])(implicit accuracy : Double= 0.0001, ord : Ordering[F]) extends TraversableProxy[(F, Double)] {
    type Pair = (F, Double)
    type Pairs = List[Pair]
    
    def this( )(implicit accuracy : Double= 0.0001, ord : Ordering[F]) = this(List())
    
    def +(v: Vector[F]) = new Vector(pairs(self, v.self).map({
        case (f,(d0,d1)) => (f,d0 + d1)       
    }).filter(filter)) //.sortBy(_._1))
      

    def -(v: Vector[F]) = new Vector(pairs(self, v.self).map({
        case (f,(d0,d1)) => (f,d0 - d1)       
    }).filter(filter)) //.sortBy(_._1))

    def *(v: Vector[F]) = pairs(self, v.self).map({
        case (f,(d0,d1)) => d0 * d1       
    }).sum

    private def filter(pair : Pair) = Math.abs(pair._2) > accuracy
    
    private def pairs(ps1: Pairs, ps2: Pairs, outcome: List[(F, (Double, Double))] = List()): List[(F, (Double, Double))] = (ps1, ps2) match {
        
        case ((f1, d1) :: pst1, (f2, d2) :: pst2) => if (ord.gt(f1, f2)) {
            pairs(ps1, pst2, (f2, (0d, d2)) :: outcome)
        } else if (ord.lt(f1,f2)) {
            pairs(pst1, ps2, (f1, (d1, 0d)) :: outcome)
        } else {
            pairs(pst1, pst2, (f1, (d1, d2)) :: outcome)
        }

        case (pst, List()) => outcome.reverse ++ pst.map({case (f,d : Double) => (f,(d,0d))})
        case (List(), pst) => outcome.reverse ++ pst.map({case (f,d : Double) => (f,(0d,d))})
    }

    def *(z: Double) = {
        println(z)
        new Vector(map({ case (x, y) => (x, y * z) }).toList)
    }

    def /(z: Double) = {
        new Vector(map({ case (x, y) => (x, y / z) }).toList)
    }

    lazy val norm = Math.pow(map(_._2).map(Math.pow(_, 2)).sum, 0.5)
    def normal() = {
        this / norm 
    }

}
