/**
 *
 */
package ru.wordmetrix.webcrawler

import scala.collection.immutable.List
import scala.math.Ordering

/**
 * @author cray
 *
 */
class TargetVectorCluster[F](average: AverageVector[F],
                             vs: List[(Double, Vector[F])],
                             n: Int)(implicit ord: Ordering[F])
        extends TargetVector[F](average, vs, n) {
    def this(n: Int = 9)(implicit ord: Ordering[F]) =
        this(new AverageVector[F], List[(Double, Vector[F])](), n)

    lazy val D = Math.sqrt(vs.map({case (p,x) => p}).sum)
    
    def +(v : Vector[F]) = {
        val t = new TargetVectorCluster[F](average + v, (0d,v) :: vs, n)
        if (t.D <= D) t else this
    }
    override
    def priority(v : Vector[F]) = (normal-v) match { case x => Math.sqrt(x*x) }
    
    def +>  (v : Vector[F]) = {
        val p = priority(v)
        
        if (D * 2 > p) {
            new TargetVectorCluster[F](average + v, (p, v) :: vs, n)
        } else {
            this
        }
    }
}