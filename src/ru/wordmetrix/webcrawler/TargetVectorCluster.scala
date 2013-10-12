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

    lazy val Sigma = Math.sqrt(vs.map({ case (p, x) => p }).sum / vs.length) 

    //    override
    def +>(v: Vector[F]) = {
        val t = new TargetVectorCluster[F](average + v, (0d, v) :: vs, n)
        if (t.Sigma <= Sigma) t else this
    }
    override def priority(v: Vector[F]) = (normal - v) match { case x => Math.sqrt(x * x) }
    override def +(v: Vector[F]) = {
        val p = priority(v)
        if (vs.length < 2) {
            new TargetVectorCluster[F](average + v, (p, v) :: vs, n)
        } else {
            /* 
             * 1 - 68%
             * 2 - 95.5%
             * 3 - 99.5%
             * */
            println("new D: %s > %s", Sigma, p)

            if (0.6 * Sigma  > p) {
                println("add: %s", vs.length + 1)
                new TargetVectorCluster[F](average + v, (p, v) :: vs, n)
            } else {
                this
            }
        }
    }
}