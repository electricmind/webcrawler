/**
 *
 */
package ru.wordmetrix.webcrawler

import scala.collection.immutable.List
import scala.math.Ordering

import ru.wordmetrix.utils.{ CFG, log }
import ru.wordmetrix.vector.Vector

/**
 * @author cray
 *
 */
class TargetVectorCluster[F](average: AverageVector[F],
                             vs: List[(Double, Vector[F])],
                             n: Int)(implicit ord: Ordering[F], cfg: CFG)
        extends TargetVector[F](average, vs, n) {
    def this(target: TargetVector[F], n: Int)(
        implicit ord: Ordering[F], cfg: CFG) = {
        this(target.average, target.vs, n)
    }
    def this(n: Int = 9)(implicit ord: Ordering[F], cfg: CFG) =
        this(new AverageVector[F], List[(Double, Vector[F])](), n)

    lazy val Sigma = Math.sqrt(vs.map({ case (p, x) => p }).sum / vs.length)

    //    override
    def +>(v: Vector[F]) = {
        val t = new TargetVectorCluster[F](average + v, (0d, v) :: vs, n)
        if (t.Sigma <= Sigma) t else this
    }

    override def priority(v: Vector[F]) = (normal - v).norm

    override def +(v: Vector[F], callback: => Unit = {}) = {
        val p = priority(v)
        if (vs.length < 2) {
            new TargetVectorCluster[F](average + v, (p, v) :: vs, n)
        } else {
            /* 
             * 1 - 68%
             * 2 - 95.5%
             * 3 - 99.5%
             * */

            /*   if (cfg.sigma * Sigma > p) {
                println("add: %s (%s,%s,p)", vs.length + 1, cfg.sigma, Sigma, p)
                callback
                new TargetVectorCluster[F](average + v, (p, v) :: vs, n)
            } else {
                this
            }*/
            val tvc = new TargetVectorCluster[F](average + v, (p, v) :: vs, n)
            log("TargetVectorClaster tvc : this %s < %s", tvc.Sigma, Sigma)
            if (tvc.Sigma < Sigma) {
                callback
                tvc
            } else {
                this
            }

        }
    }
}