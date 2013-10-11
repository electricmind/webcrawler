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

        
}