package ru.wordmetrix.webcrawler

import ru.wordmetrix.webcrawler.AverageVector
import ru.wordmetrix.webcrawler.Vector

class TargetVector[F](val average: AverageVector[F], 
                      val vs: List[(Double, Vector[F])], 
                      n: Int = 9) {
    def this(n: Int = 9)(implicit ord: Ordering[F]) =
        this(new AverageVector[F](), List[(Double, Vector[F])](), n)

    def factory(average: AverageVector[F], vs: List[(Double, Vector[F])]) =
        new TargetVector[F](average, vs.map(_._2).map(x => (average.normal * x, x)))

    def +(v1: Vector[F]): TargetVector[F] = {
        val priority = v1.normal * average.normal
        if (vs.length > n) {
            val l: List[(Double, Vector[F])] =
                ((priority, v1) :: vs).sortBy(_._1)
            l match {
                case (p, v) :: vs => if (v == v1) {
                    this
                } else {
                    factory(average - v + v1, vs)
                }
            }
        } else {
            factory(average + v1, (priority, v1) :: vs)
        }
    }
    def priority = vs.headOption match {
        case Some((p, v)) => p
        case None         => 0d
    }
    def normal = average.normal
}