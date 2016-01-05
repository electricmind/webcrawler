package ru.wordmetrix.webcrawler

import ru.wordmetrix.utils.CFG
import ru.wordmetrix.vector._

class TargetVector[F](val average: AverageVector[F],
                      val vs: List[(Double, Vector[F])],
                      n: Int) {
    def this(n: Int = 9)(implicit ord: Ordering[F], cfg: CFG) =
        this(new AverageVector[F](), List[(Double, Vector[F])](), n)

    def factory(average: AverageVector[F], vs: List[(Double, Vector[F])]) =
        new TargetVector[F](
            average,
            vs.map(_._2).map(x => (average.normal * x, x)),
            n)

    def +(v1: Vector[F], callback: => Unit = {}): TargetVector[F] = {
        val priority = v1.normal * average.normal
        if (vs.length > n) {
            ((priority, v1) :: vs).sortBy(_._1) match {
                case (p, v) :: vs => if (v == v1) {
                    this
                } else {
                    callback
                    factory(average - v + v1, vs)
                }
                case List() => this
            }
        } else {
            callback
            factory(average + v1, (priority, v1) :: vs)
        }
    }

    def priority(v: Vector[F]) = normal * v

    def priority() = vs.headOption match {
        case Some((p, v)) => p
        case None         => 0d
    }

    def normal = average.normal
}