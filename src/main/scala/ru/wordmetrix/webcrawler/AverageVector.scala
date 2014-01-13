package ru.wordmetrix.webcrawler

import ru.wordmetrix.vector.Vector

class AverageVector[F](v: Vector[F])(implicit ord: Ordering[F]) {
    def this()(implicit ord: Ordering[F]) = this(Vector[F]())
    def +(v1: Vector[F]) = new AverageVector[F](v + v1)
    def -(v1: Vector[F]) = new AverageVector[F](v - v1)
    def vector = v.normal
    def normal = v.normal
}
