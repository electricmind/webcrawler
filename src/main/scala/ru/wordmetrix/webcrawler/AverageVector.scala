package ru.wordmetrix.webcrawler

import ru.wordmetrix.utils.CFG
import ru.wordmetrix.vector.Vector

class AverageVector[F](v: Vector[F])(implicit ord: Ordering[F], cfg: CFG) {
    def this()(implicit ord: Ordering[F], cfg: CFG) = this(Vector[F]()(cfg.accuracy, ord))
    def +(v1: Vector[F]) = new AverageVector[F](v + v1)
    def -(v1: Vector[F]) = new AverageVector[F](v - v1)
    def vector = v.normal
    def normal = v.normal
}
