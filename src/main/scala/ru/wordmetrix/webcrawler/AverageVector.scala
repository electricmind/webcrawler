package ru.wordmetrix.webcrawler

import ru.wordmetrix.utils.CFG
import ru.wordmetrix.vector.Vector

/*
* AvarageVector keeps average vector of a cloud, which can be extended by
* addition of new vectors
*
* @param x Vector[F] vector
 */
class AverageVector[F](v: Vector[F])(implicit ord: Ordering[F], cfg: CFG) {

  def +(v1: Vector[F]) = new AverageVector[F](v + v1)

  def -(v1: Vector[F]) = new AverageVector[F](v - v1)

  def vector = v.normal

  def normal = v.normal
}

object AverageVector {
  def apply[F](v: Vector[F])(implicit ord: Ordering[F], cfg: CFG): AverageVector[F] =
    new AverageVector(v)

  def apply[F]()(implicit ord: Ordering[F], cfg: CFG): AverageVector[F] =
    AverageVector(Vector[F]()(cfg.accuracy, ord))
}
