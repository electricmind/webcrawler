package ru.wordmetrix.webcrawler

import ru.wordmetrix.utils.CFG

/**
 * An implementation of SemanticEstimator that collects vectors trying to
 * estimate their value in comparison with specimen.
 *
 * @author Elec
 */
class SemanticEstimator(val central: V, val target: TargetVector[Word],
                        val average: AverageVector[Word])(implicit cfg: CFG)
  extends SemanticEstimatorBase[SemanticEstimator]() {

  def this(central: V)(implicit cfg: CFG) = this(
    central = central,
    target = TargetVector[Word](n = cfg.targets) + central,
    average = AverageVector[Word](central)
  )

  lazy val size = target.vs.length

  def copy(central: V = central, target: TargetVector[Word] = target,
           average: AverageVector[Word] = average) =
    new SemanticEstimator(central, target, average)

  /**
   * Estimate how seed was relevant
   *
   * @return target, average, (new)factor
   */
  def estimate(seed: SeedId, v: V, callback: => Unit): SemanticEstimator = {
    val average1 = average + v
    val target1 = target +(v, callback)
    copy(target = target1, average = average1)

    /*
     * val pv = v * sense.target.average.normal;
     * val p = sense.target.priority()
     *
     * debug("Seed %s was accepted as target %s, it's %s < %s",
     *      seed, target.vs.length,
     *      p, pv)
     *
     */
  }

  lazy val factor = target.normal - average.normal
}
