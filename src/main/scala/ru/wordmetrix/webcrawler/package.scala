package ru.wordmetrix

import java.net.URI
import ru.wordmetrix.webcrawler.NetworkEstimatorBase
import ru.wordmetrix.webcrawler.NetworkEstimator
import ru.wordmetrix.webcrawler.NetworkDump
package object webcrawler {
    type Seed = URI
    type SeedId = Int
    type Priority = Double
    type Page = String
    type Intel = String
    type Word = Int
    type V = ru.wordmetrix.vector.Vector[Word]
    type Item = (Priority, SeedId)
    type VItem = (Priority, V)

    implicit class NetworkEstimatorDump[NE <: NetworkEstimatorBase[NE]](net: NE) {
        def dump(index: EvaluatePriorityMatrix.RevMap[Seed],
                 estimator: SemanticEstimatorBase[_]) = net match {
            case net: NetworkEstimator =>
                new NetworkDump(net).dump(index, estimator)
            case _ =>
                "This feature have not implemented yet"
        }
    }

    def normalize(s: String): URI = normalize(new URI(s))

    def normalize(base: String, s: String): URI =
        normalize(new URI(base), new URI(s))

    def normalize(base: URI, s: String): URI = normalize(base, new URI(s))

    def normalize(base: URI, uri: URI): URI =
        normalize(base.resolve(uri))

    def normalize(uri: URI) =
        new URI(uri.getScheme(), uri.getHost(), uri.getPath(), null).normalize

}