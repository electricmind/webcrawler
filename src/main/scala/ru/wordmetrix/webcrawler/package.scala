package ru.wordmetrix

import java.net.URI
package object webcrawler {
    type Seed = URI
    type Page = String
    type Intel = String
    type Word = Int
    type Priority = Double
    type V = ru.wordmetrix.vector.Vector[Word]
    type Item = (Priority, Seed)
    type VItem = (Priority, V)

    def normalize(s: String): URI = normalize(new URI(s))

    def normalize(base: String, s: String): URI =
        normalize(new URI(base), new URI(s))

    def normalize(base: URI, s: String): URI = normalize(base, new URI(s))

    def normalize(base: URI, uri: URI): URI =
        normalize(base.resolve(uri))

    def normalize(uri: URI) =
        new URI(uri.getScheme(), uri.getHost(), uri.getPath(), null).normalize

}