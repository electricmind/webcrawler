package ru.wordmetrix.webcrawler
import ru.wordmetrix.vector._

class NetworkDump(net: NetworkEstimator) {
    def dump(index: EvaluatePriorityMatrix.RevMap[Seed]): String = {
        s"""
        graph
        [
        Creator WebCrawler
        directed 1
        ${
            (for {
                (id, (priority, ids)) <- net.priorities
                uri <- index.rmap.get(id)
                
                v <- net.vectors.get(id) match {
                    case Some((v, _)) => Some(v)
                    case None         => Some(VectorHASH.empty[Word])
                }
            } yield {
                s"""
                node 
                [
                  id ${id}
                  label ${Storage.seedToFilename(uri)}
                  priority ${priority}
                  dimension ${v.size}
                length ${v.norm} 
                ]
                """
            }) mkString ("\n")
        }
        ${
            (for {
                ((id1, (priority, ids)), id) <- net.priorities.zipWithIndex
                id2 <- ids
                (v1, _) <- net.vectors.get(id1) orElse Some((VectorHASH.empty[Word], Set()))
                (v2, _) <- net.vectors.get(id2) orElse Some((VectorHASH.empty[Word], Set()))
            } yield {
                s"""
                edge 
                [
                  id ${id}
                  source ${id1}
                  target ${id2}
                  value ${(v1 - v2).norm}
                  angle ${v1.normal * v2.normal}
                ]
                """
            }) mkString ("\n")
        }
        
        ]
        """".split("\n").map(_.trim).mkString("\n")
    }
}
