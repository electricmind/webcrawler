package ru.wordmetrix.webcrawler    
import ru.wordmetrix.vector._

class NetworkDump(net: NetworkEstimator) {
    val edges = Iterator.from(1)
    
    def dump(index: EvaluatePriorityMatrix.RevMap[Seed], estimator : SemanticEstimatorBase[_]): String = {
        s"""
        graph
        [
        Creator WebCrawler
        directed 1
        ${
            (for {
                (id, (v, ids)) <- net.vectors
                uri <- index.rmap.get(id)
                
                if ids.size > 0
            } yield {
                s"""
                node 
                [
                  id ${id}
                  label "${Storage.seedToFilename(uri)}"
                  similarity ${estimator.central.normal * v.normal}
                  priority ${estimator.factor.normal * v.normal}
                  dimension ${v.size}
                length ${v.norm} 
                ]
                """
            }) mkString ("\n")
        }
        ${
            (for {
                (id1, (v1, ids)) <- net.vectors
                id2 <- ids
                //if (net.vectors contains id2)
                //if ! (net.priorities contains id2)
                (v2, ids2) <- net.vectors.get(id2) orElse Some(
                    (VectorHASH.empty[Word], Set)
                )
                
                
            } yield {
                s"""
                edge 
                [
                  id ${edges.next}
                  source ${id1}
                  target ${id2}
                  value ${(v1 - v2).norm}
                  angle ${v1.normal * v2.normal}
                  exist ${ (net.priorities contains id2) }
                  
                ]
                """
            }) mkString ("\n")
        }
        
        ]
        """".split("\n").map(_.trim).mkString("\n")
    }
}
