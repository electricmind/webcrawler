package test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ru.wordmetrix.webcrawler._

class testVector extends FlatSpec with Matchers {
    "A vector" should "be created" in {
        Vector(("A",1d))
    }

    "A vector" should "support addiniton" in {
        Vector(("A",1d)) + Vector(("A",1d)) should be (Vector(("A",2d)))
        Vector(("B",1d)) + Vector(("A",1d)) should be (Vector(("A",1d),("B",1d)))
        Vector(("A",1d)) + Vector(("B",1d)) should be (Vector(("A",1d),("B",1d)))
        Vector(("A",1d),("C",1d),("D",1d)) + Vector(("B",1d)) should be (Vector(("A",1d),("B",1d),("C",1d),("D",1d)))
        Vector(("A",1d)) + Vector(("B",1d),("C",1d),("D",1d)) should be (Vector(("A",1d),("B",1d),("C",1d),("D",1d)))
        Vector(("E",1d),("F",1d),("D",1d)) + Vector(("B",1d)) should be (Vector(("E",1d),("F",1d),("D",1d),("B",1d)))
        Vector(("E",1d)) + Vector(("B",1d),("F",1d),("D",1d)) should be (Vector(("E",1d),("F",1d),("B",1d),("D",1d)))
        Vector(("A",1d),("C",1d),("D",1d)) + Vector(("A",-1d)) should be (Vector(("C",1d),("D",1d)))
        
    }
    "A vector" should "support substraction" in {
        Vector(("A",1d)) - Vector(("A",0.5d)) should be (Vector(("A",0.5d)))
        Vector(("B",1d)) - Vector(("A",1d)) should be (Vector(("A",-1d),("B",1d)))
        Vector(("A",1d)) - Vector(("B",1d)) should be (Vector(("A",1d),("B",-1d)))
        Vector(("A",1d),("C",1d),("D",1d)) - Vector(("B",1d)) should be (Vector(("A",1d),("B",-1d),("C",1d),("D",1d)))
        Vector(("A",1d)) - Vector(("B",1d),("C",1d),("D",1d)) should be (Vector(("A",1d),("B",-1d),("C",-1d),("D",-1d)))
        Vector(("E",1d),("F",1d),("D",1d)) - Vector(("B",1d)) should be (Vector(("E",1d),("F",1d),("D",1d),("B",-1d)))
        Vector(("E",1d)) - Vector(("B",1d),("F",1d),("D",1d)) should be (Vector(("E",1d),("F",-1d),("B",-1d),("D",-1d)))
        Vector(("A",1d),("C",1d),("D",1d)) - Vector(("A",1d)) should be (Vector(("C",1d),("D",1d)))
    }

    "A vector" should "support inner product" in {
        Vector(("A",1d)) * Vector(("A",0.5d)) should be (0.5)
        Vector(("B",1d)) * Vector(("A",1d)) should be (0d)
        Vector(("A",1d)) * Vector(("B",1d)) should be (0d)
        Vector(("A",1d),("C",1d),("D",1d)) * Vector(("B",1d)) should be (0d)
        Vector(("A",1d)) * Vector(("B",1d),("C",1d),("D",1d)) should be (0d)
        Vector(("E",1d),("F",1d),("D",1d)) * Vector(("B",1d)) should be (0d)
        Vector(("A",1d)) * Vector(("B",1d),("F",1d),("D",1d)) should be (0d)
        Vector(("A",1d),("C",1d),("D",1d)) * Vector(("A",1d)) should be (1d)
    }

    "A vector" should "has a norm" in {
        Vector(("A",1d),("B",1d)).normal() should  be (Vector(("A", 0.7071067811865475),("B",0.7071067811865475)))
    }
    
    "A vector" should "support multiplication" in {
        Vector(("A",1d),("C",1d),("D",1d)) * 2.0 should be (Vector(("A",2d),("C",2d),("D",2d)))
    }

    "A vector" should "support division" in {
        Vector(("A",1d),("C",1d),("D",1d)) / 0.5 should be (Vector(("A",2d),("C",2d),("D",2d)))
    }
    
    
    
    
}