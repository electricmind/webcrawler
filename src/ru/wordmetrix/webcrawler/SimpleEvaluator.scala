package ru.wordmetrix.webcrawler

class SimpleEvaluator()(implicit cfg: CFG) extends EvaluatePriority() {
    // TODO: recalculate and reschedule one vecto
    var central = new V(List())
    var average = new V(List())

    def evaluate(item: Item): Item = item match {
        case (_, seed, seeds, v) => 
            ((central.normal - average.normal) * v, seed, seeds, v)
    }
    
    def add(v : V) = {
        average = average + v
    }
    
    def consume(v : V) {
        central = central + v
    }
}

class MatrixEvaluator()(implicit cfg: CFG) extends EvaluatePriority() {
    // TODO: recalculate and reschedule one vecto
    var central = new V(List())
    var average = new V(List())
    
    def evaluate(item: Item): Item = item match {
        case (_, seed, seeds, v) => 
            ((central.normal - average.normal) * v, seed, seeds, v)
    }
    
    def add(v : V) = {
        average = average + v
    }
    
    def consume(v : V) {
        central = central + v
    }
}
