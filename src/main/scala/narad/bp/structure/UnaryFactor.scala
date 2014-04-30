package narad.bp.structure

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 9/12/13
 * Time: 3:03 PM
 */
class UnaryFactor(idx: Int, name: String, var pots: Array[Potential]) extends Factor(idx, name) {  //, new UnaryFactorPotential(pots)) {

  def arity = 1

  override def clone = new UnaryFactor(idx, name, pots.map(_.copy))

  def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
    if (verbose) println("Computing message in Factor %s.".format(name))
    val edge = graph.edgesFrom(this).toArray.head
    val omess = edge.f2v
    edge.f2v = dampen(edge.f2v, pots.map(_.value), damp)
    if (edge.f2v.exists(_.isNaN)) {
      System.err.println(" NaN Discovered after update!")
      System.err.println("  - original mess: [%s]".format(omess.mkString(", ")))
      System.err.println("  - pot values: [%s]".format(pots.map(_.value).mkString(", ")))
      System.err.println("  - damp: " + damp)
      System.err.println("  yielding: " + edge.f2v.mkString(", "))
    }
    0.0
  }

  def getBeliefs(graph: FactorGraph): Array[Potential] = {
    val edges = graph.edgesFrom(this).toArray
    assert (edges.size == 1)
    //val tpots = pots.clone()
    val beliefs = elementMultiplication(graph.edgesFrom(this).toArray.head.v2f, pots)
    normalize(beliefs)
    if (beliefs.exists(_.value.isNaN)) {
      System.err.println("Error found in getBeliefs() for %s:".format(name))
      System.err.println("  message = [%s]".format(graph.edgesFrom(this).toArray.head.v2f.mkString(", ")))
      System.err.println("  pots = [%s]".format(pots.map(_.value).mkString(", ")))
    }
    Array(beliefs(1))
  }

  override def clamp() = {
    //System.err.println("Clamping factor %s".format(name))
    pots.foreach { p => if (p.isCorrect) p.value = 1.0 else p.value = 0.0 }
  }

  override def isCorrect = pots(1).isCorrect

}











//  override def peg() = {
//   pots.foreach { p => if (p.isCorrect) p.value = 1.0 else p.value = 0.0 }
//  }


/*
override def peg() = {
  if (pots(0).isCorrect) {
    pots
  }
  pots(0).value = 0.0
  pots(1).value = 1.0
}

override def neg() = {
  pots(0).value = 1.0
  pots(1).value = 0.0
}
*/