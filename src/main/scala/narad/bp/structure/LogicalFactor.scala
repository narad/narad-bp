package narad.bp.structure

import scala.collection.mutable.ArrayBuffer



class AtMost1Factor(idx: Int, name: String) extends Factor(idx, name) { //}, new UnaryFactorPotential(Array[Double]())) {

  def arity = 10

	def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
		var z = 1.0
		var trues = 0
		if (verbose) println("computing message for AtMost1 %s".format(name))
		for (edge <- graph.edgesFrom(this)) {
			val in = edge.v2f
			z += in(1) / in(0)
			if (in(0) == 0) trues += 1				
		}
		if (verbose) println("Z = " + z + " and trues = " + trues)

		var count = 0
		for (edge <- graph.edgesFrom(this)) {
			val in = edge.v2f
			var mess = Array(z - in(1) / in(0), 1.0)
			if (trues == 1) {
				if (in(0) == 0) {
					mess = Array(0.0, 1.0)
				}
				else {
					mess = Array(1.0, 0.0)
				}
			}
			edge.f2v = dampen(edge.f2v, mess, damp)
		}
		return 0.0
	}

	def getBeliefs(graph: FactorGraph): Array[Potential] = {
		return Array[Potential]()
	}
}


class IsAtMost1Factor(idx: Int, name: String) extends Factor(idx, name) { //}, new UnaryFactorPotential(Array[Double]())) {

  def arity = 10

	def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
		var z = 0.0
		var trues = 0
		var count = 0
		if (verbose) println("computing message for IsAtMost1 %s".format(name))
//		val edges = graph.edgesFrom(this).toArray
//		if (verbose && name == "isAtMost1(0,16)") println("here!")
		for (edge <- graph.edgesFrom(this)) {
			val in = edge.v2f
			if (count == 0) {
				if (verbose) println("first message from " + edge.variable.name + " = " + in(0) + "/" + in(1))
				z = in(0) / in(1)				
			}
			else {
				if (verbose) println("+= " + in(1) + "/" + in(0))
				z += in(1) / in(0)
				if (in(0) == 0) trues += 1				
			}
			count += 1
		}
		if (verbose) println("Z = " + z + " and trues = " + trues)
		count = 0
		for (edge <- graph.edgesFrom(this)) {
			val in = edge.v2f
			if (count == 0) {
				var mess = if (trues == 1) {
					Array(0.0, 1.0)
				}
				else if (z == Double.PositiveInfinity){
					Array(1.0, 0.0)
				}
				else {
					Array(1.0, z - in(0) / in(1))
				}
				if (verbose) println("step 1 mess = " + mess.mkString(", "))
				if (verbose) println("-->" + edge.variable.name)
				edge.f2v = dampen(edge.f2v, mess, damp)				
			}
			else {
				var mess = if (trues == 1 && in(0) == 0) {
					Array(0.0, 1.0)
				}
				else if (trues == 1 || z == Double.PositiveInfinity) {
					Array(1.0, 0.0)
				}
				else {
					Array(z - in(1) / in(0), 1.0)
				}
				if (verbose) println("step 2 mess = " + mess.mkString(", "))
				edge.f2v = dampen(edge.f2v, mess, damp)
			}
			count += 1
		}
		return 0.0
	}

	def getBeliefs(graph: FactorGraph): Array[Potential] = {
		return Array[Potential]()
	}
}


class NandFactor(idx: Int, name: String, pots: Array[Array[Potential]]) extends Table2Factor(idx, name, pots) {


	override def getBeliefs(graph: FactorGraph): Array[Potential] = {
		val b = super.getBeliefs(graph)
		val sum = b.foldLeft(0.0)(_+_.value)
		val rpots = b.filter(_.name != "n/a")
		assert(rpots.size == 1, "Did not find expected number of pots (1) in NandFactor.")
		val rpot = rpots(0)
		rpot.value = rpot.value / sum
		return Array(rpot)  // pots[name_] = a(1,1) / sum(a);
	}
	
	override def peg = {
		pots(0)(0).value = 0.0
		pots(0)(1).value = 0.0
		pots(1)(0).value = 0.0
		pots(1)(1).value = 1.0
	}

  override def clamp() = if (isCorrect) peg

  override def isCorrect = pots(1)(1).isCorrect
}

class ImpliesFactor(idx: Int, name: String, pots: Array[Array[Potential]]) extends Table2Factor(idx, name, pots) {


	override def getBeliefs(graph: FactorGraph): Array[Potential] = {
		val b = super.getBeliefs(graph)
		val sum = b.foldLeft(0.0)(_+_.value)
		val rpots = b.filter(_.name != "n/a")
		assert(rpots.size == 1, "Did not find expected number of pots (1) in NandFactor.")
		val rpot = rpots(0)
		rpot.value = rpot.value / sum
		return Array(rpot)  // pots[name_] = a(1,1) / sum(a);
	}
	
	override def peg = {
		pots(0)(0).value = 0.0
		pots(0)(1).value = 0.0
		pots(1)(0).value = 1.0
		pots(1)(1).value = 0.0
	}
}

class EPUFactor(idx: Int, name: String, pots: Array[Array[Potential]]) extends Table2Factor(idx, name, pots) {

	override def getBeliefs(graph: FactorGraph): Array[Potential] = Array[Potential]()
	
}




/*
class ImpliesFactor : public Table2Factor {
public:
  ImpliesFactor(const string& name, double pot) : Table2Factor(name) {
    pots_.resize(2,2);
    if ( pot == R_PosInf ) {
      pots_ = 0.0;
      pots_(1,0) = 1.0;
    } else {
      pots_ = 1.0;
      pots_(1,0) = pot;
    }
  }
  virtual void append_beliefs(Vertex v, const Graph& g, s2dmap& pots) const {
    Array2 a(get_beliefs(v, g));
    pots[name_] = a(1,0) / sum(a);
  }
  virtual void peg() {
    pots_ = 0.0;
    pots_(1,0) = 1.0;
  }
  virtual void neg() { error("Negging %s not supported", name_.c_str()); }

};
*/

// Not really implemented - just copied from the NAND, which might not be properly implemented
/*
class EPluribusUnumFactor(idx: Int, name: String, arity: Int) extends Table2Factor(idx, name, pots) {
	override def getBeliefs(graph: FactorGraph): Array[Potential] = {
		val b = super.getBeliefs(graph)
		val sum = b.foldLeft(0.0)(_+_.value)
		val rpots = b.filter(_.name != "n/a")
		assert(rpots.size == 1, "Did not find expected number of pots (1) in NandFactor.")
		val rpot = rpots(0)
		rpot.value = rpot.value / sum
		return Array(rpot)  // pots[name_] = a(1,1) / sum(a);
	}
	*/
//	def peg = {
//		pots(0)(0).value = 0.0
//		pots(0)(1).value = 0.0
//		pots(1)(0).value = 0.0
//		pots(1)(1).value = 1.0
//	}

/*

// E Pluribus Unum factor: map binary to multinomial variables
class EPUFactor : public Table2Factor {
public:
  EPUFactor(const string& name, Vertex v, const Graph& g) : Table2Factor(name) {
    vector<int> varity(2);
    adjacent_arity(v, g, varity);
    if ( varity[0] != 2 ) error("arity of 1st variable != 2");
    // REprintf("# %s arity = %d, %d\n", name.c_str(), varity[0], varity[1]);
    pots_.resize(varity[0], varity[1]);
    pots_ = 0;
    pots_(0,0) = 1.0;
    for ( int val = 1; val < varity[1]; ++val ) pots_(1,val) = 1.0;
  }
};


/*
Here's the logic: let's call the predicate variable P and the argOf
variable A. You want to prohibit the variable assignment where A is
true and P is false. All other variable assignments are OK and can be
scored by their other appropriate factors. The potential table for the
binary factor therefore looks like:

           P
        0    1
------------------
  0    1     1
A
  1    0     1

Multiply one incoming message by this potential table to get an
outgoing message; also do it in the opposite direction. This is what
Table2Factor does, for instance. And no, you do need to send messages
in both directions: if A is more likely to be on, P is more likely to
be on; if P is more likely to be off, A is more likely to be off. I
think this is related to a misunderstanding in your code. You don't
need to special-case the case where the probability of P being true is
0. It'll just fall out of the message computations. The only reason I
need to special-case zero messages is if they lead to division by
zero.
*/
class YouShallNotPassFactor(idx: Int, name: String) extends Factor(idx, name) {
	val pots = Array.ofDim[Double](2, 2)
	pots(0)(0) = 1
	pots(0)(1) = 1
	pots(1)(0) = 0
	pots(1)(1) = 1

  def arity = 10

	// Edge 0 assumed to be from the indicator variable
	def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
//		val edges = graph.edgesFrom(this).toArray

//		println("computing message in YSNP: " + name)
//		println("logic pots = ")
//		printMatrix(pots)

//		println
		val edges = graph.edgesFrom(this).toArray
//		println("edge 0 goes to " + edges(0).variable.name + " with message " + edges(0).v2f.mkString(", "))
//		println("edge 1 goes to " + edges(1).variable.name + " with message " + edges(1).v2f.mkString(", "))
		val m1 = mDown(edges(0).v2f, pots)
//		println("multiplied down = ")
//		printMatrix(m1)
		edges(1).f2v = dampen(edges(1).f2v, sAcross(m1), damp)

//		println
		val m2 = mAcross(edges(1).v2f, pots)
//		println("multiplied across = ")
//		printMatrix(m2)
		edges(0).f2v = dampen(edges(0).f2v, sDown(m2), damp)
		return 0.0
	}

		def getBeliefs(graph: FactorGraph): Array[Potential] = {
			return Array[Potential]()
		}
}
*/
