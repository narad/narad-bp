package narad.bp.structure

import scala.collection.mutable.ArrayBuffer
//import narad.projects.bpdp._

/*
abstract class FactorPotential {}

class UnaryFactorPotential(pots: Array[Double]) extends FactorPotential {}
class BinaryFactorPotential(pots: Array[Array[Double]]) extends FactorPotential {}
class TernaryFactorPotential(pots: Array[Array[Array[Double]]]) extends FactorPotential {}
*/

object FactorFactory {
	def createUnaryFactor(idx: Int, name: String, pot: Potential): UnaryFactor = {
		val pots = if (pot == Double.PositiveInfinity) {  
				Array[Potential](new Potential(0.0, "-%s".format(name), false), new Potential(1.0, name, pot.isCorrect))
		}
		else {
				Array[Potential](new Potential(1.0, "-%s".format(name), false), pot)
		}
		return new UnaryFactor(idx, name, pots)		
	}


	def createNandFactor(idx: Int, name: String, pot: Potential): NandFactor = {
		val pots = Array.ofDim[Potential](2,2)
		if (pot == Double.PositiveInfinity) {
			pots(0)(0) = new Potential(0.0, "n/a", false)
			pots(0)(1) = new Potential(0.0, "n/a", false)
			pots(1)(0) = new Potential(0.0, "n/a", false)
			pots(1)(1) = new Potential(1.0, pot.name, pot.isCorrect)
		}
		else {
			pots(0)(0) = new Potential(1.0, "n/a", false)
			pots(0)(1) = new Potential(1.0, "n/a", false)
			pots(1)(0) = new Potential(1.0, "n/a", false)
			pots(1)(1) = pot
		}
		return new NandFactor(idx, name, pots)
	}

}

abstract class Factor(idx: Int, name: String) extends MessageNode(idx, name) { //}, potential: FactorPotential) extends MessageNode(idx, name) {

	def computeMessages(fg: FactorGraph, damp: Double, verbose: Boolean): Double
	
	def getBeliefs(graph: FactorGraph): Array[Potential]

	override def toString = "Factor%d[%s]".format(idx, name)	
}

class UnaryFactor(idx: Int, name: String, var pots: Array[Potential]) extends Factor(idx, name) {  //, new UnaryFactorPotential(pots)) {

	def getBeliefs(graph: FactorGraph): Array[Potential] = {
//		val beliefs = graph.beliefs(this)
//		println(beliefs.mkString(", "))  
		val beliefs = elementMultiplication(graph.edgesFrom(this).toArray.first.v2f, pots)
		normalize(beliefs)
//		val sum = beliefs.foldLeft(0.0)(_+_.value)
//		beliefs.foreach(_.value / sum)
		return Array(beliefs(1))
	}

	def peg = {
		pots(0).value = 0.0
		pots(1).value = 1.0
	}
//		val scores = graph.edgesFrom(this).toList.first.v2f 
//		return elementMultiplication(pots, scores).map(Tuple(potname, _))
/*
		val beliefs = elementMultiplication(pots, scores)
		val sum = beliefs.foldLeft(0.0)(_+_)
		println("\n" + potname)
		println("Unary score: %s".format(scores.mkString(",")))
		println("pots = " + pots.mkString(","))
		println("beliefs = " + beliefs.mkString(","))
		println("normalized = " + beliefs(1) / sum)
		return Array(Tuple(potname, beliefs(1) / sum))
*/
//		return Array(Tuple(potname, pots(1) / pots.foldLeft(0.0)(_+_)))
//	}		

	def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
		if (verbose) println("Computing message in Factor %s.".format(name))
		val edge = graph.edgesFrom(this).toArray.first
		edge.f2v = dampen(edge.f2v, pots.map(_.value), damp)
		return 0.0
	}

/*
	def appendBeliefs(graph: FactorGraph) {
		val beliefs = getBeliefs(graph).map(_._2)
		val sum = beliefs.foldLeft(0.0)(_+_)
		pots(0) = beliefs(1) / sum //  pots[name_] = a(1) / sum(a);
	}
*/

}

class Table1Factor(idx: Int, name: String, pots: Array[Potential]) extends Factor(idx, name) { //}, new UnaryFactorPotential(pots)) {
		def computeMessages(fg: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
			val edge = fg.edgesFrom(this).toArray.first
			edge.f2v = dampen(edge.f2v, pots.map(_.value), damp)
			return 0.0
		}	
		
		def getBeliefs(graph: FactorGraph): Array[Potential] = {
/*
			val npots  = new ArrayBuffer[(String, Double)]
			println("TABLE1FACTOR BELIEFS for %s".format(this.name))
			println("POTS = " + pots.mkString(","))
			println("NAMES = " + names.mkString(","))
			val scores = graph.edgesFrom(this).toList.first.v2f 
			for (i <- 0 until pots.size) {
				npots += Tuple(names(i), scores(i))
			}
			return npots.toArray
			*/
//				val beliefs = graph.beliefs(this)
//				println(beliefs.mkString(", "))
//				val beliefs = graph.successors(this).toArray.first.asInstanceOf[Variable[Int]].getBeliefs(graph).map(_._2)
				val beliefs = elementMultiplication(graph.edgesFrom(this).toArray.first.v2f, pots)
				normalize(beliefs)
//				val sum = beliefs.foldLeft(0.0)(_+_)
//				for (i <- 0 until beliefs.size) beliefs(i) / sum
				return Array(beliefs(1))
			}
}


class Table2Factor(idx: Int, name: String, pots: Array[Array[Potential]]) extends Factor(idx, name) {

	def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
			val edges = graph.edgesFrom(this).toArray
//			println("edge 0 goes to " + edges(0).variable.name + " with message " + edges(0).v2f.mkString(", "))
//			println("edge 1 goes to " + edges(1).variable.name + " with message " + edges(1).v2f.mkString(", "))
			val m1 = mDown(edges(0).v2f, pots)
//			println("multiplied down = ")
//			printMatrix(m1)
			edges(1).f2v = dampen(edges(1).f2v, sAcross(m1), damp)

//			println
			val m2 = mAcross(edges(1).v2f, pots)
//			println("multiplied across = ")
//			printMatrix(m2)
			edges(0).f2v = dampen(edges(0).f2v, sDown(m2), damp)		
			return 0.0
		}
		
/*
		val edges = fg.edgesFrom(this).toArray
		assert(edges.size == 2, "Binary Factor is improperly configured: has %d neighbors.".format(edges.size))

		val m1 = multiplyAcross(edges(0).v2f, pots)
		edges(1).f2v = dampen(edges(1).f2v, sumDown(m1), damp)

		val m2 = multiplyDown(edges(1).v2f, pots)
		edges(0).f2v = dampen(edges(0).f2v, sumAcross(m1), damp)
		return 0.0
	}
*/

	def getBeliefs(graph: FactorGraph): Array[Potential] = {
		val edges = graph.edgesFrom(this).toArray
		var prod = 1.0
		for (i <- 0 until 2; j <- 0 until 2) {
			prod *= edges(0).v2f(i) * edges(1).v2f(j)
		}
		val beliefs = pots.flatten
		beliefs.foreach(_.value *= prod)
		return beliefs
	}
}	

/*
  virtual double compute_messages(Vertex v, Graph& g, double damp) {
    vector<Edge> dests(2);
    all_out_edges(v, g, dests);
    firstIndex i; secondIndex j;
//		cout << "First Index " << i << endl;
//		cout << "Second Index " << j << endl;
    Array2 tmp(pots_.shape());
    dvec mess(pots_.extent(firstDim));

    tmp = pots_ * g[dests[1]].v2f(j);
		cout << "TEMP = " << tmp << " " << endl;
//		cout << j << " " << endl;
    mess = sum(tmp, j);
		cout << "MESS = " << mess << " " << endl;
    damp_assign(g[dests[0]].f2v, mess, damp);

    mess.resize(pots_.extent(secondDim));
    tmp = pots_ * g[dests[0]].v2f(i);
    mess = sum(tmp(j, i), j);
    damp_assign(g[dests[1]].f2v, mess, damp);
    return 0;
  }
  Array2 get_beliefs(Vertex v, const Graph& g) const {
    Array2 a(pots_.shape());
    vector<Edge> dests(2);
    all_out_edges(v, g, dests);
    firstIndex i; secondIndex j;
    a = pots_ * g[dests[0]].v2f(i) * g[dests[1]].v2f(j);
    // WARNING: Incrementing e in the expression DOES NOT WORK!!!
    // a = (pots_ * (g[*e].v2f)(i)) * (g[*(++e)].v2f)(j);
    return a;
  }
*/

class Named1Factor(idx: Int, name: String, pots: Array[Potential]) extends Factor(idx, name) { //}, new UnaryFactorPotential(pots)) {
	
	def computeMessages(fg: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
		val edge = fg.edgesFrom(this).toArray.first
//		println("Compute UNARY FACTOR message!  Factor = %s".format(this.toString))
//		println("Edge: %s".format(edge.toString))
		edge.f2v = dampen(edge.f2v, pots.map(_.value), damp)
//		println("EDGE %s Message has been set: (%d) %s".format(edge, edge.f2v.size, edge.f2v.mkString(", ")))
		return 0.0
	}
	
	
	/// NOT A VALID IMPLEMENTATION
	def getBeliefs(graph: FactorGraph): Array[Potential] = {
		val npots  = new ArrayBuffer[Potential]
		val beliefs = elementMultiplication(graph.edgesFrom(this).toArray.first.v2f, pots)
		normalize(beliefs)
		return beliefs
	}
//		for (i <- 0 until pots.size) {
//			npots += Tuple(names(i), scores(i))
//		}
//		return npots.toArray
//	}
}

class IsAtMost1Factor(idx: Int, name: String) extends Factor(idx, name) { //}, new UnaryFactorPotential(Array[Double]())) {
	
	def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
		var z = 1.0
		var trues = 0
		var count = 0
//		println("computing message for IsAtMost1 %s".format(name))
		for (edge <- graph.edgesFrom(this)) {
			val in = edge.v2f
			if (count == 0) {
//				println("first message")
				z = in(0) / in(1)				
			}
			else {
//				println("second message")
				z += in(1) / in(0)
				if (in(0) == 0) trues += 1				
			}
			count += 1
		}
//		println("Z = " + z + " and trues = " + trues)
		count = 0
		for (edge <- graph.edgesFrom(this)) {
			val in = edge.v2f
			if (count == 0) {
				var mess = Array(1.0, z - in(0) / in(1))
				if (trues == 1) {
					mess = Array(0.0, 1.0)
				}
				else if (z == Double.PositiveInfinity){
					mess = Array(1.0, 0.0)
				}
//				println("step 1 mess = " + mess.mkString(", "))
				edge.f2v = dampen(edge.f2v, mess, damp)				
			}
			else {
			var mess = Array(z - in(0) / in(1), 1.0)
			if (trues == 1) {
				if (in(0) == 0) {
					mess = Array(0.0, 1.0)
				}
				else {
					mess = Array(1.0, 0.0)
				}
			}
			else if (z == Double.PositiveInfinity) {
				mess = Array(1.0, 0.0)
			}
//			println("step 2 mess = " + mess.mkString(", "))
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
	
	def peg = {
		pots(0)(0).value = 0.0
		pots(0)(1).value = 0.0
		pots(1)(0).value = 0.0
		pots(1)(1).value = 1.0
	}
}





























/*
class Table2Factor(idx: Int, name: String, names: Array[Array[String]], pots: Array[Array[Double]]) extends Factor(idx, name, new BinaryFactorPotential(pots)) {
	
	def computeMessages(fg: FactorGraph, damp: Double = 1.0): Double = {
		val edges = fg.edgesFrom(this).toArray
		assert(edges.size == 2, "Binary Factor is improperly configured: has %d neighbors.".format(edges.size))
//		println("Dimensionality of first message: 1x%d".format(edges(0).v2f.size))
//		println("Dimensionality of 2nd   message: 1x%d".format(edges(1).v2f.size))
		
//		val pots = scoredpots.map(_.map(_._2))
		val m1 = multiplyAcross(edges(0).v2f, pots)
		edges(1).f2v = dampen(edges(1).f2v, sumDown(m1), damp)

		val m2 = multiplyDown(edges(1).v2f, pots)
		edges(0).f2v = dampen(edges(0).f2v, sumAcross(m1), damp)
//		println("UPDATED 0: %s".format(edges(0).f2v.mkString(", ")))
//		println("UPDATED 1: %s".format(edges(1).f2v.mkString(", ")))
		return 0.0
	}

	def getBeliefs(graph: FactorGraph): Array[(String, Double)] = {
		val npots  = new ArrayBuffer[(String, Double)]
		val scores = graph.edgesFrom(this).toList.first.v2f 
		for (i <- 0 until scores.size; j <- 0 until scores.size) {
//			npots += Tuple(scoredpots(i)(j)._1, scores(i))
			npots += Tuple(names(i)(j), scores(i))
		}
		return npots.toArray
	}
}	



class Named2Factor(idx: Int, name: String, names: Array[Array[String]], pots: Array[Array[Double]]) extends Table2Factor {
	
	def computeMessages(fg: FactorGraph, damp: Double = 1.0): Double = {
		val edges = fg.edgesFrom(this).toArray
		assert(edges.size == 2, "Binary Factor is improperly configured: has %d neighbors.".format(edges.size))
		val m1 = multiplyAcross(edges(0).v2f, pots)
		edges(1).f2v = dampen(edges(1).f2v, sumDown(m1), damp)

		val m2 = multiplyDown(edges(1).v2f, pots)
		edges(0).f2v = dampen(edges(0).f2v, sumAcross(m1), damp)
		return 0.0
		
		//
		for (v1 <- 0 until varity(0); v2 <- 0 until varity(1))
		pots(val1)(val2) = ()
	}


// rewrite getBeliefs
	def getBeliefs(graph: FactorGraph): Array[(String, Double)] = {
		val npots  = new ArrayBuffer[(String, Double)]
		val scores = graph.edgesFrom(this).toList.first.v2f 
		for (i <- 0 until scores.size; j <- 0 until scores.size) {
//			npots += Tuple(scoredpots(i)(j)._1, scores(i))
			npots += Tuple(names(i)(j), scores(i))
		}
		return npots.toArray
	}
}	
*/
	
/*
class NandFactor(name: String, pots: Array[Potential]) extends Table2Factor(name, names, pots) {
	
	def getBeliefs(graph: FactorGraph): Array[Potential] = {
		val beliefs = pots
		var c = 1
		for (edge <- graph.edgesFrom(this); i <- 0 until beliefs.size) {
			c match {
				1 => 
				2 =>
				_ => assert(false, "ERROR: should not have reached count higher than 2 for NandFactor edges")
			}
		}
		
		beliefs(i) = edge.v2f(i) 
		val sum = beliefs.foldLeft(0.0)(_+_)
		for (i <- 0 until beliefs.size) beliefs(i) / sum
		return Array(Tuple(names(0), beliefs(1)))
	}
	
	def peg = {
		pots(0)(0) = 0.0
		pots(0)(1) = 0.0
		pots(1)(0) = 0.0
		pots(1)(1) = 1.0
	}
}
*/

/*	
	class NandFactor : public Table2Factor {
	public:
	  virtual void append_beliefs(Vertex v, const Graph& g, s2dmap& pots) const {
	    Array2 a(get_beliefs(v, g));
	    pots[name_] = a(1,1) / sum(a);
	  }

	};
	*/
















	/*
	class UnaryFactor : public Table1Factor {
	public:
	  UnaryFactor(const string& name, double pot) : Table1Factor(name) {
	    pots_.resize(2);
	    if ( pot == R_PosInf ) {
	      pots_ = 0, 1;
	    } else {
	      pots_ = 1, pot;
	    }
	    // pots_ /= sum(pots_);
	  }

	  virtual void append_beliefs(Vertex v, const Graph& g, s2dmap& pots) const {
	    Array1 a(get_beliefs(v, g));
	    pots[name_] = a(1) / sum(a);
	  }

	  virtual void peg() {
	    pots_ = 0, 1;
	  }
	};

	class Table1Factor : public Factor {
	public:
	  Table1Factor(const string& name) : Factor(name) {}
	  Table1Factor(const string& name, const double *data, int dim1) : Factor(name) {
	    pots_.resize(dim1);
	    pots_ = Array1(const_cast<double *>(data), shape(dim1), duplicateData, fortranArray);
	  }
	  virtual double compute_messages(Vertex v, Graph& g, double damp) {
	    damp_assign(g[*(out_edges(v, g).first)].f2v, pots_, damp);
	    return 0;
	  }
	  Array1 get_beliefs(Vertex v, const Graph& g) const {
	    Array1 a(pots_.shape());
	    a = pots_ * g[*(out_edges(v, g).first)].v2f;
	    return a;
	  }
	  virtual double entropy(Vertex v, const Graph& g) {
	    Array1 a(pots_.shape()), b(pots_.shape());
	    a = pots_ * g[*(out_edges(v, g).first)].v2f;
	    a /= sum(a);
	    return -sum(zapnan(a * (log(a) - log(pots_))));
	  }
	protected:
	  Array1 pots_;
	};
	*/
	/*
		def computeMessages(fg: FactorGraph, damp: Double = 1.0): Double = {
		val edge = fg.edgesFrom(this).toArray.first
		println("Compute UNARY FACTOR message!  Factor = %s".format(this.toString))
		println("Edge: %s".format(edge.toString))
		edge.f2v = dampen(edge.f2v, pots, damp)
		println("EDGE %s Message has been set: (%d) %s".format(edge, edge.f2v.size, edge.f2v.mkString(", ")))
		return 0.0
	}

	virtual double compute_messages(Vertex v, Graph& g, double damp) {
    vector<Edge> dests(2);
		// Get the outgoing edges: dests(0) and dests(1)
    all_out_edges(v, g, dests);
    firstIndex i; secondIndex j;

    Array2 tmp(pots_.shape());
    dvec mess(pots_.extent(firstDim));

    tmp = pots_ * g[dests[1]].v2f(j);
    mess = sum(tmp, j);
    damp_assign(g[dests[0]].f2v, mess, damp);

    mess.resize(pots_.extent(secondDim));
    tmp = pots_ * g[dests[0]].v2f(i);
    mess = sum(tmp(j, i), j);
    damp_assign(g[dests[1]].f2v, mess, damp);
    return 0;
  }
*/





		/*	for atMost1			
				println("SUMMING:")
				for (edge <- graph.edgesFrom(this)) {
					val in = edge.v2f
					println("  in edge message = " + in.mkString(","))
					z += in(1) / in(0) 
					if (in(0) == 0) trues += 1
				}		

				println("Z = %f\t\tTrues = %d".format(z, trues))
				for (edge <- graph.edgesFrom(this)) {
					val in = edge.v2f
					var mess = Array[Double](z - in(1) / in(0), 1.0)
					if (trues == 1) {
						if (in(0) == 0)
							mess = Array[Double](0.0, 1.0)
						else 
							mess = Array[Double](1.0, 0.0)
					}
					edge.f2v = dampen(edge.f2v, mess, damp)
				}
				return 0.0
			}
		*/
