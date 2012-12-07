package narad.bp.structure

abstract class Factor(idx: Int, name: String) extends MessageNode(idx, name) { //}, potential: FactorPotential) extends MessageNode(idx, name) {

	def computeMessages(fg: FactorGraph, damp: Double, verbose: Boolean): Double
	
	def getBeliefs(graph: FactorGraph): Array[Potential]

	override def toString = "Factor%d[%s]".format(idx, name)	
	
	def logIncrement(s: Double, x: Double): Double = {
//		System.out.println("QUEUE S = " + s)
//		System.out.println("QUEUE X = " + x)
		if (s == Double.NegativeInfinity) {
//			System.out.println("QUEUE case 1")
			return x
		}
		else {
			val d = s - x
			if (d >= 0) {
				if (d <= 745) {
//					System.out.println("QUEUE case 2")
//					System.out.println("QUEUE log is " + Math.log(1.0 + Math.exp(-1.0 * d)))
					return s + Math.log(1.0 + Math.exp(-1.0 * d))
				}
				else {
//					System.out.println("QUEUE case 5")
					return s
				}
			}
			else if (d < -745) {
//				System.out.println("QUEUE case 3")
				return x
			}
			else {
//				System.out.println("QUEUE case 4")
				return x + Math.log(1.0 + Math.exp(d))
			}
		}
	}
}

class UnaryFactor(idx: Int, name: String, var pots: Array[Potential]) extends Factor(idx, name) {  //, new UnaryFactorPotential(pots)) {

  def arity = 1

	def getBeliefs(graph: FactorGraph): Array[Potential] = {
		val edges = graph.edgesFrom(this).toArray
		assert (edges.size == 1)
		println("DEBUG:  v2f message for %s is %s from %s.".format(name, edges.first.v2f.mkString(","),edges.first.variable.name))
		println("DEBUG:    * [%s]".format(pots.map(_.value).mkString(", ")))
		val beliefs = elementMultiplication(graph.edgesFrom(this).toArray.first.v2f, pots)
		println("DEBUG:    = [%s]".format(beliefs.map(_.value).mkString(", ")))
		normalize(beliefs)
		println("DEBUG:   = [%s] normalized.".format(beliefs.map(_.value).mkString(", ")))
		println("DEBUG:   returning potetnial %s".format(beliefs(1).toString))
		println("DEBUG:   ")
		return Array(beliefs(1))
	}

	def peg = {
		pots(0).value = 0.0
		pots(1).value = 1.0
	}

	def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
		if (verbose) println("Computing message in Factor %s.".format(name))
		val edge = graph.edgesFrom(this).toArray.first
		edge.f2v = dampen(edge.f2v, pots.map(_.value), damp)
		return 0.0
	}
}




























































/*
	def appendBeliefs(graph: FactorGraph) {
		val beliefs = getBeliefs(graph).map(_._2)
		val sum = beliefs.foldLeft(0.0)(_+_)
		pots(0) = beliefs(1) / sum //  pots[name_] = a(1) / sum(a);
	}
*/


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


//import narad.projects.bpdp._

/*
abstract class FactorPotential {}

class UnaryFactorPotential(pots: Array[Double]) extends FactorPotential {}
class BinaryFactorPotential(pots: Array[Array[Double]]) extends FactorPotential {}
class TernaryFactorPotential(pots: Array[Array[Array[Double]]]) extends FactorPotential {}
*/

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
