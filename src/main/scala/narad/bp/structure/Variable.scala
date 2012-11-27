package narad.bp.structure

import scala.collection.mutable.ArrayBuffer

class Variable(idx: Int, name: String, var arity: Int) extends MessageNode(idx, name) {

	def computeMessages(graph: FactorGraph, damp: Double, verbose: Boolean = false): Double = {
//		if (verbose) println("Computing variable message for %s.".format(name))
//		println("damp = " + damp)
		var maxDiff = -1.0
		for (dest <- graph.edgesFrom(this)) {
			if (verbose) println("\nVariable message from %s ==> %s:".format(this, dest.factor.name))
			val omess = dest.v2f
			var nmess = Array.fill(omess.size)(1.0)
			for (e <- graph.edgesFrom(this) if e != dest) {
				if (verbose) println("  * [%s] from %s...".format(e.f2v.mkString(", "), e.factor.name))
				nmess = vprod(e.f2v, nmess)
			}
			if (verbose) println("  ...final message = [%s]".format(nmess.mkString(", ")))
			val sum = nmess.foldLeft(0.0)(_+_)
			nmess = nmess.map(_ / sum)
//			if (verbose) println("  ...normalized =    [%s]".format(nmess.mkString(", ")))
			val res = dampen(omess, nmess, damp)
//			if (verbose) println("  ...updated    =    [%s]\n".format(res.mkString(", ")))
			dest.v2f = res
			val diffList = vsub(omess, res)
			val diff = diffList.max
			if ( diff > maxDiff ) maxDiff = diff
		}
		maxDiff
	}

	def getBeliefs(graph: FactorGraph): Array[(String, Double)] = {
		val res = new Array[Double](arity)
		for (e <- graph.edgesFrom(this))
		for (i <- 0 until res.size) res(i) *= e.f2v(i)
		val sum = res.foldLeft(0.0)(_+_)
		val beliefs = res.map(_ / sum)
		return Array((name, beliefs(1)))
	}

	def logOdds(graph: FactorGraph): Double = {
		if (arity > 2) return -1.0 * Math.log(0)  // For now this is okay
		val res = Array[Double](1.0, 1.0)
		for (edge <- graph.edgesFrom(this)) {
			for (i <- 0 until edge.f2v.size) {
				res(i) *= edge.f2v(i)
			}
		}
		return Math.log(res(1)) - Math.log(res(0))
	}

	override def toString = "Variable%d[%s]".format(idx, name)

}































/*


class Variable[T](idx: Int, name: String, values: Array[T]) extends MessageNode(idx, name) {
	
	def computeMessages(graph: FactorGraph, damp: Double): Double = {
		println("computing variable message for %s!".format(name))
		var maxDiff = -1.0
		for (dest <- graph.edgesFrom(this)) {
//			println("  DEST = " + dest.toString)
//			val omess = dest.f2v
			val omess = dest.v2f
			var nmess = Array.fill(omess.size)(1.0)
			for (e <- graph.edgesFrom(this) if e != dest) {
//				println("    -- multiplying in %s".format(e.f2v.mkString(",")))
				nmess = vprod(e.f2v, nmess)
			}
//			println("OMESS = %s".format(omess.mkString(", ")))
//			println("NMESS = %s".format(nmess.mkString(", ")))
//			println("DENOM = %f".format(sum))
//			println("  old nmess = %s".format(nmess.mkString(",")))
			val sum = nmess.foldLeft(0.0)(_+_)
			nmess = nmess.map(_ / sum)
			val res = dampen(omess, nmess, damp)
			dest.v2f = res
//			println("  new message = %s".format(dest.v2f.mkString(",")))
			val diffList = vsub(omess, res)
//			println("diff list = [" + diffList.mkString(","))
      val diff = diffList.max
//			println("\tdiff = " + diff)
      if ( diff > maxDiff ) maxDiff = diff
		}
//		println("diff is " + maxDiff)
		maxDiff
	}
	
	def getBeliefs(graph: FactorGraph): Array[(String, Double)] = {
		val res = new Array[Double](arity)
		for (e <- graph.edgesFrom(this))
			for (i <- 0 until res.size) res(i) *= e.f2v(i)
		val sum = res.foldLeft(0.0)(_+_)
		val beliefs = res.map(_ / sum)
		return Array(Tuple(name, beliefs(1)))
	}
	
	def logOdds(graph: FactorGraph): Double = {
		if (arity > 2) return -1.0 * Math.log(0)  // For now this is okay
		val res = Array[Double](1.0, 1.0)
		for (edge <- graph.edgesFrom(this)) {
			for (i <- 0 until edge.f2v.size) {
				res(i) *= edge.f2v(i)
			}
		}
		return Math.log(res(1)) - Math.log(res(0))
	}
	
	
/*	
  virtual double log_odds(Vertex v, const Graph& g) const {
    EdgeIterator e, end;
    if ( arity_ > 2 ) return -log(0);
    lvec res(arity_);
    res = 1;
    for ( tie(e, end) = out_edges(v, g); e != end; ++e ) {
      res *= g[*e].f2v;
    }
    return log(res(1)) - log(res(0));
  }
*/



	//		println(beliefs.mkString(", "))
	//		var n = ""
	//		for (n <- graph.successors(this)) {
	//			if (n.pots.size == ar)
	//		}
	
	/*
	def beliefs(graph: FactorGraph)
	dvec get_beliefs(Vertex v, const Graph& g) const {
    EdgeIterator e, end;
    lvec res(arity_);
    res = 1;
    for ( tie(e, end) = out_edges(v, g); e != end; ++e ) {
      res *= g[*e].f2v;
    }
    res /= sum(res);
    return l2dvec(res);
  }
	*/
	
	def vprod(v1: Array[Double], v2: Array[Double]): Array[Double] = {
		v1.zipWithIndex.map{case(e,i) => e * v2(i)}
	}
	
	def vdiv(v1: Array[Double], v2: Array[Double]): Array[Double] = {
		v1.zipWithIndex.map{case(e,i) => e / v2(i)}
	}
	
	def vadd(v1: Array[Double], v2: Array[Double]): Array[Double] = {
		v1.zipWithIndex.map{case(e,i) => e + v2(i)}
	}
	
	def vsub(v1: Array[Double], v2: Array[Double]): Array[Double] = {
		v1.zipWithIndex.map{case(e,i) => e - v2(i)}
	}
	
	def arity = values.size 
	
	override def toString = "Variable%d[%s]".format(idx, name)
}


*/



/*	
		for (edge <- graph.edgesFrom(this)) {		
		}
		EdgeIterator e, end;
    dvec res(arity_);
    res = 1;
    for ( tie(e, end) = out_edges(v, g); e != end; ++e ) {
      res *= g[*e].f2v;
    }
    res /= sum(res);
    return res;
		// normalize
		for ()
	}
}
*/
/*
		virtual double compute_messages(Vertex v, Graph& g, double damp) {
	    damp_assign(g[*(out_edges(v, g).first)].f2v, pots_, damp);
	    return 0;
	  }
		maxDiff 
	}
}
*/ 

/*
class BinaryFactor(idx: Int, name: String, pots: Array[Array[Double]]) extends Factor(idx, name, new BinaryFactorPotential(pots)) {
	
	def computeMessages(fg: FactorGraph, damp: Double): Double = {
		var maxDiff = -1		
		maxDiff 
	}
}

class TernaryFactor(idx: Int, name: String, pots: Array[Array[Array[Double]]]) extends Factor(idx, name, new TernaryFactorPotential(pots)) {
	def computeMessages(fg: FactorGraph, damp: Double): Double = {
		var maxDiff = -1		
		maxDiff 
	}
}
*/





// ------ VARIABLES:










	
	/*
	virtual double compute_messages(Vertex v, Graph& g, double damp) {
    EdgeIterator dest, e, end;
    double maxDiff = -1;
    for (tie(dest, end) = out_edges(v, g); dest != end; ++dest ) {
      dvec& res = g[*dest].v2f;
      dvec old(res.shape());
      old = res;
      lvec mess(res.shape());
      mess = 1.0;
      for (e = out_edges(v, g).first; e != end; ++e ) {
	if ( e == dest ) continue;
	mess *= g[*e].f2v;
      }
      mess /= sum(mess);
      damp_assign(res, l2dvec(mess), damp);
      old -= res;
      double diff = max(old);
      if ( diff > maxDiff ) maxDiff = diff;
    }
    // cerr << "# " << name_ << ": " << maxDiff << endl;
    return maxDiff;
  }
   */









//extends Variable { //with DiscreteValues {


// All variables will be discrete for now 



/*
class ContinuousVariable extends Variable with ContinuousValues{
	
}

trait DiscreteValues[T] {
}
*/
