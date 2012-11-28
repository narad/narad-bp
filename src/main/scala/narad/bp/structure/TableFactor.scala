package narad.bp.structure

import scala.collection.mutable.ArrayBuffer

class Table1Factor(idx: Int, name: String, pots: Array[Potential]) extends Factor(idx, name) { //}, new UnaryFactorPotential(pots)) {

  def arity = 1

		def computeMessages(fg: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
			if (verbose) println("Computing table1factor message for %s".format(name))
			val edge = fg.edgesFrom(this).toArray.first
//			val v = pots(0).value
//			edge.f2v = dampen(edge.f2v, Array(v, 1-v), damp)
			if (verbose) println("pots = " + pots.map(_.value).mkString(", "))
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
				
//				val pot = pots(0)
//				val cloned = new Potential(1-pot.value, pot.name, !pot.label)
//				val ip = Array(pot, cloned)

				val beliefs = elementMultiplication(graph.edgesFrom(this).toArray.first.v2f, pots) //ip) //pots)
				normalize(beliefs)
//				val sum = beliefs.foldLeft(0.0)(_+_)
//				for (i <- 0 until beliefs.size) beliefs(i) / sum
//				return Array(beliefs(1))  // Changed this to 0 to account for problems calculating Z in IsAtMost1, think it's better to initalize with 0 and return index 0
//				return Array(beliefs(1))
				return beliefs
			}
}


class Table2Factor(idx: Int, name: String, pots: Array[Array[Potential]]) extends Factor(idx, name) {

  def arity = 2

	def computeMessages(graph: FactorGraph, damp: Double = 1.0, verbose: Boolean = false): Double = {
		if (verbose) System.err.println("DEBUG:  Computing message in " + name)
		val edges = graph.edgesFrom(this).toArray
//		System.err.println("Edges = " + edges)

    if (verbose) System.err.println("DEBUG: POTS[0,0] = " + pots(0)(0))
    if (verbose) System.err.println("DEBUG: POTS[0,1] = " + pots(0)(1))
    if (verbose) System.err.println("DEBUG: POTS[1,0] = " + pots(1)(0))
    if (verbose) System.err.println("DEBUG: POTS[1,1] = " + pots(1)(1))
    if (verbose) System.err.println("DEBUG: table2m[0,0] = " + edges(0).v2f(0) + " * " + edges(1).v2f(0))
    if (verbose) System.err.println("DEBUG: table2m[0,1] = " + edges(0).v2f(0) + " * " + edges(1).v2f(1))
    if (verbose) System.err.println("DEBUG: table2m[1,0] = " + edges(0).v2f(1) + " * " + edges(1).v2f(0))
    if (verbose) System.err.println("DEBUG: table2m[1,1] = " + edges(0).v2f(1) + " * " + edges(1).v2f(1))

    val var1 = edges(0).variable
		val var2 = edges(1).variable
		val m2 = mAcross(edges(0).v2f, pots)
    if (verbose) System.err.println("DEBUG: TMP[0,0] = " + m2(0)(0))
    if (verbose) System.err.println("DEBUG: TMP[0,1] = " + m2(0)(1))
    if (verbose) System.err.println("DEBUG: TMP[1,0] = " + m2(1)(0))
    if (verbose) System.err.println("DEBUG: TMP[1,1] = " + m2(1)(1))
    val sm2 = norm(sDown(m2))
		edges(1).f2v = dampen(edges(1).f2v, sm2, damp)		
		if (verbose) System.err.println("----------------")
		val m1 = mDown(edges(1).v2f, pots)
    if (verbose) System.err.println("DEBUG: TMP[0,0] = " + m1(0)(0))
    if (verbose) System.err.println("DEBUG: TMP[0,1] = " + m1(0)(1))
    if (verbose) System.err.println("DEBUG: TMP[1,0] = " + m1(1)(0))
    if (verbose) System.err.println("DEBUG: TMP[1,1] = " + m1(1)(1))
    val sm1 = norm(sAcross(m1))
		edges(0).f2v = dampen(edges(0).f2v, sm1, damp)	
		if (verbose) System.err.println("DEBUG: mess 1 = [" + sm1.mkString(", ") + "]; damp = " + damp)
		if (verbose) System.err.println("DEBUG: mess 2 = [" + sm2.mkString(", ") + "]; damp = " + damp)
		return 0.0
	}

	def getBeliefs(graph: FactorGraph): Array[Potential] = {
		val edges = graph.edgesFrom(this).toArray
//		System.err.println("Edges = " + edges + " in " + name)
		val beliefs = pots.clone
		for (i <- 0 until edges(0).variable.arity; j <- 0 until edges(1).variable.arity) {
			beliefs(i)(j).value *= edges(0).v2f(i) * edges(1).v2f(j)
		}
    val f = beliefs.flatten
    normalize(f)
    f
	}
}

/*
class Table3Factor(idx: Int, name: String, pots: Array[Array[Potential]]) extends Factor(idx, name) {

}
*/







//		val a = Array.tabulate(2,2)(case(i,j) => edges(0).v2f(i) * edges(1).v2f(j))
/*
		var prod = 1.0
		for (i <- 0 until 2; j <- 0 until 2) {
			System.err.println("table2m i = %d; j = %d = %f * %f".format(i, j, edges(0).v2f(i), edges(1).v2f(j)))
			prod *= edges(0).v2f(i) * edges(1).v2f(j)
		}
*/
//		System.err.println("prod = " + prod)
/*
		val beliefs = pots.flatten
		System.err.println("Pots = " + beliefs.mkString("\n"))
		beliefs.foreach(_.value *= prod)
		System.err.println("Pots After = " + beliefs.mkString("\n"))
		return beliefs
	}
*/

	/*
	virtual void append_beliefs(Vertex v, const Graph& g, s2dmap& pots) const {
    Array2 a(get_beliefs(v, g));
    pots[name_] = a(1,0) / sum(a);
  }
*/

	
	
	
	
	
	
	
	
	
	
	
		
//		System.err.println("var1 (%d) = %s".format(var1.arity, edges(0).v2f.size))
//		System.err.println("var2 (%d) = %s".format(var2.arity, edges(1).v2f.size))
		
//		System.err.println("pots = (%d x %d)".format(pots.size, pots(0).size))
//		for (i <- 0 until pots.size) {
//			System.err.println("i = %d: %s".format(i, pots(i).map(_.name).mkString("\t|\t")))
//		}
//		System.err.println
	//		val edges = graph.edgesFrom(this).toArray
//			System.err.println("edge 0 goes to " + edges(0).variable.name + " with message " + edges(0).v2f.mkString(", "))
//			System.err.println("edge 1 goes to " + edges(1).variable.name + " with message " + edges(1).v2f.mkString(", "))
//			val m1 = mDown(Array(0.2, 0.8), pots)

//println
//println("In = ")
//println(edges(0).v2f.mkString(", "))
//println("multiplied across = ")
//printMatrix(m2)
//println("summed down = ")
//println(sm2.mkString(", "))
//System.err.println("Arity of variable %s is %s;  Message to variable is [%s].".format(edges(1).variable.name, edges(1).variable.arity, sm2.mkString(", ")))

//println("In = ")
//println(edges(1).v2f.mkString(", "))
			
/*
			println("multiplied down = ")
			printMatrix(m1)
			println("summed across = ")
			println(sm1.mkString(", "))
			println("multed down = %d x %d".format(m1.size, m1(0).size))
			System.err.println("Arity of variable %s is %s;  Message to variable is [%s].".format(edges(0).variable.name, edges(0).variable.arity, sm1.mkString(", ")))
*/

		
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



