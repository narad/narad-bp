package narad.bp.structure
//import narad.structure.graph._
import scala.collection.mutable.{ArrayBuffer, HashMap, Map}
import scala.util.matching._
//import narad.projects.relmarg._
//import narad.projects.bpdp._


class FactorGraphBuilder(pots: Array[Potential]) { 
	var vcount = 0
	var fcount = 0
	val nodes = new ArrayBuffer[MessageNode]
	val edges = new ArrayBuffer[ArrayBuffer[Int]]
	val vnames = new HashMap[String, Int]

	def addCKYFactor(pattern: Regex, facname: String = "CKY", slen: Int): Int = {
		nodes += new CKYFactor(ncount, facname, slen)
		while (edges.size < nodes.size) edges += new ArrayBuffer
		for (i <- 0 until nodes.size-1 if matches(nodes(i).name, pattern) && nodes(i).isVariable) {
			edges(nodes.size-1) += i 
		}
		fcount += 1
		return ncount-1
	}
	
	def addEdge(vidx: Int, fidx: Int) = {
		edges(fidx) += vidx
	}
	
	def addFactor(factor: Factor): Int = {
		nodes += factor
		fcount += 1
		edges += new ArrayBuffer
		return ncount-1
	}	
	
	def addImpliesFactor(pattern1: Regex, pattern2: Regex, facname: String = "fac%d".format(fcount), fpots: Array[Potential]): Int = {
		nodes += FactorFactory.createImpliesFactor(ncount, facname, fpots(0))
		while (edges.size < nodes.size) edges += new ArrayBuffer
		for (i <- 0 until nodes.size-1) {
			if (matches(nodes(i).name, pattern1) || matches(nodes(i).name, pattern2)) {
				edges(nodes.size-1) += i
			}
		} 
		fcount += 1
		return ncount-1
	}

	def addNandFactor(pattern1: Regex, pattern2: Regex, facname: String = "fac%d".format(fcount), fpots: Array[Potential]): Int = {
		nodes += FactorFactory.createNandFactor(ncount, facname, fpots(0))
		while (edges.size < nodes.size) edges += new ArrayBuffer
		for (i <- 0 until nodes.size-1) {
			if (matches(nodes(i).name, pattern1) || matches(nodes(i).name, pattern2)) {
				edges(nodes.size-1) += i
			}
		} 
		fcount += 1
		return ncount-1
	}

	def addEPUFactor(pattern1: Regex, pattern2: Regex, arity: Int, facname: String = "fac%d".format(fcount)): Int = {
		nodes += FactorFactory.createEPUFactor(ncount, facname, arity)
		while (edges.size < nodes.size) edges += new ArrayBuffer
//		System.err.println("Finding neighbors of fac: " + facname)
		for (i <- 0 until nodes.size-1) {
//			System.err.println("-" + nodes(i).name)
			if (matches(nodes(i).name, pattern1) || matches(nodes(i).name, pattern2)) {
//				System.err.println("  - matched: " + nodes(i).name)
				edges(nodes.size-1) += i
			}
		} 
		fcount += 1
		return ncount-1
	}
	
	def addSegmentationFactor(pattern: Regex, facname: String = "SEGMENT", slen: Int, maxWidth: Int): Int = {
		nodes += new SegmentationFactor(ncount, facname, slen, maxWidth)
		while (edges.size < nodes.size) edges += new ArrayBuffer
		for (i <- 0 until nodes.size-1 if matches(nodes(i).name, pattern) && nodes(i).isVariable) {
			edges(nodes.size-1) += i 
		}
		fcount += 1
		return ncount-1
	}

	def addIsAtMost1Factor(iPattern: Regex, dPattern: Regex, facname: String = "fac%d".format(fcount)): Int = {
		nodes += new IsAtMost1Factor(ncount, facname)
		while (edges.size < nodes.size) edges += new ArrayBuffer
		edges(nodes.size-1) += nodes.indexWhere(n => matches(n.name, iPattern))
		var found = false
		for (idx <- 0 until nodes.size if matches(nodes(idx).name, dPattern)) {
			edges(nodes.size-1) += idx
			found = true
		}
		assert(found == true, "Pattern %s not found in addIsAtMost1.".format(dPattern))
		fcount += 1
		return ncount-1
	}

	def addAtMost1Factor(dPattern: Regex, facname: String = "fac%d".format(fcount)): Int = {
		nodes += new AtMost1Factor(ncount, facname)
		while (edges.size < nodes.size) edges += new ArrayBuffer
		for (idx <- 0 until nodes.size if matches(nodes(idx).name, dPattern)) {
			edges(nodes.size-1) += idx
		}
		fcount += 1
		return ncount-1
	}
	
	def addIsAtMost1FactorByIndex(iidx: Int, didxs: Array[Int], facname: String = "fac%d".format(fcount)): Int = {
		nodes += new IsAtMost1Factor(ncount, facname)
		while (edges.size < nodes.size) edges += new ArrayBuffer
		edges(nodes.size-1) += iidx //nodes.indexWhere(n => matches(n.name, iPattern))
		didxs.foreach(edges(nodes.size-1) += _)
		fcount += 1
		return ncount-1
	}
	
	def addTable2Factor(varname1: String, varname2: String, arity1: Int=0, arity2: Int=0, facname: String = "fac%d".format(fcount), fpots: Array[Potential]): Int = {
		val idx1 = vnames.getOrElse(varname1, -1)
		val idx2 = vnames.getOrElse(varname2, -1)
		assert (idx1 != -1 && idx2 != -1, "Var not found in addNamed2")
		assert(arity1 > 0 && arity2 > 0, "Table2 arities are not defined.")

		System.err.println("arity 1 = " + arity1)
		System.err.println("arity 2 = " + arity2)
		val npots = resize(fpots, arity1, arity2)
		System.err.println("created a %d x %d for %d pots.".format(npots.size, npots(0).size, fpots.size))
		nodes += new Table2Factor(ncount, facname, npots)

		while (edges.size < nodes.size) edges += new ArrayBuffer
		edges(nodes.size-1) += idx1
		edges(nodes.size-1) += idx2
		fcount += 1
		return ncount-1
	}
	
	def resize(opots: Array[Potential], arity1: Int, arity2: Int): Array[Array[Potential]] = {
		System.err.println(opots.size)
//		println(opots.mkString("\n"))
		System.err.println("desired = " + arity1 + " x " + arity2)
		Array.tabulate[Potential](arity1, arity2){case(i,j) => 
			val offset = (arity2 * i) + j // (arity1 * j) + i
//			opots(0)
//			System.err.println("(" + arity1 + " * " + i + ") + " + j)
			System.err.println(i + "__" + j + " = " + offset) 
			opots(offset)
//				opots(offset)
		}
	}
	
	def addNamed1Factor(varname: String, facname: String = "fac%d".format(fcount), fpots: Array[Potential]): Int = {
/*
		val idx = nodes.indexWhere(_.name == varname)
		assert(idx != -1, "Variable %s not found".format(varname))
		val fpots = findPots(pattern)
		val arity = nodes(idx).asInstanceOf[Variable].arity
		assert(fpots.size == arity, "The number of found potentials (%d) does not match arity of var: %s (%d)".format(fpots.size, varname, arity))
		nodes += new Named1Factor(ncount, facname, fpots) // .map(_._1), fpots.map(_._2))
		while (edges.size < nodes.size) edges += new ArrayBuffer
		edges(nodes.size-1) += idx
		fcount += 1
*/
//	System.err.println("named1 pots size = " + fpots.size)
		val idx = vnames.getOrElse(varname, -1)
		assert(idx != -1, "Variable %s not found".format(varname))
		nodes += new Named1Factor(ncount, facname, fpots)
		while (edges.size < nodes.size) edges += new ArrayBuffer
		edges(nodes.size-1) += idx
		fcount += 1
		return ncount-1
	}
	
	def addVariable(vname: String, arity: Int): Int = {
		System.err.println("arity = " + arity)
		assert(!vnames.contains(vname), "Variable %s is already defined in graph!".format(vname))
		nodes += new Variable(ncount, vname, arity)
		edges += new ArrayBuffer[Int]
		vnames(vname) = ncount
		vcount += 1
		return ncount-1
	}

	def addVariables(vnames: Array[String], arity: Int) = {
		for (name <- vnames) addVariable(name, arity)
	}

	def addUnaryFactor(varname: String, facname: String = "fac%d".format(fcount), fpots: Array[Potential]): Int = {
		val idx = vnames.getOrElse(varname, -1)
		assert(idx != -1, "Variable %s not found".format(varname))
		nodes += FactorFactory.createUnaryFactor(ncount, facname, fpots(0))
		while (edges.size < nodes.size) edges += new ArrayBuffer
		edges(nodes.size-1) += idx
		fcount += 1
		return ncount-1
	}

	def addUnaryFactorByIndex(idx: Int, facname: String = "fac%d".format(fcount), fpots: Array[Potential]): Int = {
		assert(idx != -1, "Invalid idx in Unary Factor construction.")
		nodes += FactorFactory.createUnaryFactor(ncount, facname, fpots(0))
		while (edges.size < nodes.size) edges += new ArrayBuffer
		edges(nodes.size-1) += idx
		fcount += 1
		return ncount-1
	}
	
	def addTable1Factor(varname: String, facname: String = "fac%d".format(fcount), tpots: Array[Potential]): Int = {
		val idx = vnames.getOrElse(varname, -1)
		assert(idx != -1, "Variable %s not found".format(varname))
		nodes += new Table1Factor(ncount, facname, tpots) 
		while (edges.size < nodes.size) edges += new ArrayBuffer
		edges(nodes.size-1) += idx
		fcount += 1
		return ncount-1
	}
	
	def findPots(pattern: String): Array[Potential] = pots.filter(_.name.matches(pattern))

	def matches(str: String, pattern: Regex): Boolean = pattern.findFirstIn(str).isDefined

	def ncount: Int = vcount + fcount

	def toFactorGraph: FactorGraph = {
		val nmap = nodes.toArray
		val emap = edges.map(_.toArray).toArray
//		println("Nodes\n" + nmap.mkString("\n"))
//		println
//		println("Edges\n" + emap.map(_.mkString(" ")).mkString("\n"))
		val graph =  FactorGraph.fromAdjacencyMatrix(nodes.toArray, edges.map(_.toArray).toArray)
		return graph
	}
	
	def check = {
		for (i <- 0 until edges.size; j <- edges(i)) {
			
			System.err.println("fac name i = " + i + "= " + nodes(i).name)
			System.err.println("var name j = " + j + "= " + nodes(j).name)
			val f = nodes(i).asInstanceOf[Factor]
			val v = nodes(j).asInstanceOf[Variable]
		}
	}
}




























/*
val pmap = pots.foldLeft(Map[String, Potential]())((map, pot) => map + (pot.name -> pot))


def addTable1Factor(varname: String, facname: String = "fac%d".format(fcount), tpots: Array[Potential]) {
val idx = nodes.indexWhere(_.name == varname)
assert(idx != -1, "Variable %s not found".format(varname))
nodes += new Table1Factor(ncount, facname, tpots) //new UnaryFactor(ncount, facname, fpots) // .map(_._1), fpots.map(_._2))
while (edges.size < nodes.size) edges += new ArrayBuffer
edges(nodes.size-1) += idx
fcount += 1		
}

def addNamed1Factor(pattern: String, varname: String, facname: String = "fac%d".format(fcount)) {
val idx = nodes.indexWhere(_.name == varname)
assert(idx != -1, "Variable %s not found".format(varname))
val fpots = findPots(pattern)
val arity = nodes(idx).asInstanceOf[Variable].arity
assert(fpots.size == arity, "The number of found potentials (%d) does not match arity of var: %s (%d)".format(fpots.size, varname, arity))
nodes += new Named1Factor(ncount, facname, fpots) // .map(_._1), fpots.map(_._2))
while (edges.size < nodes.size) edges += new ArrayBuffer
edges(nodes.size-1) += idx
fcount += 1
}

/*
def addNamed2Factor(pattern: String, facname: String = "fac%d".format(fcount), varname1: String, varname2: String) = {
val idx1 = nodes.indexWhere(_.name == varname1)
val idx2 = nodes.indexWhere(_.name == varname2)
val arity1 = nodes(idx1).asInstanceOf[Variable].arity
val arity2 = nodes(idx2).asInstanceOf[Variable].arity
val fpots = Array.ofDim[Potential](arity1, arity2)
val rpots = pots.filter(_.matches(Pattern))
for (i <- 0 until arity; j <- 0 until arity) {
fpots(i)(j) = 
}

}
*/

def addIsAtMost1Factor(indicPattern: String, pattern: String, facname: String = "fac%d".format(fcount)) {
nodes += new IsAtMost1Factor(ncount, facname)
while (edges.size < nodes.size) edges += new ArrayBuffer
edges(nodes.size-1) += nodes.indexWhere(_.name.matches(indicPattern))
for (idx <- 0 until nodes.size) {
if (nodes(idx).name.matches(pattern)) edges(nodes.size-1) += idx
}
fcount += 1
}


def addYouShallNotPassFactor(indicPattern: String, pattern: String, facname: String = "fac%d".format(fcount)) {
nodes += new YouShallNotPassFactor(ncount, facname)
while (edges.size < nodes.size) edges += new ArrayBuffer
edges(nodes.size-1) += nodes.indexWhere(_.name.matches(indicPattern))
for (idx <- 0 until nodes.size) {
if (nodes(idx).name.matches(pattern)) edges(nodes.size-1) += idx
}
fcount += 1		
}

/*
def addBigramChain(start: Int, end: Int) = {

}
*/


def addProjectiveTreeFactor(pattern: String, facname: String = "PTREE", slen: Int) {
nodes += new ProjectiveTreeFactor(ncount, facname, slen, false)
while (edges.size < nodes.size) edges += new ArrayBuffer
for (i <- 0 until nodes.size-1) {
if (nodes(i).name.startsWith(pattern) && nodes(i).isVariable) {
edges(nodes.size-1) += i
} 
}
fcount += 1
}

// If the pot must be found via pattern
def addUnaryFactor(pattern: String, varname: String, facname: String = "fac%d".format(fcount)) {
val idx = nodes.indexWhere(_.name == varname)
assert(idx != -1, "Variable %s not found".format(varname))
val arity = nodes(idx).asInstanceOf[Variable].arity
val fpots = findPots(pattern)
assert(fpots.size == 1, "Did not find a single potential for %s, a unary factor, instead found %d.".format(pattern, fpots.size))
nodes += FactorFactory.createUnaryFactor(ncount, facname, fpots(0)) //new UnaryFactor(ncount, facname, fpots) // .map(_._1), fpots.map(_._2))
while (edges.size < nodes.size) edges += new ArrayBuffer
edges(nodes.size-1) += idx
fcount += 1
}	


*/









/*
val ab = new ArrayBuffer[(String, Double)]
var i = 0
while (i < pots.size) {
if (names(i).matches(pattern)) ab += Tuple(names(i), pots(i))
i += 1
}
ab.toArray
}
*/

/*	
def addNamed2Factor(regex: Regex, varname1: String, varname2: String, facname: String = "fac%d".format(fcount)) {
val idx1 = nodes.indexWhere(_.name == varname1)
val idx2 = nodes.indexWhere(_.name == varname2)
//		val nbuf = new ArrayBuffer[String]
//		val sp = names.zip(pots)
//		val fpots = names.zip(pots).filter { case(name,score) => regex.findFirstIn(name) != None }
//		val fpots2 = (0 until nodes(idx1).asInstanceOf[Variable[Int]].arity).toArray.map { i =>
//			fpots.filter { case(fpot, score) => val regex(slot1, slot2) = fpot; slot1.toInt == i }.sortBy { case(rpot,score) => val regex(slot1, slot2) = rpot; slot2.toInt }.toArray
//		}.toArray
//		val names = fpots2.map(_.map(_._1))
//		val pots  = fpots2.map(_.map(_._2))


nodes += new Named2Factor(ncount, facname, fpots2.map(_.map(_._1), fpots2.map(_.map(_._2)) //, fpots.map(_._2))
while (edges.size < nodes.size) edges += new ArrayBuffer
edges(nodes.size-1) += idx1
edges(nodes.size-1) += idx2
fcount += 1
}
*/


//	def toMessageGraph: MessageGraph = {
	//		return new MessageGraph(nodes.toArray, edges.map(_.toArray).toArray)
	//	}
