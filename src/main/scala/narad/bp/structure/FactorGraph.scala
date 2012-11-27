package narad.bp.structure
//import narad.structure.graph._
import scala.collection.mutable.{ArrayBuffer, HashMap, Queue}
import scala.util.matching._
//import narad.projects.relmarg._
//import narad.projects.bpdp._


trait FactorGraph { //extends Graph[MessageNode, MessageEdge] {
	def factors: Array[Factor]
	def variables: Array[Variable]
	def nodes: Array[MessageNode]
	def edgesFrom(n: MessageNode): Iterator[MessageEdge]
	def successors(n: MessageNode): Iterator[MessageNode]
//	def beliefs(n: MessageNode): Array[Double]
	def potentialBeliefs: Array[Potential]
	def variableBeliefs: Array[(String, Double)] 
}

object FactorGraph {


  def fromAdjacencyMatrix(nodelist: Array[MessageNode], dependencies: Array[Array[Int]]): FactorGraph = new FactorGraph {
		lazy val edgeList = (for (i <- 0 until size; j <- dependencies(i)) 
														yield new MessageEdge(nodelist(i).asInstanceOf[Factor],
																									nodelist(j).asInstanceOf[Variable])).toArray

		lazy val edgehash = constructMap
		
		def constructMap: HashMap[Int, Array[Int]] = {
			val ab = Array.fill(nodelist.size)(new ArrayBuffer[Int])
			for (i <- 0 until edgeList.size) {
				val e = edgeList(i)
				val fi = nodehash(e.factor)
				val vi = nodehash(e.variable)
				ab(fi) += i
				ab(vi) += i				
			}
			val map = new HashMap[Int, Array[Int]]
			var count = 0
			for (b <- ab) {
				map(count) = b.toArray
				count += 1
			}
			return map
		}
/*
			val map = new HashMap[Int, ArrayBuffer[Int]]
			for (i <- 0 until edgeList.size) {
				val e = edgeList(i)
				val fi = nodehash(e.factor)
				val vi = nodehash(e.variable)
				if (!map.contains(fi)){
					map(fi) = new ArrayBuffer(i)
				} 
				else {
					map(fi) += i
				}
				if (!map.contains(vi)){
					map(vi) = new ArrayBuffer(i)
				} 
				else {
					map(vi) += i
				}
			}
   	return map
	}
*/	

		lazy val nodehash = nodelist.zipWithIndex.foldLeft(Map[MessageNode, Int]())((m,p) => m + (p._1 -> p._2))


		val ifactors = nodelist.filter(_.isFactor).map(_.asInstanceOf[Factor]).toArray
		val ivariables = nodelist.filter(_.isVariable).map(_.asInstanceOf[Variable]).toArray
		
		def potentialBeliefs: Array[Potential] = {
			val beliefs = new ArrayBuffer[Potential]
			for (f <- factors) beliefs ++= f.getBeliefs(this)
			beliefs.toArray
		}
		
		def variableBeliefs = {
			val beliefs = new ArrayBuffer[(String, Double)]
			for (v <- variables) beliefs ++= v.getBeliefs(this)
			beliefs.toArray
		}
		
		// hoping this is real space arithmetic
		// method called from factors - variables have their own getBeliefs
/*
		def beliefs(n: MessageNode): Array[Double] = {
			val msize = messageSize(n)
			val res = new Array[Double](msize)
			for (i <- 0 until msize) res(i) = 1.0
			for (edge <- edgesFrom(n)) {
				for (i <- 0 until msize) res(i) *= edge.f2v(i) //edge.f2v(i) // changed to fix SRL Model, may have ruined parser model?
			}
			val sum = res.foldLeft(0.0)(_+_)
			return res.map(_ / sum)
		}		
*/
				
		def messageSize(n: MessageNode) = edges.next.f2v.size
		
		def nodes = nodelist
		
		def factors = ifactors
		
		def variables = ivariables

//		def nodes: Iterator[MessageNode] = nodelist.iterator        //for (n <- nodelist) yield n
		
//		def factors: Iterator[Factor] = ifactors.iterator           //for (f <- ifactors) yield f         //factors.iterator //for (n <- nodes if n.isFactor) //nodes.filter(_.isFactor).view.map(_.asInstanceOf[Factor]).iterator
		
//		def variables: Iterator[Variable[Int]] = ivariables.iterator         //for (v <- ivariables) yield v         //variables.iterator //nodes.filter(_.isVariable).view.map(_.asInstanceOf[Variable[Int]]).iterator

		def edges: Iterator[MessageEdge] = edgeList.iterator

		def edgesFrom(n: MessageNode): Iterator[MessageEdge] = (for (e <- edgehash.getOrElse(nodehash(n), Array[Int]())) yield edgeList(e)).iterator   //.map(edgeList(0)) //.map(edgeList(_)).iterator    //for (e <- edges if (e contains n)) yield e

//		def successors(node: MessageNode): Iterator[MessageNode] = dependencies(nodelist.indexOf(node)).view.map(nodelist(_)).iterator
//def successors(node: MessageNode): Iterator[MessageNode] = dependencies(nodehash(node)).view.map(nodelist(_)).iterator
	def successors(node: MessageNode): Iterator[MessageNode] = for (d <- dependencies(nodehash(node)).iterator) yield nodelist(d)     //(dependencies(nodehash(node)).foreach(yield nodelist(d)))
//dependencies(nodelist.indexOf(node)).view.map(nodelist(_)).iterator

		def size: Int = nodelist.size		

		override def toString: String = {
			val sb = new StringBuilder
			sb.append("FactorGraph[\n ")
			for (node <- factors){ //nodelist) {
				sb.append("  " + node.toString + "\n")
				for (neighbor <- successors(node)) {
					sb.append("    ==> " + neighbor.toString + "\n")
				}
			}
			sb.append("]")
			sb.toString
		}	
			
	
		def propagate(iterations: Int, dampStart: Double, dampRate: Double, threshold: Double): Boolean = {
			val queue = new Queue[MessageNode]
			queue ++= nodelist
//			queue ++= nodelist.filter(_.name.contains("brackfac")) ++ nodelist.filter(_.name.contains("labelfac")) ++ 
//								nodelist.filter(_.name.contains("var")) ++ nodelist.filter(_.name.contains("atMost")) ++ nodelist.filter(_.name.contains("CKY"))

			var i = 0; 
			var damp = dampStart; 
			var converged = false
			var maxDiff = -1.0
			while (i < iterations) {
				maxDiff = -1
				for (v <- queue) {
					var diff = v.computeMessages(this, damp)      //FG_[*nodep].node->compute_messages(*nodep, FG_, damp)
					if ( diff > maxDiff ) maxDiff = diff				
				}
				if ( iterations > 0 && maxDiff < threshold ) {
					return true
				}
				if ( iterations > 1 ) damp *= dampRate			
			}
			return false
		}
	}
}


































//			println("nodes:")
//			println(nodelist.mkString("\n"))
//			println("edges:")
//			for (i <- 0 until dependencies.size) {
//				println(i + ":  " + dependencies(i).mkString(", "))
//			}
//			println(edgeList.mkString("\n"))


//			edgeList.zipWithIndex.foldLeft(Map[Int, Array[Int]]()){(m, d) =>
//			val dv = d._1 // the edge
//			val di = d._2 // the edge indices into edgeList
//			m + (nodehash(d._1.factor) -> di) + (nodehash(d._1.variable) -> di)
	//	}
		
/*
		lazy val edgehash = dependencies.zipWithIndex.foldLeft(Map[Int, Array[Int]]()){(m,d) => 
			val dv = d._1
			val di = d._2
			val ab = new ArrayBuffer[Int]
			dv.foreach(ab += _)
			m + (di -> ab.toArray)
		} 
*/		
/*		
		
		lazy val edgehash2 = {
			val map = new HashMap
			for (i <- 0 until )
		}
			dependencies.zipWithIndex.foldLeft(Map[MessageEdge, Array[MessageEdge]]()){(m,d) => 
			val dv = d._1
			val di = d._2
			val ArrayBuffer[Int]
			for (d <- dv) {
				ab += d
			}
			m + (di -> ab.toArray)
		}
*/		
		
//.map(case(d,i) => d.foldLeft(Map[MessageEdge, Array[MessageEdg]())((m,di) => i -> di))
		
//		(for (i <- 0 until size)  j <- dependencies(i)) 
//			yield new MessageEdge(nodelist(i).asInstanceOf[Factor],
//																									nodelist(j).asInstanceOf[Variable[Int]])).toList


//		val edgehash = edgeList.foldLeft(Map[Int, Array[Int]]())((m,e) => m + (nodehash(e.factor) -> nodehash(e.variable))) ++ edgeList.foldLeft(Map[Int, Array[Int]]())((m,e) => m + (nodehash(e.variable) -> nodehash(e.factor)))
		

/*
		lazy val edgehash = createEdgeHash()
		
		def createEdgeHash(): = {
			val map = new HashMap
			for (edge <- edgeList) {
				map += 
			}

		}
*/
/*																									
		lazy val edgehash = dependencies.zipWithIndex.foldLeft(Map[Int, Int]){(m,ep) => 
			val ea = ep._1
			val ei = ep._2
			val ab = new ArrayBuffer[Tuple[Int, Int]]
			for (i <- 0 until ep._2; j <- dependencies(i)) {
				ab += (i,j)
			}
			m + (ab.toArray))
*/

	

	/*

	// A first pass on tying together Factor graphs and models in a natural way

	class MessageGraph(nodelist: Array[MessageNode], dependencies: Array[Array[Int]]) extends FactorGraph {

		lazy val edgeList = (for (i <- 0 until size; j <- 0 until dependencies(i).size) 
														yield new MessageEdge(nodelist(i).asInstanceOf[Factor],
																									nodelist(j).asInstanceOf[Variable[Int]])).toList

		def nodes = for (n <- nodelist) yield n

		def factors: Iterator[Factor] = nodes.filter(_.isFactor).map(_.asInstanceOf[Factor]).iterator

		def variables: Iterator[Variable[Int]] = nodes.filter(_.isVariable).map(_.asInstanceOf[Variable[Int]]).iterator

		def edges: Iterator[MessageEdge] = edgeList.iterator

		def edgesFrom(n: MessageNode): Iterator[MessageEdge] = for (e <- edges if (e contains n)) yield e

		def successors(node: MessageNode): Iterator[MessageNode] = dependencies(nodelist.indexOf(node)).map(nodelist(_)).iterator

		def size: Int = nodelist.size		

		override def toString: String = {
			println("nodes:")
			println(nodelist.mkString("\n"))
			println("edges:")
			for (i <- 0 until dependencies.size) {
				println(i + ":  " + dependencies(i).mkString(", "))
			}
			println(edgeList.mkString("\n"))
			val sb = new StringBuilder
			sb.append("FactorGraph[\n ")
			for (node <- nodelist) {
				sb.append("  " + node.toString + "\n")
				for (neighbor <- successors(node)) {
					sb.append("    ==> " + neighbor.toString + "\n")
				}
			}
			sb.append("]")
			sb.toString
		}	


		def propagate(iterations: Int, dampStart: Double, dampRate: Double, threshold: Double): Boolean = {
			val queue = new scala.collection.mutable.Queue[narad.inference.MessageNode]
			queue ++= nodelist

			var i = 0; 
			var damp = dampStart; 
			var converged = false
			var maxDiff = -1.0
			while (i < iterations) {
				maxDiff = -1
				for (v <- queue) {
					var diff = v.computeMessages(this, damp)      //FG_[*nodep].node->compute_messages(*nodep, FG_, damp)
					if ( diff > maxDiff ) maxDiff = diff				
				}
				if ( iterations > 0 && maxDiff < threshold ) {
					return true
				}
				if ( iterations > 1 ) damp *= dampRate			
			}
			return false
		}
	}

	*/


	
	
	/*
	Vertex FactorGraph::make_unary_factor(const string& name, Vertex v) {
	  Vertex f = add_vertex(FG_);
	  make_edge(f, v);
	  s2dmap::const_iterator p = pots_.find(name);
	  if ( p == pots_.end() ) error("Nonexistent potential for %s", name.c_str());
	  make_factor(f, new UnaryFactor(name, p->second));
	  return f;
	}
	*/
	

//		var i = -1
//		pots.filter{i += 1; names(i).matches(pattern)}
//	}
//		scoredpots.filter { case(pot,score) => pot.name.matches(pattern) }.toArray
//		var i = 0
//		while (i < pots.size) {
//			if (names(i).matches(pattern))
//		}
//	}





		/*
				val fpots = pots.filter { pot => regex.findFirstIn(pot.name) != None }.groupBy { pot =>
					val regex(slot1, slot2) = pot.name;  slot1
				}.map(_._2.toArray).toArray
				for (i <- 0 until fpots2.size; j <- 0 until fpots2(i).size) {
					println("[%d][%d] = ".format(i,j) + fpots2(i)(j))
				}
				println("fpots size = " + fpots2.size)
				println("fpots(0).size = " + fpots2(0).size)
		*/

		//		assert(idx1 != -1 && idx2 != -1, "Variable %s not found".format(varname))
		//		val fpots = findPots(pattern)
		//		println("found pots with pattern %s = %d".format(pattern, fpots.size))
		//		nodes += new UnaryFactor(ncount, facname, fpots.map(_._1), fpots.map(_._2))
		//		while (edges.size < nodes.size) edges += new ArrayBuffer
		//		edges(nodes.size-1) += idx1
		//		edges(nodes.size-1) += idx2
		//		fcount += 1








	
/*
	val nodes  = new ArrayBuffer[MessageNode]
	val edges = new ArrayBuffer[ArrayBuffer[Int]]
	
	def ncount: Int = vcount + fcount
	
	def addVariable(name: String, arity: Int) {
		if (nodes.find(_.name == name) == None) {
			nodes += new Variable[Int](ncount, name, (0 until arity).toArray)
			vcount += 1
		}
	}
	
	def addVariables(names: Array[String], arity: Int) {
		for (name <- names) addVariable(name, arity)
	}
	
	def addNamedFactor(pattern: String, varname: String, facname: String = "fac%d".format(fcount)) {
		val idx = nodes.indexWhere(_.name == varname)
		assert(idx != -1, "Variable %s not found".format(varname))
		nodes += new UnaryFactor(ncount, facname, findPots(pattern))
		addEdge(fcount, idx)
		addEdge(idx, fcount)
		fcount += 1
	}
	
	def addEdge(from: Int, to: Int) {
		while (edges.size <= from) {
			edges += new ArrayBuffer[Int]
		}
		while (edges(from).size <= to) {
			edges(from) += 0
		}
		edges(from)(to) = 1
	}
	
	def findPots(pattern: String): Array[Double] = {
		pots.zipWithIndex.filter { case(pot,idx) =>
			pot.name.matches(pattern)
		}.map { case(pot,idx) =>
			scores(idx)
		}.toArray
	}
	
	def toFactorGraph: FactorGraph = {
		val map = scala.collection.mutable.Map[MessageNode, Seq[MessageNode]]()
		nodes.zipWithIndex.foreach{ case(n,i) =>
			map(n) = edges(i).zipWithIndex.filter(_._1 == 1).map{ case(p,pi) => nodes(pi)}.toSeq 
		}
		return FactorGraph.fromAdjacencyList(map.toMap)
	}

	
}

*/

/*	


	val npots = pots.zipWithIndex.filter { case(pot,idx) => pot.name.matches(pattern) }.map { case(pot,idx) => scores(idx)}.toArray
	println("fonud pots = %d".format(npots.size))		
	for (i <- 0 until vars.size) {
		if (vars(i).name == varname) { 
			edges(i) += new UnaryFactor(ncount, "fac%d".format(vars.size-ncount), npots)
			ncount += 1		
		}
	}
}

def addVariables(names: Array[String], arity: Int) {

}

//	val vars  = new ArrayBuffer[Variable[Int]]
//	val edges = new ArrayBuffer[ArrayBuffer[Factor]]
	
	val redges = new ArrayBuffer[MessageEdge]


	def addNamedFactor(pattern: String, varname: String, arity: Int) {
		
	}
	
	
	def addVariables(names: Array[String], arity: Int) {
		for (name <- names) {
			val node = new Variable(ncount, name, (0 until arity).toArray)
			vars += node
			edges += new ArrayBuffer
		}
	}

	def addNamedFactor(pattern: String, varname: String) {
		val npots = pots.zipWithIndex.filter { case(pot,idx) => pot.name.matches(pattern) }.map { case(pot,idx) => scores(idx)}.toArray
		println("fonud pots = %d".format(npots.size))		
		for (i <- 0 until vars.size) {
			if (vars(i).name == varname) { 
				edges(i) += new UnaryFactor(ncount, "fac%d".format(vars.size-ncount), npots)
				ncount += 1		
			}
		}
	}
	
	def toFactorGraph: FactorGraph = {
		val ab = new ArrayBuffer[(Variable[Int], Seq[Factor])]
		for (i <- 0 until vars.size) {
			ab += Tuple(vars(i), edges(i).toSeq)
		}
		return FactorGraph.fromAdjacencyList(ab.toMap)
	}
	
}

*/




//var nrnodes = 0
//val adjList = scala.collection.mutable.Map[MessageNode, Seq[MessageNode]]()

//			adjList(node) = Seq[MessageNode]()
//			nrnodes += 1
			//adjList+= (node, Seq[MessageNode]) //(node) = Seq[MessageNode]





//	val nodes = new ArrayBuffer[MessageNode]
//	val edges = new ArrayBuffer[MessageEdge]

//			nodes += new Variable(nodes.size+1, name, Array.fill(arity)(7.7))

/*
for ( idx in 1:slen ) {
  fac <- sprintf("pos(%d,%%d)", idx-1)
	add.named1.factors(m, fac, vars[idx], default=1)
}

val var4 = new Variable(3, "wet glass", Array(true, false))
val fac1 = new Factor(4, "fac1", pots)
*/




















/*
class FactorGraph extends Graph[Node, Edge] {
	
}
*/


//class FactorGraph[T]() { //alist: Array[(Factor, Array[Variable[T]])]) { 
	//	val graph = graph.fromAdjacencyList()

	//}

/*	

abstract class Graph {
type Edge
type Node <: NodeIntf
abstract class NodeIntf {
def connectWith(node: Node): Edge
}
def nodes: Iterator[Node]
def edges: Iterator[Edge]
def successors(n: Node): Iterator[Node]
}

abstract class BipartiteGraph {
def unodes:
}

class FactorGraph extends Graph {
type Edge = EdgeImpl
type Node = NodeImpl
protected def newNode: Node = new NodeImpl
protected def newEdge(f: Node, t: Node): Edge =
new EdgeImpl(f, t)
}
*/

/*
abstract class DirectedGraph extends Graph {
type Edge <: EdgeImpl
class EdgeImpl(origin: Node, dest: Node) {
def from = origin
def to = dest
}
class NodeImpl extends NodeIntf {
def connectWith(node: Node): Edge = {
val edge = newEdge(this, node)
edges = edge :: edges
edge
}
}
protected def newNode: Node
protected def newEdge(from: Node, to: Node): Edge
var nodes: List[Node] = Nil
var edges: List[Edge] = Nil
def addNode: Node = {
val node = newNode
nodes = node :: nodes
node
}
}	
*/







//extends Graph {

	//	def addVariables()
	//	
	//	def addFactors()

	//}

/*
label.model <- function(pots, ...) {
p <- brack.model(pots, ...)
lfacs <- grep("spanLabel", names(pots), value="T")
lvars <- sub("^spanLabel", "Constit", lfacs)

add.variables(p, lvars, arity=2)
add.unary.factors(p, lfacs, lvars)

var.exp <- as.list(parse(text=lvars))
lefts <- sapply(var.exp, `[[`, 2)
rights <- sapply(var.exp, `[[`, 3)
offsets <- 1000 * lefts + rights
tapply(lvars, offsets, function(vars) {
fname <- sub("^Constit.*\\(", "isAtmost1(", vars[[1]])
indic <- sub("^Constit.*\\(", "B(", vars[[1]])
add.isatmost1.factor(p, fname, c(indic, vars))
})
p
}
*/