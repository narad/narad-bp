package narad.bp.structure
//import narad.structure.graph._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.util.matching._
//import narad.projects.relmarg._
//import narad.projects.bpdp._

abstract class MessageNode(val idx: Int, val name: String) {

  def arity: Int

  //def copy: MessageNode

	def computeMessages(fg: FactorGraph, damp: Double, verbose: Boolean = false): Double
	
	def dampen(rev: Array[Double], old: Array[Double], damp: Double): Array[Double] = {
//		println("OLD VEC: " + old.mkString(", "))
//		println("DAMP = " + damp)
		val kappa = old.foldLeft(0.0)(_+_)
		if (damp == 1) {
//			println("NEW VEC: " + old.map(_/kappa).mkString(", "))
			return old.map(_ / kappa)
		}
		else {
			var res = rev.map(_ * (1 - damp))
			res = res.zipWithIndex.map{case(e, i) => e + (damp / kappa) * old(i)}
//			println("NEW VEC: " + res.mkString(", "))
			return res
		}
	}

  def getName = name
/*
	def elementMultiplication(v1: Array[Double], v2: Array[Double]): Array[Double] = {
		return v1.zip(v2).map(p => p._1 * p._2)
	}
*/
  def clonePots(pots: Array[Potential]): Array[Potential] = {
    val cloned = new Array[Potential](pots.size)
    for (i <- 0 until pots.size) {
      cloned(i) = new Potential(pots(i).value, pots(i).name, pots(i).isCorrect)
    }
  cloned
  }

	def elementMultiplication(v1: Array[Double], pots: Array[Potential]): Array[Potential] = {
		val v2 = pots.map(_.clone()) //clonePots(pots) //pots.clone
		assert(v1.size == v2.size)
		for (i <- 0 until v2.size) v2(i).value *= v1(i)
		return v2
	}
	
	def elementMultiplicationInPlace(v1: Array[Double], v2: Array[Potential]): Unit = {
//		assert(v1.size == v2.size)
		for (i <- 0 until v2.size) v2(i).value *= v1(i)
	}
		
	def multiplyDown(vector: Array[Double], matrix: Array[Array[Double]]): Array[Array[Double]] = {
		matrix.zipWithIndex.map { case(mvec, midx) => vector.toList.zip(mvec).map(p => p._1 * p._2).toArray }
	}

	def multiplyAcross(vector: Array[Double], matrix: Array[Array[Double]]): Array[Array[Double]] = {
		vector.zipWithIndex.map { case(mess, midx) => matrix(midx).map(mess * _) }
	}
	
	def multiplyDown(vector: Array[Double], matrix: Array[Array[Potential]]): Array[Array[Double]] = {
		matrix.zipWithIndex.map { case(mvec, midx) => vector.toList.zip(mvec).map(p => p._1 * p._2.value).toArray }
	}

	def multiplyAcross(vector: Array[Double], matrix: Array[Array[Potential]]): Array[Array[Double]] = {
		vector.zipWithIndex.map { case(mess, midx) => matrix(midx).map(mess * _.value) }
	}
	
	def sumDown(matrix: Array[Array[Double]]): Array[Double] = {
		(0 until matrix.size).map{ i => 
			var sum = 0.0
			for (j <- 0 until matrix(i).size) sum += matrix(i)(j) 
			sum
		}
	}.toArray
	
	def sumAcross(matrix: Array[Array[Double]]): Array[Double] = {
		matrix.map(_.foldLeft(0.0)(_+_))
	}
	
	def mAcross(v1: Array[Double], v2: Array[Array[Double]], colwise: Boolean = true) = {
    System.err.println(v1.mkString(", "))
    System.err.println(" * ")
    System.err.println(v2.map(_.mkString(",")).mkString("\n"))
		val res = Array.ofDim[Double](v1.size, v2(0).size)
		for (i <- 0 until v1.size; j <- 0 until v2.size) {
			res(i)(j) = v1(i) * v2(i)(j)
		}
    System.err.println(" = ")
    System.err.println(res.map(_.mkString(",")).mkString("\n"))
    res
	}

  /*
  	def mAcross(v1: Array[Double], v2: Array[Array[Double]]) = {
		val res = Array.ofDim[Double](v1.size, v2(0).size)
		for (i <- 0 until v1.size; j <- 0 until v2.size) {
			res(i)(j) = v1(i) * v2(i)(j)
		}
		res
	}
   */

  def mDown(v1: Array[Double], v2: Array[Array[Double]]) = {
		val res = Array.ofDim[Double](v1.size, v2(0).size)
		for (i <- 0 until v1.size; j <- 0 until v2.size) {
			res(i)(j) = v1(j) * v2(i)(j)
		}
		res
	}

		def vprod(v1: Array[Double], v2: Array[Double]): Array[Double] = {
	//		System.err.println(v1.size + " vs " + v2.size)
			v1.zipWithIndex.map{case(e,i) => e * v2(i)}
		}

		def vdiv(v1: Array[Double], v2: Array[Double]): Array[Double] = {
			v1.zipWithIndex.map{case(e,i) => e / v2(i)}
		}

		def vadd(v1: Array[Double], v2: Array[Double]): Array[Double] = {
			v1.zipWithIndex.map{case(e,i) => e + v2(i)}
		}

		def vsub(v1: Array[Double], v2: Array[Double]): Array[Double] = {
	//		println(v1.size + " vs " + v2.size)
			v1.zipWithIndex.map{case(e,i) => e - v2(i)}
		}


/*	
	def mAcross(v1: Array[Double], v2: Array[Array[Potential]]) = {
		val res = Array.ofDim[Double](v1.size, v2(0).size)
		for (i <- 0 until v1.size; j <- 0 until v2.size) {
			res(i)(j) = v1(i) * v2(i)(j).value
		}
		res
	}


	def mDown(v1: Array[Double], v2: Array[Array[Potential]]) = {
		val res = Array.ofDim[Double](v1.size, v2(0).size)
		for (i <- 0 until v1.size; j <- 0 until v2.size) {
			res(i)(j) = v1(j) * v2(i)(j).value
		}
		res
	}
*/	
	

	def mAcross(v1: Array[Double], v2: Array[Array[Potential]]) = {
    System.err.println("multiply across:")
		System.err.println("new vec = %d x %d".format(v2.size, v2(0).size))
		assert(v2.size == v1.size, "(%d)x%d matrix v2 did not match vector dimensionality of %d in mAcross".format(v2.size, v2(0).size, v1.size))
		Array.tabulate(v2.size, v2(0).size){ case(i,j) =>
			System.err.println("i = " + i + "; j = " + j + "; val " + v1(i) + " * pot = " + v2(i)(j))
//			System.err.println("i = " + i + "; j = " + j)
	//		System.err.println("val " + v1(i) + " * pot = " + v2(i)(j))
      System.err.println("   = " + (v1(i) * v2(i)(j).value))
			v1(i) * v2(i)(j).value
		}
	}

  def mAcross(v1: Array[Double], v2: Array[Array[Array[Potential]]]) = {
    System.err.println("multiply across:")
    System.err.println("new vec = %d x %d".format(v2.size, v2(0).size))
    assert(v2.size == v1.size, "(%d)x%d matrix v2 did not match vector dimensionality of %d in mAcross".format(v2.size, v2(0).size, v1.size))
    Array.tabulate(v2.size, v2(0).size){ case(i,j) =>
      System.err.println("i = " + i + "; j = " + j + "; val " + v1(i) + " * pot = " + v2(i)(j))
      //			System.err.println("i = " + i + "; j = " + j)
      //		System.err.println("val " + v1(i) + " * pot = " + v2(i)(j))
      //System.err.println("   = " + (v1(i) * v2(i)(j).value))
      //v1(i) * v2(i)(j).value
      1
    }
  }

	def mDown(v1: Array[Double], v2: Array[Array[Potential]]) = {
    System.err.println("multiply down:")
		System.err.println("new vec = %d x %d".format(v2.size, v2(0).size))
		assert(v2(0).size == v1.size, "%dx(%d) matrix v2 did not match vector dimensionality of %d in mDown.".format(v2.size, v2(0).size, v1.size))
		Array.tabulate(v2.size, v2(0).size){ case(i,j) =>
			System.err.println("i = " + i + "; j = " + j + "; val " + v1(j) + " * pot = " + v2(i)(j))
//			System.err.println("i = " + i + "; j = " + j)
	//		System.err.println("val " + v1(i) + " * pot = " + v2(i)(j))
      System.err.println("   = " + (v1(j) * v2(i)(j).value))
			v1(j) * v2(i)(j).value
		}
	}
	
//		val res = Array.ofDim[Double](v1.size, v2(0).size)
//		for (i <- 0 until v1.size; j <- 0 until v2.size) {
//			res(i)(j) = v1(j) * v2(i)(j).value
//		}
//		res
//	}

  def multThrough(v: Array[Double], m2: Array[Array[Array[Potential]]], pivot: String="i"): Array[Array[Array[Potential]]] = {
    val m = m2.clone()
    for (i <- 0 until m.size; j <- 0 until m(0).size; k <- 0 until m(0)(0).size) {
          pivot match {
            case "i" => m(i)(j)(k).value = m(i)(j)(k).value * v(i)
            case "j" => m(i)(j)(k).value = m(i)(j)(k).value * v(j)
            case "k" => m(i)(j)(k).value = m(i)(j)(k).value * v(k)
        }
      }
    m
  }

  def sumThrough(m: Array[Array[Array[Double]]]): Array[Array[Double]] = {
    Array.tabulate(m.size, m(0).size) { case(i,j) =>
      m(i)(j).foldLeft(0.0)(_+_)
    }
  }


	def sAcross(m: Array[Array[Double]]): Array[Double] = {
		val res = Array.ofDim[Double](m.size)
		for (i <- 0 until m.size; j <- 0 until m(0).size) {
			res(i) += m(i)(j)
		}
		res
	}
	
	def sDown(m: Array[Array[Double]]): Array[Double] = {
			val res = Array.ofDim[Double](m(0).size)
			for (i <- 0 until m.size; j <- 0 until m(0).size) {
				res(j) += m(i)(j)
			}
			res
	}
	
	def printMatrix(m: Array[Array[Double]]) = {
		for (i <- 0 until m.size) {
			println("[%s]".format(m(i).mkString(", ")))
		}
	}
	
	def isFactor: Boolean = toString.contains("Factor")
	
	def isVariable: Boolean = toString.contains("Variable")
	
//	def isUnary: Boolean = arity == 1
	
	def normalize(v: Array[Potential]) = {
		val sum = v.foldLeft(0.0)(_+_.value)
		v.foreach(_.value /= sum)
//		println("NORMED: " + v.map(_.toString).mkString(", "))
	}

  def norm(v: Array[Double]): Array[Double] = {
    val sum = v.foldLeft(0.0)(_+_)
    v.map(_ / sum)
  }
		
	override def equals(that: Any): Boolean = {
		this.toString == that.toString
	}
	
	def truncate(x: Double) = "%.10f".format(x).toDouble
	
	override def toString = "Node%d[%s]".format(idx, name)
}


// An undirected edge with message-keeping
case class MessageEdge(factor: Factor, variable: Variable) {
//	println(factor + " ===> " + variable)
	var f2v = Array.fill(variable.arity)(1.0 / variable.arity)
	var v2f = Array.fill(variable.arity)(1.0 / variable.arity)
	def contains(n: MessageNode): Boolean = (factor == n || variable == n)
	def contains(n1: MessageNode, n2: MessageNode): Boolean = contains(n1) && contains(n2) // this si going to cause problems with self-loops
	
	override def toString = "%s ==> %s".format(factor.toString, variable.toString)
	
	override def equals(that: Any): Boolean = that match {
		case other: MessageEdge => factor.idx == other.factor.idx && variable.idx == other.variable.idx
		case _=> false
	}
//	{
//		this.toString == that.toString
//	}
}
