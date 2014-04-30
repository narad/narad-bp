package narad.bp.structure
//import narad.structure.graph._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.util.matching._
//import narad.projects.relmarg._
//import narad.projects.bpdp._
import scala.math._


class CKYFactor(idx: Int, name: String, slen: Int) extends Factor(idx, name) {
  val INDICES_PATTERN = new Regex("brackvar\\(([0-9]+)\\,([0-9]+)\\)")

  def arity = slen

  def computeMessages(graph: FactorGraph, damp: Double, verbose: Boolean = false): Double = {
    if (verbose)		println("COMPUTING CKY MESSAGE!")
    val pegs = new ArrayBuffer[(Int,Int)]
    val alpha = Array.ofDim[Double](slen+1, slen+1)
    val beta  = Array.ofDim[Double](slen+1, slen+1)
    val score = Array.ofDim[Double](slen+1, slen+1)
    val grad  = Array.ofDim[Double](slen+1, slen+1)
    //	val cc = graph.edgesFrom(this).Array.size
    //		println(graph.toString)
    //		println("tree has %d neighbors".format(cc))

    for (edge <- graph.edgesFrom(this)) {
      //		println("the edge = " + edge.variable.name)
      edge.variable.name match {
        case INDICES_PATTERN(start, end) => {
          val i = start.toInt
          val k = end.toInt
          val m = edge.v2f
          assert(m.size == 2, "Message from Variable to CKY factor should be boolean, has arity %d instead.".format(m.size))
          score(i)(k) = log(m(1)) - log(m(0))
          if (verbose) System.err.println("  incoming from (%d,%d) = [F=%1.2f,T=%1.2f] = %1.2f".format(i, k, m(0), m(1), score(i)(k)))
          if (m(0) == 0) {
            score(i)(k) = 0
            pegs += ((i, k))
          }
        }
        case _ => System.err.println("ERROR IN CKY FACTOR - CONNECTED VAR (%s) DOES NOT MATCH PATTERN!".format(edge.variable.name))
      }
    }
//    println("PEGS = " + pegs.mkString(", "))

    for (peg <- pegs) {
      val start = peg._1
      val end = peg._2
      for (i <- 0 until start; j <- start+1 until end) score(i)(j) = Double.NegativeInfinity
      for (i <- start+1 until end; j <- end+1 to slen) score(i)(j) = Double.NegativeInfinity
    }

    for (w <- 2 until slen; i <- 0 to slen-w) {
      val k = i+w
      var inside = Double.NegativeInfinity
      for (j <- i+1 until k) inside = logIncrement(inside, beta(i)(j) + beta(j)(k))
      beta(i)(k) = inside + score(i)(k)
      //			println("BETA[%d,%d] = %f".format(i,k,beta(i)(k)))
    }

    score(0)(slen) = 0
    var logZ = Double.NegativeInfinity
    for (j <- 1 until slen) logZ = logIncrement(logZ, beta(0)(j) + beta(j)(slen))
    alpha(0)(slen) = -logZ
    if (verbose) System.err.println("-log Z = " + alpha(0)(slen))

    for (w <- 2 until slen; i <- 0 to slen-w) {
      alpha(i)(i+w) = Double.NegativeInfinity
      grad(i)(i+w) = Double.NegativeInfinity
    }

    for (w <- slen to 2 by -1; i <- 0 to slen-w) {
      val k = i+w
      val gpar = alpha(i)(k)
      val s = score(i)(k)
      //			println("GPAR[%d,%d] = %f".format(i,k, gpar))
      //			println("SSCORE[%d,%d] = %f".format(i,k, s))
      for (j <- i+1 until k) {
        //val first = alpha(i)(j)
        alpha(i)(j) = logIncrement(alpha(i)(j), gpar + beta(j)(k) + s)
        //				println("  Add 1 [%d,%d] = (%f + %f + %f + %f) = %f".format(i,j, first, gpar, beta(j)(k), s, alpha(i)(j)))
        alpha(j)(k) = logIncrement(alpha(j)(k), gpar + beta(i)(j) + s)
        //				println("  Add 2 [%d,%d] = %f".format(j,k, alpha(j)(k)))
        grad(i)(k)  = logIncrement(grad(i)(k),  gpar + beta(i)(j) + beta(j)(k))
      }
    }

// 		println("about to iterate over cky edges")
    for (edge <- graph.edgesFrom(this)) {
//      println(" --> " + edge.variable.name)
      edge.variable.name match {
        case INDICES_PATTERN(start, end) => {
          val i = start.toInt
          val k = end.toInt
          if (i != 0 || k != slen) {
            val m = Array[Double](1-scala.math.exp(score(i)(k) + grad(i)(k)), scala.math.exp(grad(i)(k)))
            val odds = m(1) / m(0)
            if (score(i)(k) == Double.NegativeInfinity || odds == Double.NegativeInfinity) {
              m(0) = 1
              m(1) = 0
            }
            else if (score(i)(k) == 0 || odds == Double.PositiveInfinity) {
              m(0) = 0
              m(1) = 1
            }
            if (verbose) System.err.println("  CKY --> (%d,%d) = [%f,%f]".format(i, k, m(0), m(1)))
            // if (verbose)
 //             println("--overwriting [%f, %f] with damp = %f".format(edge.f2v(0), edge.f2v(1), damp))
            edge.f2v = dampen(edge.f2v, m, damp)
            if (m.exists(_.isNaN) || edge.f2v.exists(_.isNaN)) {
              System.err.println("NaN Found in CKY Tree Factor:")
              System.err.println(" -- in message to %s = [%s]".format(edge.variable.getName, m.mkString(", ")))
              System.err.println(" -- resulting in edge.f2v of [%s]".format(edge.f2v.mkString(", ")))
              for (edge <- graph.edgesFrom(this)) {
                System.err.println("  <-- Incoming Message from %s = [%s]".format(edge.variable.name, edge.v2f.mkString(", ")))
              }
              System.err.println("log Z = " + logZ)
              for (i <- 0 until score.size; j <- 0 until score(i).size) {
                System.err.println("  score(%d)(%d) = ".format(i,j) + score(i)(j))
              }
              System.err.println
              for (i <- 0 until alpha.size; j <- 0 until alpha(i).size) {
                System.err.println("  alpha(%d)(%d) = ".format(i,j) + alpha(i)(j))
              }
              System.err.println
              for (i <- 0 until beta.size; j <- 0 until beta(i).size) {
                System.err.println("  beta(%d)(%d) = ".format(i,j) + beta(i)(j))
              }
              System.err.println
              for (i <- 0 until grad.size; j <- 0 until grad(i).size) {
                System.err.println("  grad(%d)(%d) = ".format(i,j) + grad(i)(j))
              }
            }
          }
        }
        case _ => System.err.println("ERROR IN CKY FACTOR - CONNECTED VAR (%s) DOES NOT MATCH PATTERN!".format(edge.variable.name))
      }
    }
    return 0
  }

  def getBeliefs(graph: FactorGraph): Array[Potential] = {
    return Array[Potential]()
    //		assert(true, "Method getBeliefs() for CKYFactor should not be called! (yet!)")
    //		return null.asInstanceOf[Array[(String, Double)]]
  }
}


class ProjectiveTreeFactor(idx: Int, name: String, slen: Int, multirooted: Boolean = false) extends Factor(idx, name) {
  val INDICES_PATTERN = new Regex(".*\\(([0-9]+)\\,([0-9]+)\\).*")

  def arity = slen

  def computeMessages(graph: FactorGraph, damp: Double, verbose: Boolean = false): Double = {
    if (verbose) System.err.println("Computing PTREE message!")
    val maxdim =  slen + 1
    val worksize = maxdim * maxdim
    val tkmat    = Array.ofDim[Double](worksize + maxdim)
    val gradmat  = Array.ofDim[Double](worksize + maxdim)

    val heads = Array.fill(slen)(-1)
    //		val edges = graph.edgesFrom(this)  - should use an iterator and manually advance
    val edges = graph.edgesFrom(this).toArray
    var ei = 0

    val groups = edges.groupBy{e =>
      val INDICES_PATTERN(start, end) = e.variable.name
      (start.toInt, end.toInt)
    }

    for (dep <- 1 to slen) {
      val tkoffset = dep * slen
      tkmat(tkoffset + dep - 1) = 0
      var trues = 0
      var trueHead = -1
      for (head <- 0 to slen if dep != head) {
        val earray = groups((head, dep))
        assert(earray.size == 1, "For head = %d and dep = %d, %d incoming edges were found in ProjectiveTreeFactor".format(head, dep, earray.size))
        val edge = earray(0)
        //				val edge = edges(ei)
        //				ei += 1
        //				assert(edge.variable.name == "linkvar(%d,%d)".format(head, dep), edge.variable.name + " did not match with head " + head + " and dep " + dep)
        val m = edge.v2f
        if (verbose)				System.err.println("DEBUG:  input message from kid " + dep + " and head " + head + " is = " + m.mkString(", "))
        if (m(0) == 0) {
          trues += 1
          trueHead = head
        }
        else {
          val score = m(1) / m(0)
          tkmat(head * slen + dep - 1) = score * -1.0
          tkmat(tkoffset + dep - 1) += score
        }
      }
      if (trues == 1) {
        heads(dep-1) = trueHead
        tkmat(tkoffset + dep - 1) = 1
        for (head <- 0 to slen if dep != head) {
          tkmat(head * slen + dep - 1) = if (head == trueHead) -1 else 0
        }
      }
      else if (trues > 1) {
        heads(dep-1) = -2
      }
      else {
        heads(dep-1) = -1
      }
    }
    val z = sumTree(tkmat, gradmat, slen, multirooted)
    ei = 0
    if (z == 0) {
      if (verbose)			System.err.println("DEBUG:  Z_ = 0 loop")
      for (dep <- 1 to slen) {
        val zdep = tkmat(dep * (slen + 1) - 1)
        val koffset = (dep - 1) * slen
        for (head <- 0 to slen if dep != head) {
          val m = heads(dep-1) match {
            case -2 =>   Array(Double.NaN, Double.NaN)
            case -1 =>   normalize(Array(1.0, zdep + tkmat(head * slen + dep - 1)))
            case _ => if (heads(dep-1) == head) Array(0.0, 1.0) else Array(1.0, 0.0)  // Doesn't like match comp with head
          }
          if (verbose) System.err.println("DEBUG:  to var " + edges(ei).variable.name)
          if (verbose) System.err.println("DEBUG:  " + m.mkString(" "))
          val edge = groups((head, dep))(0)
          edge.f2v = dampen(edge.f2v, m, damp)
          //          edges(ei).f2v = dampen(edges(ei).f2v, m, damp)
          ei += 1
        }
      }
      return 0
    }
    else {
      if (verbose)			System.err.println("DEBUG:  Z_ != 0 loop")
      for (dep <- 1 to slen) {
        val koff = (dep - 1) * slen
        val tkoff = dep * slen
        for (head <- 0 to slen if dep != head) {
          val m = heads(dep-1) match {
            case -2 =>	Array(Double.NaN, Double.NaN)
            case -1 =>  {
              val s = if (head > dep) 1 else 0
              val n = gradmat(koff + head - s)
              Array(1 + tkmat(head * slen + dep - 1) * n, n)
            }
            case _ => if (heads(dep-1) == head) Array(0.0, 1.0) else Array(1.0, 0.0)
          }
          if (verbose) System.err.println("DEBUG:  to var " + edges(ei).variable.name)
          if (verbose) System.err.println("DEBUG:  " + m.mkString(" "))
          val edge = groups((head, dep))(0)
          edge.f2v = dampen(edge.f2v, m, damp)
          if (m.exists(_.isNaN) || edge.f2v.exists(_.isNaN)) {
            System.err.println("NaN Found in PTree Factor:")
            for (edge <- graph.edgesFrom(this)) {
              System.err.println("  <-- Incoming Message from %s = [%s]".format(edge.variable.name, edge.v2f.mkString(", ")))
            }
            System.err.println("  Z = " + z)
            for (dep <- 1 to slen) {
              val tkoffset = dep * slen
              val doff = tkoffset + dep - 1
              println("  tkmat(%d) = ".format(doff, tkmat(doff)))

            }
          }
          //          edges(ei).f2v = dampen(edges(ei).f2v, m, damp)
          ei += 1
        }
      }
      return 0
    }
  }

  def sumTree(tkmat: Array[Double], gradmat: Array[Double], slen: Int, multirooted: Boolean = false, verbose: Boolean = false): Double = {
    val sch = Array.ofDim[Double](slen+1, slen+1, 2, 2)
    val gch = Array.ofDim[Double](slen+1, slen+1, 2, 2)
    var res = 0.0
    val start = if (multirooted) 0 else 1
    //		System.err.println("slen = %d".format(slen))
    for (i <- 0 until slen*slen) gradmat(i) = Double.NegativeInfinity
    for (s <- 0 to slen; i <- 0 to 1; j <- 0 to 1) sch(s)(s)(i)(j) = 0.0
    for (width <- 1 to slen; s <- start to slen) {
      val t = s + width
      //			System.err.println("s = %d; t = %d".format(s, t))
      if (t <= slen) {
        for (i <- 0 to 1; j <- 0 to 1) sch(s)(t)(i)(j) = Double.NegativeInfinity
        if (s > 0) {
          val lkid = log(-1.0 * tkmat(t * slen + s-1))
          for (r <- s until t) {
            //					System.err.println("DEBUG: \tsch(s)(r)(1)(1) = " + sch(s)(r)(1)(1))
            //					System.err.println("DEBUG: \tsch(r+1)(t)(0)(1) = " + sch(r+1)(t)(0)(1))
            //					System.err.println("DEBUG: \tkid = " + lkid)
            if (verbose) System.err.println("DEBUG:  sch(%d)(%d)(1)(1) += ".format(s,r) + sch(s)(r)(1)(1) + " + " + sch(r+1)(t)(0)(1) + " + " + lkid)
            sch(s)(t)(0)(0) = logIncrement(sch(s)(t)(0)(0), sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1) + lkid)
            //						System.err.println("sch(s)(t)(0)(0) after update 1 = " + sch(s)(t)(0)(0))
          }
        }
        val rkid = Math.log(-1.0 * tkmat(s * slen + t-1))
        for (r <- s until t) {
          sch(s)(t)(1)(0) = logIncrement(sch(s)(t)(1)(0), sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1) + rkid)
          //					System.err.println("sch(s)(t)(1)(0) update to %f when r = %d".format(sch(s)(t)(1)(0), r))
        }
        for (r <- s until t) {
          sch(s)(t)(0)(1) = logIncrement(sch(s)(t)(0)(1), sch(s)(r)(0)(1) + sch(r)(t)(0)(0))
          //					System.err.println("sch(s)(t)(0)(1) update to %f when r = %d".format(sch(s)(t)(0)(1), r))
        }
        for (r <- s+1 to t) {
          sch(s)(t)(1)(1) = logIncrement(sch(s)(t)(1)(1), sch(s)(r)(1)(0) + sch(r)(t)(1)(1))
          //					System.err.println("sch(s)(t)(1)(1) update to %f when r = %d".format(sch(s)(t)(1)(1), r))
        }
      }
    }
    if (!multirooted) {
      sch(0)(slen)(1)(1) = Double.NegativeInfinity
      for (r <- 1 to slen) {
        sch(0)(slen)(1)(1) = logIncrement(sch(0)(slen)(1)(1), sch(1)(r)(0)(1) + sch(r)(slen)(1)(1) + Math.log(-1.0 * tkmat(r-1)))
      }
    }
    res = sch(0)(slen)(1)(1)
    //		System.err.println("res = " + res)
    for (s <- 0 to slen; t <- s to slen; i <- 0 to 1; j <- 0 to 1) {
      gch(s)(t)(i)(j) = Double.NegativeInfinity
    }
    gch(0)(slen)(1)(1) = -1.0 * res
    if (!multirooted) {
      for (r <- 1 to slen) {
        gch(1)(r)(0)(1) = logIncrement(gch(1)(r)(0)(1),
          -1.0 * res + sch(r)(slen)(1)(1) + log(-1.0 * tkmat(r-1)))
        gch(r)(slen)(1)(1) = logIncrement(gch(r)(slen)(1)(1),
          -1.0 * res + sch(1)(r)(0)(1) + log(-1.0 * tkmat(r-1)))
        gradmat((r-1) * slen) = logIncrement(gradmat((r-1) * slen),
          -1.0 * res + sch(1)(r)(0)(1) + sch(r)(slen)(1)(1))
        if (verbose) System.err.println("gradmat(" + ((r-1) * slen) + " = " + gradmat((r-1) * slen))
      }
    }
    for (width <- slen to 1 by -1; s <- start to slen) {
      val t = s + width
      if (t <= slen) {
        var gpar = gch(s)(t)(1)(1)
        // System.err.println("T = " + t)
        // System.err.println("DEBUG:  ogpar1,1 (%d) = %f".format(t,gpar))
        //				System.err.println("gpar(%d) = %f".format(t,gpar))
        for (r <- s+1 to t) {
          gch(s)(r)(1)(0) = logIncrement(gch(s)(r)(1)(0), gpar + sch(r)(t)(1)(1))
          gch(r)(t)(1)(1) = logIncrement(gch(r)(t)(1)(1), gpar + sch(s)(r)(1)(0))
          //	System.err.println("gch(s)(r)(1)(0) update to %f when r = %d".format(gch(s)(r)(1)(0), r))
          //	System.err.println("gch(r)(t)(1)(1) update to %f when r = %d".format(gch(r)(t)(1)(1), r))
        }
        gpar = gch(s)(t)(0)(1)  // this seems to be s,r instead of s,t for some reason
        //				System.err.println("s is " + s)
        //				System.err.println("t is " + t)
        //				System.err.println(gch(2)(25)(1)(1))
        //				System.err.println(gch(2)(25)(0)(1))
        //	System.err.println("DEBUG:  ogpar0,1 (%d) = %f".format(t,gpar))
        for (r <- s until t) {
          gch(s)(r)(0)(1) = logIncrement(gch(s)(r)(0)(1), gpar + sch(r)(t)(0)(0))
          gch(r)(t)(0)(0) = logIncrement(gch(r)(t)(0)(0), gpar + sch(s)(r)(0)(1))
          //	System.err.println("gch(%d)(%d)(0)(1) update to %f when r = %d".format(s, r, gch(s)(r)(0)(1), r))
          //	System.err.println("gch(%d)(%d)(0)(0) update to %f when r = %d".format(r, t, gch(r)(t)(0)(0), r))
        }
        if (s > 0) {
          var lgrad = Double.NegativeInfinity
          val lkid = scala.math.log(-1.0 * tkmat(t * slen + s-1))
          gpar = gch(s)(t)(0)(0)
          //	System.err.println("lkid gpar when s > 0 = " + gpar)
          for (r <- s until t) {
            gch(s)(r)(1)(1) 	= logIncrement(gch(s)(r)(1)(1), gpar + sch(r+1)(t)(0)(1) + lkid)
            gch(r+1)(t)(0)(1) = logIncrement(gch(r+1)(t)(0)(1), gpar + sch(s)(r)(1)(1) + lkid)
            lgrad = logIncrement(lgrad, gpar + sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1))
            //	System.err.println("gch(%d)(%d)(1)(1) update to %f when r = %d".format(s, r, gch(s)(r)(1)(1), r))
            //	System.err.println("gch(%d)(%d)(0)(1) update to %f when r = %d".format(r+1, t, gch(r+1)(t)(0)(1), r))
            //	System.err.println("lgrad when r = %d is %f".format(r, lgrad))
          }
          //  System.err.println("DEBUG: lgrad = " + lgrad)
          gradmat((s-1) * slen + t-1) = logIncrement(gradmat((s-1) * slen + t-1), lgrad)
        }
        val rkid = Math.log(-1.0 * tkmat(s * slen + t-1))
        // System.err.println("DEBUG:  rkid = " + rkid)
        var rgrad = Double.NegativeInfinity
        gpar = gch(s)(t)(1)(0)
        //System.err.println("gpar final = " + gpar)
        for (r <- s until t) {
          gch(s)(r)(1)(1)   = logIncrement(gch(s)(r)(1)(1), gpar + sch(r+1)(t)(0)(1) + rkid)
          gch(r+1)(t)(0)(1) = logIncrement(gch(r+1)(t)(0)(1), gpar + sch(s)(r)(1)(1) + rkid)
          rgrad = logIncrement(rgrad, gpar + sch(s)(r)(1)(1) + sch(r+1)(t)(0)(1))
          //	System.err.println("rgrad when r = %d is %f".format(r, rgrad))
        }
        gradmat((t-1) * slen + s) = logIncrement(gradmat((t-1) * slen + s), rgrad)
      }
    }
    for (i <- 0 until slen * slen) {
      gradmat(i) = Math.exp(gradmat(i))
      if (verbose) System.err.println("DEBUG fradmat %d = %f".format(i, gradmat(i)))
    }
    return abs(res) //slog(res, 1)
  }

  /*
  gpar = gc->val[1][0];
  for ( r = s; r < t; ++r ) {
    log_incr(gch[s][r].val[1][1],
    gpar + sch[r+1][t].val[0][1] + rkid);
    log_incr(gch[r+1][t].val[0][1],
    gpar + sch[s][r].val[1][1] + rkid);
    log_incr(rgrad,
      gpar + sch[s][r].val[1][1] + sch[r+1][t].val[0][1]);
  }
  log_incr(gradmat[(t-1) * slen_ + s], rgrad);
}
}   */

  def normalize(v: Array[Double]): Array[Double] = {
    val sum = v.foldLeft(0.0)(_+_)
    v.map(_ / sum)
  }

  def getBeliefs(graph: FactorGraph): Array[Potential] = {
    return Array[Potential]()
  }
  /*
  def logIncrement(s: Double, x: Double): Double = {
    var d = 0.0
    if (s == Double.NegativeInfinity) {
      return x
    }
    else {
      d = s - x
      if (d >= 0) {
        if (d <= 745) {
          return s + Math.log(1.0 + Math.exp(-d))
        }
        else {
          return s
        }
      }
      else if (d < -745) {
        return x
      }
      else {
        return x + Math.log(1.0 + Math.exp(d))
      }
    }
  }
  */
}
