package narad.bp.train
import narad.bp.structure._
import narad.bp.util._
//import narad.structure.graph._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.util.matching._
//import narad.projects.relmarg._
//import narad.projects.bpdp._
//import narad.projects.cparser._


object SGDTrainer {
	
		def train(params: Array[Double], fidxFile: String, constructor: (PotentialExample, Array[Potential]) => Model, maxExamples: Int = Int.MaxValue, 
							rate: Double = -0.01, variance: Double = 0.0, bpIters: Int = 5, verbose: Boolean = false): Array[Double] = {
			var count = 0
			var pvv = params.clone
			val damp = 0.099
			for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {

				val feats   = ex.getFeatures				
				val pots    = ex.getPotentials
				pots.foreach { pot => pot.value = feats(pot.name).foldLeft(0.0)((sum, feat) => sum + params(feat.idx) * feat.value) }
				pots.foreach { pot => pot.value = Math.exp(pot.value) }
	
				val model = constructor(ex, pots)
				if (verbose) println("\nGraph:\n%s".format(model.graph.toString))
				if (verbose) println("\nPre-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
				var startTime = System.currentTimeMillis()
				runBP(model.graph, bpIters, damp, 0.1, verbose = verbose)
				System.err.println("Propagation time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
				val beliefs = model.graph.potentialBeliefs

				if (verbose) println("\nPost-BP Beliefs:\n%s".format(beliefs.map(_.toString).mkString("\n")))
				val margs = beliefs.map( b => if (b.isCorrect) b.value - 1.0 else b.value)
				pvv = margupdate(pvv, beliefs.map(p => feats(p.name)), margs, rate, variance)
				if (verbose) println("\nUpdated Param Vector:\n%s".format(pvv.zipWithIndex.map{case(e,i) => "%d\t%f".format(i,e)}.mkString("\n")))
				count += 1
			}
			return pvv
		}	


		def test(params: Array[Double], constructor: (PotentialExample, Array[Potential]) => Model, ex: PotentialExample, 
		rate: Double = -0.01, variance: Double = 0.0, bpIters: Int = 5, verbose: Boolean = false): Model = {
			var count = 0
			var pvv = params.clone
			val damp = 0.099
			val feats   = ex.getFeatures				
			val pots    = ex.getPotentials
			pots.foreach { pot => pot.value = feats(pot.name).foldLeft(0.0)((sum, feat) => sum + params(feat.idx) * feat.value) }
			pots.foreach { pot => pot.value = Math.exp(pot.value) }

			val model = constructor(ex, pots)
			if (verbose) println("\nGraph:\n%s".format(model.graph.toString))
			if (verbose) println("\nPre-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
			runBP(model.graph, bpIters, damp, 0.1, verbose = verbose)
			if (verbose) println("\nPost-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
			model
		}	



/*
	def computePotentials(params: Array[Double], feats: Array[Array[Feature]]): Array[Double] = {
		feats.map(x => x.foldLeft(0.0)((sum, feat) => sum + params(feat.idx) * feat.value))
	}
*/
		def runBP(graph: FactorGraph, bpiters: Int = 5, drate: Double = 0.99, dinit: Double = -0.03, threshold: Double = .001, verbose: Boolean = false): Boolean = {
			val model = graph
			var maxDiff = -1.0
			var damp = dinit
			val mqueue = scala.collection.mutable.Queue[MessageNode]() 
			val uqueue = scala.collection.mutable.Queue[MessageNode]() 
			for (fac <- model.factors) {
				if (model.edgesFrom(fac).size == 1)
					uqueue += fac
				else
					mqueue += fac
			}
			for (v <- model.variables) {
				mqueue += v
			}
			// Do the unary facs first with no damping
			for (u <- uqueue) {
				u.computeMessages(model, damp=1, verbose)
			}
			// Do the others with successive damping
			for (i <- 0 until bpiters) {
				if (verbose) println("Performing BP iteration %d".format(i))
				for (v <- mqueue) {
					maxDiff = -1.0
					var diff = v.computeMessages(model, 1, verbose) //damp=damp)
					if (diff > maxDiff) maxDiff = diff
				}
				if (i > 0 && maxDiff < threshold) {
					return true
				}
				if (i > 1) damp *= drate
			}
			return false			
		}

		def margupdate(oldpv: Array[Double], feats: Array[Array[Feature]], margs: Array[Double], rate: Double = 1.0, variance: Double = 0.0): Array[Double] = {
//			println("marg update rate = " + rate)
//			println("marg update variance = " + variance)
			var newpv = oldpv
			assert(feats.size == margs.size, "feats and marg arguments to margupdate were not identically sized (%d to %d).".format(feats.size, margs.size))
			// Apply Regularization
			if (variance > 0) {
				val reg = 1 - (-1.0 * rate / variance)  // David says remove negative when doing it right
				newpv = newpv.map(_ * reg)
			}
			// Update Param Vector
			for (i <- 0 until margs.size) {
				val grad = margs(i) * rate
				if (grad != 0.0) {
					for (j <- 0 until feats(i).size) {
						val feat = feats(i)(j)
						newpv(feat.idx) += grad * feat.value
					}
				}
			}
			return newpv
		}

		def hiddenMarginalization(pots: Array[Potential], model: Model, bpIters: Int = 10, damp: Double = 0.99): Array[Potential] = {
			val conv = runBP(model.graph, bpIters, damp)
			val den  = model.graph.potentialBeliefs
			
			for (factor <- model.graph.factors) {
				if (factor.getClass == classOf[UnaryFactor]) {
					val fc = factor.asInstanceOf[UnaryFactor]
					if (fc.pots(0).isCorrect) fc.peg
//					val idx = names.indexOf(fc.potname)
//					assert(idx >= 0, "Potname %s not found in names vector in hidden marg.".format(fc.potname))
//					if (correct(idx) fc.peg
				}
			}
			val convNum  = runBP(model.graph, bpIters, damp)
			val num = model.graph.potentialBeliefs
			for (i <- 0 until den.size) den(i).value -= num(i).value
			return den //den.zipWithIndex.map{case(v, idx) => v.value - num(idx).value}
		}
		
		def init(initFile: String, pvsize: Int = 0): Array[Double] = {
			assert(initFile != "null" || pvsize > 0, "Both init.file and pv.size are not specified correctly.")
			if (initFile == "null") {
				return Array.fill(pvsize+1)(0.0)
			}
			else {
				val params1 = io.Source.fromFile(initFile).getLines().map(_.toDouble).toArray
				val params = Array[Double](0.0) ++ params1
				if (pvsize > params.size) {
					return params ++ Array.fill(pvsize - (params.size+1))(0.0)
				}
				else {
					return params
				}
			}
		}
}














