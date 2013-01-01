package narad.bp.inference
import narad.bp.structure._

trait InferenceMethod {
	
	def infer(model: ModelInstance, options: InferenceOptions): (Boolean, Int)
	//def infer(bpiters: Int = 5, drate: Double = 0.99, dinit: Double = -0.03, threshold: Double = .001, verbose: Boolean = false): (Boolean, Int)
	
}

trait BeliefPropagation extends InferenceMethod {

	def infer(model: ModelInstance, options: InferenceOptions): (Boolean, Int) = {
		var damp = options.DAMP_INIT  //dinit
		val drate = (1.0 - options.DAMP_RATE)
		val graph = model.graph
		val verbose = options.VERBOSE

    var j = 0
    // Pushing the unary factors first
    for (n <- graph.factors if graph.edgesFrom(n).size == 1) {
      val diff = n.computeMessages(graph, damp=1.0, verbose=verbose)
      if (verbose) System.err.println("DEBUG: computing messages for [idx=" + j + "] for " + n.name + " [Update difference of " + diff + "].")
      j += 1
    }

		for (i <- 0 until options.INFERENCE_ITERATIONS) {
			var maxDiff = -1.0
			var startTime = System.currentTimeMillis()
      // Switching to the model-specific inference ordering
			for (n <- model.messageOrder(graph)) {
				val diff = if (n.isFactor && n.arity == 1) {
					n.computeMessages(graph, damp=1.0, verbose=verbose)
				}
				else {
					n.computeMessages(graph, damp=damp, verbose=verbose)
				}
				if (verbose) System.err.println("DEBUG: computing messages for [idx=" + j + "] for " + n.name + " [Update difference of " + diff + "].")
				if (diff > maxDiff) maxDiff = diff
        j += 1
			}
			System.err.println("Propagation time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")
			startTime = System.currentTimeMillis()
			if (i > 0 && maxDiff < options.DIFF_THRESHOLD) {
				return (true, i)
			}
      System.err.println("DEBUG: original damp_rate = " + options.DAMP_RATE)
      System.err.println("DEBUG: damp update: damp=%f * drate=%f = %f".format(damp, drate, damp * drate))
			if (i > 1) damp *= drate
		}
		System.err.println("BP did not converge.")
		(false, options.INFERENCE_ITERATIONS)
	}
}

/*
trait HiddenVariableBeliefPropagation extends InferenceMethod {

  def infer(model: ModelInstance, options: InferenceOptions): (Boolean, Int) = {
    val m1 = model.clone()
    val m2 = model.clone()
    super.infer(m1, options)
    super.infer(m2, options)

  }
}
 */

trait InferenceOptions {
	
	def DAMP_INIT: Double
	
	def DAMP_RATE: Double
	
	def DIFF_THRESHOLD: Double

	def INFERENCE_ITERATIONS: Int
	
	def VERBOSE: Boolean
	
}

trait InferenceOrder {
	
	def messageOrder(graph: FactorGraph): Iterator[MessageNode] = {
    System.err.println("Using Default Inference Order...")
		val mqueue = scala.collection.mutable.Queue[MessageNode]() 
		for (fac <- graph.factors) {
			if (graph.edgesFrom(fac).size == 1)
				mqueue += fac
			else
				mqueue += fac
		}
		for (v <- graph.variables) {
			mqueue += v
		}
		mqueue.iterator
	}		
}
























/*
	self: FactorGraphModel =>

	def infer(bpiters: Int = 5, drate: Double = 0.99, dinit: Double = -0.03, threshold: Double = .001, verbose: Boolean = false): (Boolean, Int) = {

		var damp = options.DAMP_INIT  //dinit
		for (i <- 0 until bpiters) {
			var maxDiff = -1.0
			var startTime = System.currentTimeMillis()
			for (n <- messageOrder(graph)) {
				val diff = if (n.isFactor && graph.edgesFrom(n).toArray.size == 1) {
					n.computeMessages(graph, damp=1.0, verbose)
				}
				else {
					n.computeMessages(graph, damp=damp, verbose)
				}
				if (verbose) System.err.println("...computing messages for " + n.name + " [Update difference of " + diff + "].")
				if (diff > maxDiff) maxDiff = diff
			}
			System.err.println("Propagation time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")
			startTime = System.currentTimeMillis()
			if (i > 0 && maxDiff < threshold) {
				return (true, i)
			}
			if (i > 1) damp *= drate
		}
		System.err.println("BP did not converge.")
		return (false, bpiters)
	}
}
*/
