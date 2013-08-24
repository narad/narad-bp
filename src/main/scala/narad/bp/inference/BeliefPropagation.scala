package narad.bp.inference

import narad.bp.structure.ModelInstance

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

    var iters = if (model.isExact) 1 else options.INFERENCE_ITERATIONS
    for (i <- 0 until iters) {
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
      //System.err.println("Propagation time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")
      startTime = System.currentTimeMillis()
      if (i > 0 && maxDiff < options.DIFF_THRESHOLD) {
        return (true, i)
      }
      //System.err.println("DEBUG: original damp_rate = " + options.DAMP_RATE)
      if (verbose) System.err.println("DEBUG: damp update: damp=%f * drate=%f = %f".format(damp, drate, damp * drate))
      if (i > 1) damp *= drate
    }
    if (verbose) System.err.println("BP did not converge.")
    (false, iters)
  }
}