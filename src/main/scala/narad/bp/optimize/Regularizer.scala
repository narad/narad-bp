package narad.bp.optimize

trait Regularizer

trait L1Regularizer extends Regularizer {
  self: Optimizer =>
	
	override def updateParams(old: Array[Double], update: ParameterUpdate, scale: Double = -1.0, variance: Double = 0.0): Array[Double] = {
	//	System.err.println("Updating with Regularization...")
	//	System.err.println("scale = " + scale)
	//	System.err.println("variance = " + variance)

		val pv = old.clone                     // Memory Error here on English WSJ baseline <--
		if (variance > 0) {
			val reg = 1 - (-1.0 * scale / variance)  // David says remove negative when doing it right				
			for (i <- 0 until pv.size) {
				pv(i) = pv(i) * reg
			}
		}
		for (i <- update.keys) {
			pv(i) += update(i) * scale
		}
		pv
	}
}
		
trait GroupRegularizer extends Regularizer {
	
}		





