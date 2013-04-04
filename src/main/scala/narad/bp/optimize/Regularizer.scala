package narad.bp.optimize
import collection.mutable.HashMap

trait Regularizer {}

trait L2Regularizer extends Regularizer {
  self: Optimizer =>

	override def updateParams(pv: Array[Double], update: ParameterUpdate, scale: Double = -1.0, variance: Double = 0.0): Array[Double] = {
    if (variance > 0) {
      val reg = 1 - (-scale / variance)
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
  self: Optimizer =>

  lazy val rgroups = regularizationGroups

  lazy val rweights = Array(options.GROUP1_REG, options.GROUP2_REG, options.GROUP3_REG)

  override def updateParams(pv: Array[Double], update: ParameterUpdate, scale: Double = -1.0, variance: Double = 0.0): Array[Double] = {
    if (variance > 0) {
    val reg = 1 - (-scale / variance)
    for (i <- 0 until pv.size) {
      pv(i) = pv(i) * rweights(rgroups(i)) * reg
    }
  }
  for (i <- update.keys) {
    pv(i) += update(i) * scale
  }
  pv
}
}


trait ConfidenceWeightedL1Regularizer extends Regularizer {
  self: Optimizer =>

//  private val RATES

  override def updateParams(pv: Array[Double], update: ParameterUpdate, scale: Double = -1.0, variance: Double = 0.0): Array[Double] = {
    if (variance > 0) {
      val reg = 1 - (-scale / variance)
      for (i <- 0 until pv.size) {
        //    pv(i) = pv(i) * reg
      }
    }
    for (i <- update.keys) {
      pv(i) += update(i) * scale     // Doesn't support feat values > 1
    }
    pv
  }
}





//  private var currentIteration = 0
//  private val lastUpdate = new HashMap[Int, Int]



//	System.err.println("Updating with Regularization...")
//	System.err.println("scale = " + scale)
//	System.err.println("variance = " + variance)

/*
    if (variance > 0) {
      val reg = 1 - (-scale / variance)
      for (i <- 0 until pv.size) {
        pv(i) = pv(i) * reg
      }
    }
    for (i <- update.keys) {
      pv(i) += update(i) * scale     // Doesn't support feat values > 1
    }
    pv
*/
/*
    if (variance > 0) {
      val reg = 1 - (-scale / variance)
      for (i <- 0 until pv.size) {
        pv(i) = pv(i) * reg
      }
    }
*/
/*
 currentIteration += 1
 val reg = 1 - (-scale / variance)
 for (i <- update.keys) {
   pv(i) = pv(i) * reg * (currentIteration - lastUpdate.getOrElse(i, 0))
   pv(i) += update(i) * scale
   lastUpdate(i) = currentIteration
 }
*/

//currIter += 1

/*
  if ( var > 0 ) {
    double reg = 1 - (-scale / var);	// remove NEGATIVE when doing it right
    for ( int i = 0; i < nres; ++i ) {
      res[i] *= reg;
    }
  }
  int pcount = indices_.size();
  for ( int i = 0; i < pcount; ++i ) {
    double grad = pgrad[i];
    if ( grad == 0.0 ) continue;
    grad *= scale;
    const vector<int>& idx = indices_[i];
    const vector<double>& val = values_[i];
    int fcount = idx.size();
    for ( int f = 0; f < fcount; ++f ) res[idx[f]] += grad * val[f];
  }u
 */




