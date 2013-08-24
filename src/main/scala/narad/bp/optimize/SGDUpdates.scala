package narad.bp.optimize
import narad.bp.inference._
import narad.bp.structure._
import narad.bp.util._

trait TrainingMethod {}

trait SGDUpdates extends TrainingMethod {

		def update(model: ModelInstance, options: OptimizerOptions): ParameterUpdate = {
			val rate = options.RATE
			//System.err.println("rate = " + rate)
			val pv = new ParameterUpdate
			val beliefs = model.marginals.sortBy(_.name)
			val feats = model.features
			val margs = beliefs.collect{case b if (feats.contains(b.name)) => if (b.isCorrect) b.value - 1.0 else b.value} //.map(truncate(_))
      assert(!margs.exists(_.isNaN), "NaN Found in BP marginals:\n%s".format(beliefs.mkString("\n")))
      System.err.println("POST-BP")
      beliefs.foreach(System.err.println(_))
			val fnames = beliefs.collect{case p if feats.contains(p.name) => feats(p.name)}
      println("FNAMES:")
      fnames.foreach(System.err.println(_))
      System.err.println("DEBUG: Post-BP MARGINALS:")
			for (i <- 0 until margs.size) {
        System.err.println("DEBUG: post-bp marg[ " + beliefs(i).name + " ] =  " +  margs(i))
      }

			val updates = margs
			for (i <- 0 until updates.size if updates(i) != 0.0) {
				val grad = updates(i) //* rate
				for (j <- 0 until fnames(i).size) {
					val fidx = fnames(i)(j).idx
            val oval = pv.getOrElse(fidx, "-500")
//					pv(fidx) = pv(fidx) + grad * fnames(i)(j).value
						pv(fidx) = pv.getOrElse(fidx, 0.0) + grad * fnames(i)(j).value
            if (pv(fidx).isNaN) {
              System.err.println("NaN Found in SGD Update:")
              System.err.println("  fidx = " + fidx)
              System.err.println("  pv = " + oval)
              System.err.println("  grad = " + grad)
              System.err.println("  value = " + fnames(i)(j).value)
              assert(!pv(fidx).isNaN, "SGD Update produced a NaN.")
            }
				}
			}
			return pv
		}

  def hiddenUpdate(instance1: ModelInstance, instance2: ModelInstance, options: OptimizerOptions): ParameterUpdate = {
    val denMargs = instance1.marginals
    val numMargs = instance2.marginals

    for (i <- 0 until denMargs.size) {
      denMargs(i).value = denMargs(i).value - numMargs(i).value
    }
    val beliefs = denMargs.sortBy(_.name)

    val rate = options.RATE
    val pv = new ParameterUpdate
    val feats = instance1.features

    val margs = beliefs.map(_.value)
    val fnames = beliefs.collect{case p if feats.contains(p.name) => feats(p.name)}
    val updates = margs
    for (i <- 0 until updates.size if updates(i) != 0.0) {
      val grad = updates(i) //* rate
      for (j <- 0 until fnames(i).size) {
        val fidx = fnames(i)(j).idx
        val oval = pv.getOrElse(fidx, "-500")
        pv(fidx) = pv.getOrElse(fidx, 0.0) + grad * fnames(i)(j).value
        if (pv(fidx).isNaN) {
          System.err.println("NaN Found in SGD Update:")
          System.err.println("  fidx = " + fidx)
          System.err.println("  pv = " + oval)
          System.err.println("  grad = " + grad)
          System.err.println("  value = " + fnames(i)(j).value)
          assert(!pv(fidx).isNaN, "SGD Update produced a NaN.")
        }
      }
    }
    return pv
  }
}

























  /*
    val rate = options.RATE
    //System.err.println("rate = " + rate)
    val pv = new ParameterUpdate
    val beliefs = model.marginals.sortBy(_.name)
    val feats = model.features
    val margs = beliefs.map( b => if (b.isCorrect) b.value - 1.0 else b.value) //.map(truncate(_))
    assert(!margs.exists(_.isNaN), "NaN Found in BP marginals:\n%s".format(beliefs.mkString("\n")))
    //System.err.println("POST-BP")
    //margs.foreach(System.err.println(_))
    val fnames = beliefs.collect{case p if feats.contains(p.name) => feats(p.name)}
    //      System.err.println("DEBUG: Post-BP MARGINALS:")
    //			for (i <- 0 until margs.size) {
    //        System.err.println("DEBUG: post-bp marg[ " + beliefs(i).name + " ] =  " +  margs(i))
    //      }

    val updates = margs
    for (i <- 0 until updates.size if updates(i) != 0.0) {
      val grad = updates(i) //* rate
      for (j <- 0 until fnames(i).size) {
        val fidx = fnames(i)(j).idx
        //            val oval = pv.getOrElse(fidx, "-500")
        //					pv(fidx) = pv(fidx) + grad * fnames(i)(j).value
        pv(fidx) = pv.getOrElse(fidx, 0.0) + grad * fnames(i)(j).value
        if (pv(fidx).isNaN) {
          System.err.println("NaN Found in SGD Update:")
          System.err.println("  fidx = " + fidx)
          System.err.println("  pv = " + oval)
          System.err.println("  grad = " + grad)
          System.err.println("  value = " + fnames(i)(j).value)
          assert(!pv(fidx).isNaN, "SGD Update produced a NaN.")
        }
      }
    }
    return pv
  }
}
               */





















/*
trait SGDUpdates extends TrainingMethod {

		def update(model: ModelInstance, pvsize: Int, options: OptimizerOptions): Array[Double] = {
			val rate = options.RATE
			System.err.println("rate = " + rate)
			val pv = new Array[Double](pvsize)
			val beliefs = model.marginals
			val feats = model.features
			val margs = beliefs.map( b => if (b.isCorrect) b.value - 1.0 else b.value) //.map(truncate(_))
			val fnames = beliefs.collect{case p if feats.contains(p.name) => feats(p.name)}
			System.err.println("MARGINALS:")
			for (i <- 0 until margs.size) { System.err.println("MARG: " + beliefs(i).name + "\t" + margs(i))}
			val updates = margs
			for (i <- 0 until updates.size if updates(i) != 0.0) {
				val grad = updates(i) //* rate
				for (j <- 0 until fnames(i).size) {
					val fidx = fnames(i)(j).idx
					pv(fidx) = pv(fidx) + grad * fnames(i)(j).value
				}
			}
			return pv
		}
}
*/

// trait TrainingOptions {}

//	self: FactorGraphModel with InferenceMethod =>
//	
//			def train(params: Array[Double], fidxFile: String, maxExamples: Int = Int.MaxValue, 
//								rate: Double = -0.01, variance: Double = 1.0, bpIters: Int = 5, verbose: Boolean = false): Array[Double] = {


/*
				var count = 0
				var pvv = params.clone
				val damp = 0.099
				for (ex <- PotentialReader.read(fidxFile) if count < maxExamples) {

					var startTime = System.currentTimeMillis()
					val feats   = ex.getFeatures				
					val pots    = ex.getPotentials
					pots.foreach { pot => pot.value = feats(pot.name).foldLeft(0.0)((sum, feat) => sum + pvv(feat.idx) * feat.value) }
					pots.foreach { pot => pot.value = Math.exp(pot.value) }
					System.err.println("Setup time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		

					startTime = System.currentTimeMillis()
					val model = construct(ex, pots)
					if (verbose) println("\nInitial Params:\n%s".format(params.zipWithIndex.mkString("\n")))
					if (verbose) println("\nGraph:\n%s".format(model.graph.toString))
					println("\nGraph:\n%s".format(model.graph.toString))
					if (verbose) println("\nPre-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
					System.err.println("Construction time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		

					startTime = System.currentTimeMillis()
					val stats = infer(bpIters, drate=rate, dinit=1, verbose = verbose)
					System.err.println("Propagation time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
					System.err.println("Converged? " + stats._1 + " in " + stats._2 + " iterations.")



					startTime = System.currentTimeMillis()
					val beliefs = model.graph.potentialBeliefs.filter(_.name != "null")
					System.err.println("Gathering beliefs time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		

					if (verbose) {
						println("Post-BP1 (%d beliefs)".format(beliefs.size))
						for (i <- 0 until beliefs.size) { System.err.println(beliefs(i)) }
					}

					startTime = System.currentTimeMillis()
					assert(!beliefs.exists(_.value.isNaN), "NaN detected in post-BP marginals.\nExample:\n%s\nParams:\n%s".format(ex.attributes, pvv.mkString("\n")))
					val margs = beliefs.map( b => if (b.isCorrect) b.value - 1.0 else b.value) //.map(truncate(_))

					if (verbose) {
						println("Post-BP (%d beliefs)".format(beliefs.size))
						for (i <- 0 until beliefs.size) {
	//						if (beliefs(i).name == "brack(0,16)") margs(i) = 0.0
							System.err.println(beliefs(i).name + "\t" + margs(i))
	//						System.err.println(c + " = " + beliefs(c).toString)
						}
					}
					System.err.println("Map part of update time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
					val fnames = beliefs.collect{case p if feats.contains(p.name) => feats(p.name)} //filter(feats.contains(_.name)).map(p => feats(p.name))
					System.err.println("And with the belief mapping in the update time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
					startTime = System.currentTimeMillis()
					margupdateInPlace(pvv, fnames, margs, rate, variance)
	//				pvv = margupdate(pvv, beliefs.map(p => feats(p.name)), margs, rate, variance)
					assert(!pvv.exists(_.isNaN), "NaN detected in parameter vector.\nExample:\n%s".format(ex.attributes))

					System.err.println("Update time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
					if (verbose) println("\nUpdated Param Vector:\n%s".format(pvv.zipWithIndex.map{case(e,i) => "%d\t%f".format(i,e)}.mkString("\n")))
					System.err.println
					count += 1
				}
				return pvv
			}
			
			
					def margupdate(oldpv: Array[Double], feats: Array[Array[Feature]], margs: Array[Double], rate: Double = 1.0, variance: Double = 0.0): Array[Double] = {
			//			println("marg update rate = " + rate)
			//			println("marg update variance = " + variance)
						System.err.println("Updating marginals (immutable mode) with rate %f and variance %f".format(rate, variance))
						val newpv = if (variance > 0) {
							val reg = 1 - (-1.0 * rate / variance)  // David says remove negative when doing it right
							oldpv.map(_ * reg)				
						}
						else {
							oldpv
						}

			//			var newpv = oldpv
						assert(feats.size == margs.size, "feats and marg arguments to margupdate were not identically sized (%d to %d).".format(feats.size, margs.size))
						// Apply Regularization
			//			if (variance > 0) {
			//				val reg = 1 - (-1.0 * rate / variance)  // David says remove negative when doing it right
			//				newpv = newpv.map(_ * reg)
			//			}
						// Update Param Vector

						for (i <- 0 until margs.size if margs(i) != 0.0) {
								val grad = margs(i) * rate
								for (j <- 0 until feats(i).size) {
			//						val feat = 
									val fidx = feats(i)(j).idx
									newpv(fidx) = newpv(fidx) + grad * feats(i)(j).value
							}
						}
						return newpv
					}

							def margupdateInPlace(pv: Array[Double], feats: Array[Array[Feature]], margs: Array[Double], rate: Double = 1.0, variance: Double = 0.0) = {
								assert(feats.size == margs.size, "feats and marg arguments to margupdate were not identically sized (%d to %d).".format(feats.size, margs.size))
								System.err.println("Updating marginals (in place) with rate = %f, variance = %f, and pv size of %d".format(rate, variance, pv.size))
								// Apply Regularization
								val startTime = System.currentTimeMillis()
								if (variance > 0) {
									val reg = 1 - (-1.0 * rate / variance)  // David says remove negative when doing it right				
					//				pv.foreach(_ * reg)				
									for (i <- 0 until pv.size) {
										pv(i) = pv(i) * reg
									}
								}
								System.err.println("Reg time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		

								// Update Param Vector
								for (i <- 0 until margs.size if margs(i) != 0.0) {
									val grad = margs(i) * rate
									for (j <- 0 until feats(i).size) {
										val fidx = feats(i)(j).idx
										pv(fidx) = pv(fidx) + grad * feats(i)(j).value
									}
								}
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

*/