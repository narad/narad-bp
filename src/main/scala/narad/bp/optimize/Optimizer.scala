package narad.bp.optimize
import narad.bp.inference._
import narad.bp.structure._
import narad.bp.util._
import java.io.{File, FileWriter}
//import scala.collection.mutable.{ArrayBuffer, HashMap}
//import scala.util.matching._

class Optimizer(model: Model) extends BeliefPropagation with SGDUpdates {
  val debugsort  = """un\(([0-9]+),([0-9]+)\)""".r
  var RATES = Array[Double]()

  def train(data: Iterable[PotentialExample], options: OptimizerOptions): Array[Double] = {
		var params = init(options.INIT_FILE, options.PV_SIZE)
    val verbose = options.VERBOSE
    val time = options.TIME
    RATES = Array.fill[Double](params.size)(1.0)
    System.err.print("About to calculate data size: ")
    val DATA_SIZE = data.size
    System.err.println(DATA_SIZE + ".")

		for (i <- 0 until options.TRAIN_ITERATIONS) {
      var batchCount = 0
      var startTime = System.currentTimeMillis()
      for (batch <- order(data, i, options)) {
        batchCount += 1
        var batchTime = System.currentTimeMillis()
        var numPots = 0
        var numIters = 0
        val updates = new Array[ParameterUpdate](options.BATCH_SIZE)
        var batchIndex = 0
        batch.par.foreach { ex =>
          val instance = model.constructFromExample(ex, params)
          val beliefs = instance.marginals
          numPots += ex.potentials.size

          if (verbose) System.err.println("DEBUG: GRAPH")
          if (verbose) System.err.println(instance.graph)
          if (verbose) System.err.println("DEBUG: POST-EXP / BEFORE BP:")
          if (verbose) {
            //            if (beliefs(0).name.contains("un")) beliefs.sortBy{ p => val debugsort(s, e) = p.name; (5000 * s.toInt) + e.toInt }.foreach(b => System.err.println("DEBUG: BEFORE: " + b))
            //              beliefs.sortBy(_.name).foreach(b => System.err.println("DEBUG: BEFORE: " + b))
            beliefs.foreach(b => System.err.println("DEBUG: BEFORE: " + b))
          }
          val (converged, inferIters) = infer(instance, options)
          numIters += inferIters
//          updates(i % options.BATCH_SIZE) = update(instance, options)
          updates(batchIndex) = update(instance, options)
          batchIndex += 1
				}
        if (time) System.err.print("\rTRAINING: ...processing example %d/%d [Last took %fs. for %d pots. in %d BP iters]      ".format(
          batchCount*options.BATCH_SIZE, DATA_SIZE, (System.currentTimeMillis() - batchTime) / 1000.0, numPots, numIters))

        val avg = average(updates)
//        println("Size of update = " + avg.size)
  //      println(-1.0 * options.RATE)
				params = updateParams(params, avg, scale=(-1.0 * options.RATE), variance=(options.VARIANCE * DATA_SIZE))
	//			params = updateParamsWithVariableRate(params, avg, scale=(-1.0 * options.RATE), variance=(options.VARIANCE * DATA_SIZE))
        if (verbose) System.err.println("PVV")
				if (verbose) params.zipWithIndex.foreach {case(p,pi) => System.err.println(pi + "\t" + p)}
			}
      if (time) {
        val etime = (System.currentTimeMillis() - startTime) / 1000.0
        if (etime > 60) {
          System.err.println("\rTRAINING: Finished Training Iteration %d [%fm.]                                       ".format(i, etime/60))
        }
        else {
          System.err.println("\rTRAINING: Finished Training Iteration %d [%fs.]                                       ".format(i, etime))
        }
        System.err.println("     Avg time of %fs per example.".format(etime / DATA_SIZE))
      }
      writeParams(params, i, options)
		}
		params
	}
	
	def order[T](data: Iterable[T], i: Int, options: OptimizerOptions): Iterator[Iterable[T]] = {
		val bsize = if (i+1 == options.TRAIN_ITERATIONS && options.AVERAGE_LAST) data.size else options.BATCH_SIZE
		if (options.TRAIN_ORDER == "RANDOM") {
			util.Random.shuffle(data.toIterable).toIterator.grouped(bsize)
		}
		else {
			data.grouped(bsize)
		} 
	}
	
	def test(data: Iterable[PotentialExample], options: OptimizerOptions) {
		val params = init(options.INIT_FILE, options.PV_SIZE)
    for (ex <- data) {
			val instance = model.constructFromExample(ex, params)
			infer(instance, options)
			model.decode(instance)
		}
	}
		
	def updateParams(old: Array[Double], update: ParameterUpdate, scale: Double = -1.0, variance: Double = 0.0): Array[Double] = {
    val pv = old.clone()
    for (i <- update.keys) {
			pv(i) += update(i) * scale
		}
		pv
	}

  def updateParamsWithVariableRate(old: Array[Double], update: ParameterUpdate, scale: Double = -1.0, variance: Double = 0.0): Array[Double] = {
    val pv = old.clone()
    for (i <- update.keys) {
      pv(i) += update(i) * scale * RATES(i)
      RATES(i) *= 0.995
    }
    pv
  }

	def writeParams(params: Array[Double], currIter: Int, options: OptimizerOptions) {
		val modelOutputFile = options.MODEL_OUTPUT_FILE
		val i = currIter + 1
		val file = if (i == options.TRAIN_ITERATIONS) 
			modelOutputFile + ".pv" 
		else 
			modelOutputFile + "." + i + ".pv"
		val out = new FileWriter(file)
		for (p <- params.tail) out.write(p + "\n")
		out.close()
	}
	
	def init(initFile: String, pvsize: Int = 0): Array[Double] = {
		assert(initFile != null || pvsize > 0, "Both init.file and pv.size are not specified correctly.")
    if (initFile == null || !(new File(initFile)).exists) {
			Array.fill(pvsize+1)(0.0)
		}
		else {
			val params1 = io.Source.fromFile(initFile).getLines().map(_.toDouble).toArray
			val params = Array[Double](0.0) ++ params1
			if (pvsize > params.size) {
				params ++ Array.fill(pvsize - (params.size+1))(0.0)
			}
			else {
				params
			}
		}
	}

	def average(updateIter: Iterable[ParameterUpdate]): ParameterUpdate = {
    val updates = updateIter.toSeq
		var update = updates(0)
//    println(update.size)
//    System.err.println("updates size = " + updates.size)
		for (i <- 1 until updates.size if updates(i) != null) {
			update = update.add(updates(i))
		}
		if (updates.size > 1) {
      for (k <- update.keys) update(k) = update(k) / updates.size
    }
    update
	}
}
		
		
		
trait OptimizerOptions extends TrainingOptions with InferenceOptions {}

trait TrainingOptions {

  def AVERAGE_LAST: Boolean

  def BATCH_SIZE: Int

  def INIT_FILE: String

  def MODEL_OUTPUT_FILE: String

  def TRAIN_ORDER: String

  def PV_SIZE: Int

  def RATE: Double

  def TRAIN_ITERATIONS: Int

  def VARIANCE: Double

  def TIME: Boolean

}



class HiddenStructureOptimizer(model: HiddenStructureModel) extends Optimizer(model) {

  override def train(data: Iterable[PotentialExample], options: OptimizerOptions): Array[Double] = {
    var params = init(options.INIT_FILE, options.PV_SIZE)
    val verbose = options.VERBOSE
    for (i <- 0 until options.TRAIN_ITERATIONS) {
      for (batch <- order(data, i, options)) {
        if (verbose) System.err.println("Batchsize = " + batch.size)
        val updates = batch.map { ex =>

          val instance1 = model.constructFromExample(ex, params)
          val instance2 = model.constructFromExample(ex.clone(), params)

          instance2.observedVariableFactors.foreach(_.clamp())

          infer(instance1, options)
          infer(instance2, options)
          val denMargs = instance1.marginals
          val numMargs = instance2.marginals

          for (i <- 0 until denMargs.size) {
            denMargs(i).value = denMargs(i).value - numMargs(i).value
          }
           val beliefs = denMargs.sortBy(_.name)

          val rate = options.RATE
          val pv = new ParameterUpdate
          val feats = ex.features

          val margs = beliefs.map(_.value)//.map( b => if (b.isCorrect) b.value - 1.0 else b.value)
          val fnames = beliefs.collect{case p if feats.contains(p.name) => feats(p.name)}

          if (verbose) System.err.println("DEBUG: POST-EXP / BEFORE BP:")
          if (verbose) beliefs.foreach(b => System.err.println("DEBUG: BEFORE: " + b))

       //   for (i <- 0 until margs.size) { System.err.println("DEBUG: post-bp marg[ " + beliefs(i).name + " ] =  " +  margs(i))}
          val updates = margs
          for (i <- 0 until updates.size if updates(i) != 0.0) {
            val grad = updates(i) //* rate
            for (j <- 0 until fnames(i).size) {
              val fidx = fnames(i)(j).idx
              pv(fidx) = pv.getOrElse(fidx, 0.0) + grad * fnames(i)(j).value
            }
          }
          pv
        }
        val avg = average(updates)
        params = updateParams(params, avg, scale=(-1.0 * options.RATE), variance=(options.VARIANCE * data.size))
        if (verbose) System.err.println("PVV")
        if (verbose) params.zipWithIndex.foreach {case(p,pi) => System.err.println(pi + "\t" + p)}
      }
      writeParams(params, i, options)
    }
    params
  }
}







class UpgradeableOptimizer(model: Model) extends Optimizer(model) {

}














































/*

class TwoStepOptimizer(model: Model) extends Optimizer(model) {

  override def train(data: Iterable[PotentialExample], options: OptimizerOptions): Array[Double] = {
    var params = init(options.INIT_FILE, options.PV_SIZE)
    val verbose = options.VERBOSE
    for (i <- 0 until options.TRAIN_ITERATIONS) {
      for (batch <- order(data, i, options)) {
        if (verbose) System.err.println("Batchsize = " + batch.size)
        val updates = batch.map { ex =>

          val instance1 = model.constructFromExample(ex, params)
          val instance2 = model.constructFromExample(ex.clone(), params)
          for (f <- instance2.graph.factors) {
            if (f.name.contains("arg")) {
              f.clamp()
            }
            if (f.name.startsWith("sense") || f.name.startsWith("label")) {
              if (f.isCorrect) f.peg()
            }
          }

          infer(instance1, options)
          infer(instance2, options)
          val denMargs = instance1.marginals
          val numMargs = instance2.marginals


          for (i <- 0 until denMargs.size) {
            denMargs(i).value = denMargs(i).value - numMargs(i).value
          }
          val beliefs = denMargs.sortBy(_.name)

          val rate = options.RATE
          val pv = new ParameterUpdate
          val feats = ex.features

          val margs = beliefs.map(_.value)//.map( b => if (b.isCorrect) b.value - 1.0 else b.value)
          val fnames = beliefs.collect{case p if feats.contains(p.name) => feats(p.name)}

          if (verbose) System.err.println("DEBUG: POST-EXP / BEFORE BP:")
          if (verbose) beliefs.foreach(b => System.err.println("DEBUG: BEFORE: " + b))

          //   for (i <- 0 until margs.size) { System.err.println("DEBUG: post-bp marg[ " + beliefs(i).name + " ] =  " +  margs(i))}
          val updates = margs
          for (i <- 0 until updates.size if updates(i) != 0.0) {
            val grad = updates(i) //* rate
            for (j <- 0 until fnames(i).size) {
              val fidx = fnames(i)(j).idx
              pv(fidx) = pv.getOrElse(fidx, 0.0) + grad * fnames(i)(j).value
            }
          }
          pv
        }
        val avg = average(updates)
        params = updateParams(params, avg, scale=(-1.0 * options.RATE), variance=(options.VARIANCE * data.size))
        if (verbose) System.err.println("PVV")
        if (verbose) params.zipWithIndex.foreach {case(p,pi) => System.err.println(pi + "\t" + p)}
      }
      writeParams(params, i, options)
    }
    params
  }
}
*/










/*

class TwoStepOptimizer(model: Model) extends Optimizer(model) {

  override def train(data: Iterable[PotentialExample], options: OptimizerOptions): Array[Double] = {
    var params = init(options.INIT_FILE, options.PV_SIZE)
    val verbose = options.VERBOSE
    for (i <- 0 until options.TRAIN_ITERATIONS) {
      for (batch <- order(data, i, options)) {
        if (verbose) System.err.println("Batchsize = " + batch.size)
        val updates = batch.map { ex =>
 //         if (verbose) System.err.println("DEBUG: GRAPH")
 //         if (verbose) System.err.println(instance.graph)


          val instance1 = model.constructFromExample(ex, params)
          val instance2 = model.constructFromExample(ex.clone(), params)
          for (f <- instance2.graph.factors) {
            if (f.name.contains("arg")) {
              f.clamp()
            }
            if (f.name.startsWith("sense") || f.name.startsWith("label")) {
              if (f.isCorrect) f.peg()
            }
          }


          /*
          val hpots = ex.getPotentials
          for (pi <- 0 until hpots.size) {
            if (hpots(pi).isCorrect) {
              hpots(pi) = Potential(10000.0, hpots(pi).name, hpots(pi).label)
            }
          }
          val instance2 = model.constructFromExample(new PotentialExample(ex.getAttributes, hpots, ex.getFeatures), params)


          for (f <- instance2.graph.factors) {
            if (f.arity == 1) {
              f1 = f.asInstanceOf[UnaryFactor]
              if (f1.isCorrect) {
                f1.peg
              }
              else if (f1.name.contains("arg")) {
                f1.neg
              }
            }
          }
          */

          infer(instance1, options)
          infer(instance2, options)
          val denMargs = instance1.marginals
          val numMargs = instance2.marginals

          /*
          System.err.println("MIXED MARGS:")
          for (i <- 0 until denMargs.size) {
            System.err.println(denMargs(i) + "\t\t\t" + numMargs(i))
          }
          println()
            */

          for (i <- 0 until denMargs.size) {
            denMargs(i).value = denMargs(i).value - numMargs(i).value
          }

         // System.err.println("FINAL MARGS:")
         // for (i <- 0 until denMargs.size) {
         //   System.err.println(denMargs(i))
         // }
         // println()



          val beliefs = denMargs.sortBy(_.name)

          val rate = options.RATE
          val pv = new ParameterUpdate
          val feats = ex.features

          val margs = beliefs.map(_.value)//.map( b => if (b.isCorrect) b.value - 1.0 else b.value)
          val fnames = beliefs.collect{case p if feats.contains(p.name) => feats(p.name)}

          if (verbose) System.err.println("DEBUG: POST-EXP / BEFORE BP:")
          if (verbose) beliefs.foreach(b => System.err.println("DEBUG: BEFORE: " + b))

       //   for (i <- 0 until margs.size) { System.err.println("DEBUG: post-bp marg[ " + beliefs(i).name + " ] =  " +  margs(i))}
          val updates = margs
          for (i <- 0 until updates.size if updates(i) != 0.0) {
            val grad = updates(i) //* rate
            for (j <- 0 until fnames(i).size) {
              val fidx = fnames(i)(j).idx
              pv(fidx) = pv.getOrElse(fidx, 0.0) + grad * fnames(i)(j).value
            }
          }
          pv
        }
        val avg = average(updates)
        params = updateParams(params, avg, scale=(-1.0 * options.RATE), variance=(options.VARIANCE * data.size))
        if (verbose) System.err.println("PVV")
        if (verbose) params.zipWithIndex.foreach {case(p,pi) => System.err.println(pi + "\t" + p)}
      }
      writeParams(params, i, options)
    }
    params
  }
}


 */


/*
  def average(arrays: Seq[Array[Double]]): Array[Double] = {
    for (i <- 1 until arrays.size) {
      for (j <- 0 until arrays(i).size) {
        arrays(0)(j) += arrays(i)(j)
      }
    }
    arrays(0).map(_ / arrays.size)
  }
*/
		
		/*
		
		
						
		for (batch <- )
				
				
					val nrExamples = if (maxEx == -1) countExamples(fidxFile) else maxEx
					for (i <- 0 until iterations) {
						val nparams = SGDTrainer.train(params, fidxFile, constructor, maxExamples = nrExamples, bpIters = 10, verbose = verbose)
						if (i+1 == iterations) {
							writeToFile(nparams.tail, outFile + ".pv")
						}
						else{
							writeToFile(nparams.tail, outFile + "." + (i+1) + ".pv")				
						}
						params = nparams
					}
				}


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
					val model = constructor(ex, pots)
					
					if (verbose) println("\nInitial Params:\n%s".format(params.zipWithIndex.mkString("\n")))
					if (verbose) println("\nGraph:\n%s".format(model.graph.toString))
					println("\nGraph:\n%s".format(model.graph.toString))
					if (verbose) println("\nPre-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
					System.err.println("Construction time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		

					startTime = System.currentTimeMillis()
					val stats = runBP(model, bpIters, drate=rate, dinit=1, verbose = verbose)
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
			
}

object Optimizer {
	
//		def truncate(x: Double) = math.round(x*1000)*0.001
	
//		final def trunc(x: Double, n: Int) = if (n<=0) math.round(x) else trunc(x*10,n-1)*0.1
	
		def truncate(x: Double) = "%.9f".format(x).toDouble
	
		def train(params: Array[Double], fidxFile: String, constructor: (PotentialExample, Array[Potential]) => FactorGraphModel, maxExamples: Int = Int.MaxValue, 
							rate: Double = -0.01, variance: Double = 1.0, bpIters: Int = 5, verbose: Boolean = false): Array[Double] = {
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
				val model = constructor(ex, pots)
				if (verbose) println("\nInitial Params:\n%s".format(params.zipWithIndex.mkString("\n")))
				if (verbose) println("\nGraph:\n%s".format(model.graph.toString))
				println("\nGraph:\n%s".format(model.graph.toString))
				if (verbose) println("\nPre-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
				System.err.println("Construction time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		

				startTime = System.currentTimeMillis()
				val stats = runBP(model, bpIters, drate=rate, dinit=1, verbose = verbose)
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


		def test(params: Array[Double], constructor: (PotentialExample, Array[Potential]) => FactorGraphModel, ex: PotentialExample, 
		         bpIters: Int = 5, verbose: Boolean = false): Model = {
			val rate = 1.0
			val variance = 0.0
			var count = 0
			var pvv = params.clone
			val damp = 0.099
			val feats   = ex.getFeatures				
			val pots    = ex.getPotentials
			if (verbose) println("\nInitial Params:\n%s".format(params.zipWithIndex.mkString("\n")))
			
			System.err.println("pots size = " + pots.size)
			System.err.println(params(4))
			System.err.println("%f".format(params(4)))
//			pots.foreach { pot => pot.value = feats(pot.name).foldLeft(0.0)((sum, feat) => sum + params(feat.idx) * feat.value) }
			pots.foreach { pot => 
				var sum = 0.0
				feats(pot.name).foreach { f =>
					var tmp = params(f.idx) * f.value
					System.err.println("adding " + params(f.idx) * f.value + 
					 									 " (from feat idx " + f.idx + ") to pot " + pot.name + 
														 ", as " + params(f.idx) + " x " + f.value + ", yielding " + (sum+tmp))
					
					sum += tmp
				}
				pot.value = sum
			}
//			"%d, param) to pot %s, as %f x %f, yielding %f.".format(params(f.idx) * f.value, f.idx, pot.name, params(f.idx), f.value, sum+tmp))				
//			pot.value = feats(pot.name).foldLeft(0.0)((sum, feat) => sum + params(feat.idx) * feat.value) }
			if (verbose) println("\nPre-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
			pots.foreach { pot => pot.value = Math.exp(pot.value) }
			if (verbose) println("\nPre-Exp-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
			

			val model = constructor(ex, pots)
			if (verbose) println("\nGraph:\n%s".format(model.graph.toString))
			if (verbose) {
				println("Post-Construction")
				val beliefs = model.graph.potentialBeliefs
				for (i <- 0 until beliefs.size) {
					System.err.println(i + ": " + beliefs(i))
				}
			}
			runBP(model, bpIters, dinit = 1.0, verbose = verbose)
			if (verbose) {
				println("Post-BP")
				val beliefs = model.graph.potentialBeliefs
				for (i <- 0 until beliefs.size) {
					System.err.println(i + ": " + beliefs(i))
				}
			}
			model
		}	

		def runBP(model: FactorGraphModel, bpiters: Int = 5, drate: Double = 0.99, dinit: Double = -0.03, threshold: Double = .001, verbose: Boolean = false): (Boolean, Int) = {
			val graph = model.graph
			var damp = dinit
			for (i <- 0 until bpiters) {
				var maxDiff = -1.0
				var seenVar = false
				var startTime = System.currentTimeMillis()
				for (n <- model.messageOrder(graph)) {
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
				

/*
	def computePotentials(params: Array[Double], feats: Array[Array[Feature]]): Array[Double] = {
		feats.map(x => x.foldLeft(0.0)((sum, feat) => sum + params(feat.idx) * feat.value))
	}
*/
/*
		def runBP(graph: FactorGraph, bpiters: Int = 5, drate: Double = 0.99, dinit: Double = -0.03, threshold: Double = .001, verbose: Boolean = false): (Boolean, Int) = {
			val model = graph
//			println("damp rate = " + drate)
//			println("damp init = " + dinit)
//			println("threshold = " + threshold)
//			var maxDiff = -1.0
			var damp = dinit
			val mqueue = scala.collection.mutable.Queue[MessageNode]() 
			val uqueue = scala.collection.mutable.Queue[MessageNode]() 

			var startTime = System.currentTimeMillis()
			for (fac <- model.factors) {
				if (model.edgesFrom(fac).size == 1)
					uqueue += fac
				else
					mqueue += fac
			}

			for (v <- model.variables) {
				mqueue += v
			}
			System.err.println("Time spent queuing: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		

			// Do the unary facs first with no damping
			startTime = System.currentTimeMillis()
			for (u <- uqueue) {
				val diff = u.computeMessages(model, damp=1, verbose)
				if (verbose) println("QUEUE: " + u.name + " = " + diff)
			}
			System.err.println("Time for unary factor propagation: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
			
			// Do the others with successive damping
			for (i <- 0 until bpiters) {
				if (verbose) println("QUEUE Performing BP iteration %d".format(i))
				var maxDiff = -1.0
				var seenVar = false
				startTime = System.currentTimeMillis()
				for (v <- mqueue) { //.sortBy(graph.edgesFrom(_).size
					if (v.isVariable && !seenVar) {
						System.err.println("Time for other factor propagation: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")
						startTime = System.currentTimeMillis()						
						seenVar = true
					}
					var diff = v.computeMessages(model, damp=damp, verbose) //damp=damp)
					if (verbose) System.err.println("QUEUE " + v.name + " = " + diff)
					if (diff > maxDiff) maxDiff = diff
				}
				System.err.println("Time for variable propagation: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")
				startTime = System.currentTimeMillis()

				if (i > 0 && maxDiff < threshold) {
					return (true, i)
				}
				if (i > 1) damp *= drate
			}
			return (false, bpiters)			
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
*/
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
				
				
		//					println(i + " w/ size " + feats(i).size)
		//						if (feat.idx == 44) {
		//							println("[" + feat.idx + "] += " + (grad * feat.value) + " = " + newpv(feat.idx))
		//						}


		def hiddenMarginalization(pots: Array[Potential], model: FactorGraphModel, bpIters: Int = 10, damp: Double = 0.99): Array[Potential] = {
			val conv = runBP(model, bpIters, damp)
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
			val convNum  = runBP(model, bpIters, damp)
			val num = model.graph.potentialBeliefs
			for (i <- 0 until den.size) den(i).value -= num(i).value
			return den //den.zipWithIndex.map{case(v, idx) => v.value - num(idx).value}
		}
	
/*		
		def nerMarginalization(pots: Array[Potential], model: Model, bpIters: Int = 40, damp: Double = 0.99): Array[Potential] = {
			val conv = runBP(model.graph, bpIters, damp)
			val den  = model.graph.potentialBeliefs
			
//			assert(!den.exists(_.isNaN))

//			val num = pots.clone
//			num.foreach(p => if (p.isCorrect || p.name.startsWith("nerlabel")) p.value = Double.PositiveInfinity)
		}
	*/	
/*		
		ner.marg <- function(pots, bp.iterations=40, model.fun=semi.ner.model,
		                     damp.rate=0.99, ...) {
		  model.fun <- match.fun(model.fun)
		  ## cat("# slen:", attr(pots, "slen"), "; tlen:", attr(pots, "tlen"), "\n")
		  p <- model.fun(pots, ...)
		  conv <- run.bp(p, bp.iterations, damp.rate=damp.rate)
		  den <- potential.beliefs(p)
			

			val 
		  pots.num <- pots
		  pots.num[as.logical(attr(pots.num, "correct")) & grepl("^nerlabel", names(pots.num))] <- Inf
		  p.num <- model.fun(pots.num, ...)
		  conv.num <- run.bp(p.num, bp.iterations, damp.rate=damp.rate)
		  num <- potential.beliefs(p.num)

		  if ( any(is.na(num)) || any(num < 0) ) {
		    cat("# bad num: ", summary(num), "\n")
		    return(numeric(length(num)))
		  }
		  ## cat("# den:", conv, "; num:", conv.num, "\n")
		  structure(den - num, convergence=conv)
		}
*/		
		
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










