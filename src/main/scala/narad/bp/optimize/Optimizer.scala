package narad.bp.optimize
import narad.bp.inference._
import narad.bp.structure._
import narad.bp.util._
import java.io.{File, FileWriter}

class Optimizer(model: Model[Scorable], val options: OptimizerOptions) extends BeliefPropagation with SGDUpdates {
  private val VERBOSE = options.VERBOSE
  private val TIME = options.TIME

  def train(data: Iterable[PotentialExample]): Array[Double] = {
    System.err.println("Beginning Training...")
    System.err.print("About to calculate data size: ")
    val DATA_SIZE = data.size
    System.err.println(DATA_SIZE + ".")

    val variance = options.VARIANCE * DATA_SIZE
    val decay = 1.0 - options.DECAY
    var scale = -1.0 * options.RATE
    val partial = (-scale / variance)
    System.err.println("Partial = " + partial)
    val reg = 1.0 - partial
//    val reg = 0.95
    //   var scale = - 0.1
    //   val decay = 0.99

    System.err.println("Rate Param = " + options.RATE)
    System.err.println("Variance Param = " + options.VARIANCE)
    System.err.println("Partial = %1.2f".format(partial))
    System.err.println("Variance = %1.2f".format(variance))
    System.err.println("Decay Factor = %1.2f".format(decay))
    System.err.println("Reg = %f".format(reg))


    var params = init(data, options.INIT_FILE, options.PV_SIZE)
    val lastUpdated = new Array[Int](params.size)
    val time = options.TIME
    val timer = new Timer()
    var t = 1
    for (i <- options.TRAIN_ITERATIONS_OFFSET until options.TRAIN_ITERATIONS) {
      timer.start() //var startTime = System.currentTimeMillis()
      System.err.println("Beginning training with batch size of %d".format(options.BATCH_SIZE))
      for (batch <- order(data, i, options)) {
        val updates = new Array[ParameterUpdate](batch.size)
        println("marg? " + options.MARGINALIZATION)
        batch.zipWithIndex.par.foreach { case(ex, bi) =>
          updates(bi) = if (options.MARGINALIZATION) {
            clampedUpdate(ex, params, VERBOSE)
          }
          else {
            standardUpdate(ex, params, VERBOSE)
          }
        }
        for (update <- updates if update != null) {
          for (idx <- update.keys) {
            params(idx) = params(idx) * math.pow(reg, t - lastUpdated(idx))
            params(idx) += update(idx) * scale
            lastUpdated(idx) = t
          }
          t += 1
        }
      }
      scale *= decay
      if (time) {
        //val etime = (System.currentTimeMillis() - startTime) / 1000.0
        timer.stop()  //val timeString = if (etime > 60) "%fm".format(etime/60) else "%fs".format(etime)
        System.err.println("\rTRAINING: Finished Training Iteration %d [%s.]                                         ".format(i, time.toString))
        System.err.println("     Avg time of %fs per example.".format(timer.elapsedTime() / DATA_SIZE))
      }
      if (options.PRINT_TRAIN_ACCURACY) {
        println("Train Accuracy at Iteration %d:".format(i))
        println(evaluate(data, i, params))
      }
      if (options.PRINT_DEV_ACCURACY) {
        println("Dev Accuracy at Iteration %d:".format(i))
        println(evaluate(new PotentialReader(options.DEV_DATA_FILE), i, params))
      }
      writeParams(params, options.MODEL_OUTPUT_FILE, i, options.TRAIN_ITERATIONS)
    }
    params
  }


  def standardUpdate(ex: PotentialExample, params: Array[Double], VERBOSE: Boolean = false): ParameterUpdate = {
    if (VERBOSE) System.err.println("standard update")
    var startTime = System.currentTimeMillis()
    //    System.err.println("VERBOSE? " + VERBOSE)
    val instance = model.constructFromExample(ex, params)
    //    instance.graph.factors.foreach { f => if (f.name.startsWith("srlLabel")) f.peg() }      /// DELETE
    if (VERBOSE) {
      println("Variable Beliefs Before BP:")
      for (b <- instance.graph.variableBeliefs) {
        println("DEBUG: pre-bp var belief: " + b)
      }
      System.err.println("\nFactor Beliefs Before BP:")
      for (m <- instance.marginals) {
        System.err.println("DEBUG: pre-bp marg[ " + m.name + " ] =  " +  m)
      }
    }
    val (converged, inferIters) = infer(instance, options)
    assert (!instance.marginals.exists(_.value.isNaN), { println("NaN found in Marginals."); dumpState(ex, params)})

    val u = update(instance, options)
    if (TIME) {
      val etime = (System.currentTimeMillis() - startTime) / 1000.0
      if (etime > 60) {
        System.err.println("\rTRAINING: Finished %d Inference Iterations [%fm.]%s".format(inferIters, etime/60, "                                         "))
      }
      else {
        System.err.println("\rTRAINING: Finished %d Inference Iterations [%fs.]%s".format(inferIters, etime, "                                         "))
      }
    }
    if (VERBOSE) {
      println("Variable Beliefs After BP:")
      for (b <- instance.graph.variableBeliefs) {
        println("DEBUG: post-bp var belief: " + b)
      }
      System.err.println("\nFactor Beliefs After BP:")
      for (m <- instance.marginals) {
        System.err.println("DEBUG: post-bp marg[ " + m.name + " ] =  " +  m)
      }
    }
    u
  }

  def clampedUpdate(ex: PotentialExample, params: Array[Double], VERBOSE: Boolean = false): ParameterUpdate = {
    System.err.println("Clamped Update.")
    var startTime = System.currentTimeMillis()
    val denInstance = model.constructFromExample(ex.clone(), params)
    val numInstance = model.constructFromExample(ex.clone(), params)
    numInstance.clampedFactors.foreach{ f =>
      println("...clamping %s".format(f.name))
      f.clamp()
    }
    if (VERBOSE) {
      System.err.println("DEBUG: Clamped Pre-BP MARGINALS:")
      val hmargs = numInstance.marginals
      for (i <- 0 until hmargs.size) {
        System.err.println("DEBUG: pre-bp marg[ " + hmargs(i).name + " ] =  " +  hmargs(i))
      }
    }
    val (converged1, inferIters1) = infer(denInstance, options)
    assert (!denInstance.marginals.exists(_.value.isNaN), { println("NaN found in Unclamped Marginals."); dumpState(ex, params)})

    val (converged2, inferIters2) = infer(numInstance, options)
    assert (!numInstance.marginals.exists(_.value.isNaN), { println("NaN found in Clamped Marginals."); dumpState(ex, params)})

    if (TIME) {
      val etime = (System.currentTimeMillis() - startTime) / 1000.0
      if (etime > 60) {
        System.err.println("\rTRAINING: Finished %d Inference Iterations (%d clamped) [%fm.]%s".format(inferIters1, inferIters2, etime/60, "                                         "))
      }
      else {
        System.err.println("\rTRAINING: Finished %d Inference Iterations (%d clamped) [%fs.]%s".format(inferIters1, inferIters2, etime, "                                         "))
      }
    }
    if (VERBOSE) {
      val di = denInstance.marginals.toArray
      val ni = numInstance.marginals.toArray
      System.err.println("\nDEBUG: Factor Beliefs After BP:")
      for (i <- 0 until di.size) {
        System.err.println("DEBUG: post-bp marg[%s] = ".format(di(i).name) + "\t" + di(i).value + "\t" + ni(i).value)
      }
    }
    assert (!numInstance.marginals.exists(_.value.isNaN), { println("NaN found in Numerator Marginals."); dumpState(ex, params)})
    assert (!denInstance.marginals.exists(_.value.isNaN), { println("NaN found in Denominator Marginals."); dumpState(ex, params)})
    hiddenUpdate(denInstance, numInstance, options)
  }


  def test(data: Iterable[PotentialExample]) {
    val initFile = options.INIT_FILE
    assert(initFile != null && new File(initFile).exists(), "Model file for testing is not specified or does not exist.")
    val params = init(data, initFile, options.PV_SIZE)
    for (ex <- data) {
      val instance = model.constructFromExample(ex, params)
      infer(instance, options)
      model.decode(instance)
    }
  }

  def evaluate(data: Iterable[PotentialExample], i: Int, params: Array[Double]): EvalContainer = {
    val output = data.map{ex =>
      val instance = model.constructFromExample(ex, params)
      infer(instance, options)
      (model.decode(instance), model.fromPotentialExample(ex, params))
    }
    val out = new FileWriter("output.%d".format(i+1))
    out.write(output.map(_._1.toString).mkString("\n\n"))
    out.close()
    val scores = output.map{case(test,gold) => test.score(gold)}
    scores.tail.foldLeft(scores.head)(_.combine(_))
  }

  def evaluateExample(params: Array[Double], ex: PotentialExample): (Int, Int) = {
    val instance = model.constructFromExample(ex, params)
    infer(instance, options)
    var correct = 0
    var incorrect = 0
    for (b <- instance.marginals) {
      if ((b.value > 0.5 && b.isCorrect) || (b.value <= 0.5 && !b.isCorrect)) {
        correct += 1
      }
      else {
        incorrect += 1
      }
    }
    (correct, incorrect)
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

  def updateParams(pv: Array[Double], update: ParameterUpdate, scale: Double = -1.0, variance: Double = 0.0): Array[Double] = {
    for (i <- update.keys) {
      pv(i) += update(i) * scale
    }
    pv
  }

  def writeParams(params: Array[Double], modelOutputFile: String, currIter: Int, maxIter: Int) { // options: OptimizerOptions) {
  //		val modelOutputFile = options.MODEL_OUTPUT_FILE
  val i = currIter + 1
    val file = if (i == maxIter) //options.TRAIN_ITERATIONS)
      modelOutputFile + ".pv"
    else
      modelOutputFile + "." + i + ".pv"
    val out = new FileWriter(file)
    for (p <- params.view.tail) out.write(p + "\n")
    out.close()
  }

  def init(data: Iterable[PotentialExample], initFile: String, pvsize: Int = 0): Array[Double] = {
    if (initFile != null && new File(initFile).exists()) {
      System.err.println("Initializing parameter vector from previous file.")
      val s = io.Source.fromFile(initFile).getLines().size
      println("init file size = " + s)
      val params = new Array[Double](s + 1)
      io.Source.fromFile(initFile).getLines().zipWithIndex.foreach { case(p,i) =>
        params(i+1) = p.toDouble
      }
      params
    }
    else if (pvsize > 0) {
      System.err.println("Initializing parameter vector from pv.size parameter: %d.".format(pvsize))
      val params = Array.fill(pvsize+2)(0.0)
      if (options.PV_SET > 0.0) {
        for (i <- options.PV_SET_RANGE._1 to options.PV_SET_RANGE._2) {
          params(i) = options.PV_SET
        }
      }
      params
    }
    else {
      System.err.println("Initializing parameter vector from data.")
      var max = 0
      for (pe <- data) {
        for (pots <- pe.getFeatures) {
          for (f <- pots._2) {
            if (f.idx > max) max = f.idx
          }
        }
      }
      Array.fill(max+2)(0.0)
    }
  }


  def average(updateIter: Iterable[ParameterUpdate]): ParameterUpdate = {
    val updates = updateIter.toSeq
    var update = updates(0)
    for (i <- 1 until updates.size if updates(i) != null) {
      println("is update null? " + (update == null).toString)
      println("is updates(" + i + ") null?" + (updates(i) == null).toString)
      update = update.add(updates(i))
    }
    if (updates.size > 1) {
      for (k <- update.keys) update(k) = update(k) / updates.size
    }
    update
  }

  def dumpState(ex: PotentialExample, params: Array[Double]) {
    writeParams(params, "model.last", 0, 0)
    val out = new FileWriter("ex.last")
    ex.writeToFile(out)
    out.write("\n")
    out.close

    val pout = new FileWriter("last.init")
    val instance = model.constructFromExample(ex, params)
    for (f <- instance.graph.factors) {
      f match {
        case x : UnaryFactor => {
          pout.write(f.name + "\n" + x.pots.mkString("\n") + "\n")
        }
        case _ => {}
      }
    }
    pout.close()
  }
}











































































/*
//    val scores = data.map {ex => model.decode(model.constructFromExample(ex, params)).score(model.fromPotentialExample(ex, params))}

    score

    val first = data.head
    val instance = model.constructFromExample(first, params)
    infer(instance, options)
    val ec = model.decode(instance).score(model.fromPotentialExample(first, params))

    val eval = data.view.tail.foldLeft(ec){ case(eval, ex) =>
      val instance = model.constructFromExample(ex, params)
      infer(instance, options)
      val ec2 = model.decode(instance).score(model.fromPotentialExample(ex, params))
      ec2.combine(eval)
    }
    eval
  }
*/



/*
      options.CONCURRENCY match {
        case "SERIAL" => {
          System.err.println("Beginning training in serial.")
          for (ex <- data) {
            val update =  options.MARGINALIZATION match {
              case "STANDARD" => standardUpdate(ex, params, VERBOSE)
              case "HIDDEN" => clampedUpdate(ex, params, VERBOSE)
            }
            for (idx <- update.keys) {
              params(idx) = (params(idx) * math.pow(reg, t - lastUpdated(idx))) + (update(idx) * scale)
              lastUpdated(idx) = t
            }
            t += 1
          }
        }
        case "BATCH" => {
          System.err.println("Beginning training with batch size of %d".format(options.BATCH_SIZE))
          for (batch <- order(data, i, options)) {
            val updates = new Array[ParameterUpdate](batch.size)
            batch.zipWithIndex.par.foreach { case(ex, bi) =>
              updates(bi) =  options.MARGINALIZATION match {
                case "STANDARD" => standardUpdate(ex, params, VERBOSE)
                case "HIDDEN" => clampedUpdate(ex, params, VERBOSE)
              }
            }
            for (update <- updates if update != null) {
              for (idx <- update.keys) {
                params(idx) = params(idx) * math.pow(reg, t - lastUpdated(idx))
                params(idx) += update(idx) * scale
                lastUpdated(idx) = t
              }
              t += 1
            }
          }
        }
      }
 */





//                val lastReg = lastUpdated(idx)
//   lastUpdated(idx) = t
//   params(idx) = (params(idx) * math.pow(reg, t - lastReg)) + (update(idx) * scale)
//                params(idx) = params(idx) * math.pow(reg, t - lastReg) // * reg
//                params(idx) += update(idx) * scale
//                params(idx) = (params(idx) * math.pow(reg, t - lastUpdated(idx))) // + (update(idx) * scale)
//                lastUpdated(idx) = t


/*
        case "HOGWILD" => {
          System.err.println("Beginning training using HOGWILD! concurrency.")
          val pdata = data.par
          pdata.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(options.NUM_CORES))
          pdata.foreach { ex =>
            val update =  options.MARGINALIZATION match {
              case "STANDARD" => standardUpdate(ex, params, VERBOSE)
              case "HIDDEN" => clampedUpdate(ex, params, VERBOSE)
            }
            for (idx <- update.keys) {
              params(idx) = (params(idx) * math.pow(reg, t - lastUpdated(idx))) + (update(idx) * scale)
              lastUpdated(idx) = t
            }
            t += 1
          }
        }
 */


/*
     val params1 = io.Source.fromFile(initFile).getLines().view.map(_.toDouble).toArray
val params = Array[Double](0.0) ++ params1
if (pvsize > params.size) {
params ++ Array.fill(pvsize - (params.size+2))(0.0)
}
else {
params
}

*/






// var groups = Array[Int]()


/*
 // A very computationally heavy setup
def calculateGroups(data: Iterable[PotentialExample], size: Int): Array[Int] = {
val g = new Array[Int](size)
for (p <- data; k <- p.features.keys; f <- p.features(k)) {
if (f.group > g(f.idx)) g(f.idx) = f.group
}
g
}

def regularizationGroups : Array[Int] = {
groups
}
*/






//          val avg = average(updates)
//          params = updateParams(params, avg, scale=(-1.0 * options.RATE), variance=(options.VARIANCE * data.size))
//				}

/*
          if (VERBOSE) System.err.println("DEBUG: GRAPH")
          if (VERBOSE) System.err.println(instance.graph)
          if (VERBOSE) System.err.println("DEBUG: POST-EXP / BEFORE BP:")
          if (VERBOSE) {
            beliefs.foreach(b => System.err.println("DEBUG: BEFORE: " + b))
          }
*/


/*
def train(data: Iterable[PotentialExample]): Array[Double] = {
  var params = init(data, options.INIT_FILE, options.PV_SIZE)
  val lastUpdated = new Array[Int](params.size)
  val VERBOSE = options.VERBOSE
  val time = options.TIME
  System.err.print("About to calculate data size: ")
  val readStart = System.currentTimeMillis()
  val DATA_SIZE = data.size
  System.err.println(DATA_SIZE + ".")
  //    println(" time to calculate data = " + (System.currentTimeMillis() - readStart) / 1000.0)
//    println("PV vec size = " + params.size)
  System.err.println("Beginning training with batch size of %d".format(options.BATCH_SIZE))
  var t = 1
  for (i <- 0 until options.TRAIN_ITERATIONS) {
    var batchCount = 0
    var startTime = System.currentTimeMillis()
    for (batch <- order(data, i, options)) {
      batchCount += 1
      var batchTime = System.currentTimeMillis()
      var numPots = 0
      var numIters = 0
      val updates = new Array[ParameterUpdate](batch.size)
      batch.zipWithIndex.par.foreach { case(ex, bi) =>
        numPots += ex.potentials.size
        val denInstance = model.constructFromExample(ex, params)
        val margs = denInstance.marginals
        denInstance.graph.factors.foreach { f => if (f.name.startsWith("arg")) f.peg() }
        if (VERBOSE) {
          System.err.println("DEBUG: Post-BP MARGINALS:")
          for (i <- 0 until margs.size) {
            System.err.println("DEBUG: pre-bp marg[ " + margs(i).name + " ] =  " +  margs(i))
          }
        }
        if (!model.usesClampedTraining) {
          if (VERBOSE) {
            println("Variable Beliefs Before:")
            for (b <- denInstance.graph.variableBeliefs) {
              println("  " + b)
            }
          }
          val (converged, inferIters) = infer(denInstance, options)
          numIters += inferIters
          updates(bi) = update(denInstance, options)
          if (VERBOSE) {
            println("Variable Beliefs After:")
            for (b <- denInstance.graph.variableBeliefs) {
              println("  " + b)
            }
          }
        }
        else {
          println("Clamped Update.")
          val numInstance = model.constructFromExample(ex.clone(), params)
          numInstance.clampedFactors.foreach{ f =>
            if (VERBOSE) println("...clamping %s".format(f.name))
            f.clamp()
          }
          if (VERBOSE) {
            System.err.println("DEBUG: Clamped Post-BP MARGINALS:")
            val hmargs = numInstance.marginals
            for (i <- 0 until hmargs.size) {
              System.err.println("DEBUG: pre-bp marg[ " + hmargs(i).name + " ] =  " +  hmargs(i))
            }
          }
          val (converged1, inferIters1) = infer(denInstance, options)
          val (converged2, inferIters2) = infer(numInstance, options)
          if (numInstance.marginals.exists(_.value.isNaN)) {
            writeParams(params, "model.last", 0, 0)
            val out = new FileWriter("ex.last")
            ex.writeToFile(out)
            out.write("\n")
            out.close
          }
          numIters += inferIters1
          updates(bi) = hiddenUpdate(denInstance, numInstance, options)
        }
      }
      if (time) {
        System.err.print("\rTRAINING: ...processing example %d/%d [Last took %fs. for %d pots. in %d BP iters per batch]      ".format(
          batchCount*options.BATCH_SIZE, DATA_SIZE, (System.currentTimeMillis() - batchTime) / 1000.0, numPots, numIters))
      }
      if (options.AVERAGE_BATCH) {
        val avg = average(updates)
        assert(false, "batch averaging not implemented at the moment - crashes in parallel.")
      }
      else {
        for (update <- updates) {
          val variance = options.VARIANCE * DATA_SIZE
          val scale = -1.0 * options.RATE
          val reg = 1 - (-scale / variance)
          for (idx <- update.keys) {
            val oval = params(idx)
            params(idx) = params(idx) * math.pow(reg, t - lastUpdated(idx)) // * reg
            val postreg = params(idx)
            params(idx) += update(idx) * scale
            lastUpdated(idx) = t
            if (options.CHECK_FOR_NAN && params(idx).isNaN) {
              System.err.println("NaN Found:")
              System.err.println("Updating param ID=" + idx)
              System.err.println("Original val = " + oval)
              System.err.println("Post reg = " + postreg)
              System.err.println("Update = " + update(idx))
              System.err.println("Scale = " + scale +"; Reg = " + reg)
              System.err.println("Last Updated " + lastUpdated(idx) + " updates ago.")
            }
          }
          t += 1
        }
      }
      if (options.CHECK_FOR_NAN) assert(!params.exists(_.isNaN), "NaN discovered at end of iteration %d.".format(i))
      //        if (VERBOSE) System.err.println("PVV")
      //				if (VERBOSE) params.zipWithIndex.foreach {case(p,pi) => System.err.println(pi + "\t" + p)}
    }
    if (time) {
      val etime = (System.currentTimeMillis() - startTime) / 1000.0
      if (etime > 60) {
        System.err.println("\rTRAINING: Finished Training Iteration %d [%fm.]                                         ".format(i, etime/60))
      }
      else {
        System.err.println("\rTRAINING: Finished Training Iteration %d [%fs.]                                         ".format(i, etime))
      }
      System.err.println("     Avg time of %fs per example.".format(etime / DATA_SIZE))
    }

    val ii = i + options.TRAIN_ITERATIONS_OFFSET
    if (options.PRINT_TRAIN_ACCURACY) {
      println("Train Accuracy at Iteration %d:".format(ii))
      println(evaluate(data, params))
    }
    if (options.PRINT_DEV_ACCURACY) {
      println("Dev Accuracy at Iteration %d:".format(ii))
      println(evaluate(new PotentialReader(options.DEV_DATA_FILE), params))
    }
    writeParams(params, options.MODEL_OUTPUT_FILE, ii, options.TRAIN_ITERATIONS + options.TRAIN_ITERATIONS_OFFSET)
  }
  params
}
*/



/*
    assert(initFile != null || pvsize > 0, "Both init.file and pv.size are not specified correctly.")
    if (initFile == null || !(new File(initFile)).exists) {
      Array.fill(pvsize+2)(0.0)
    }
    else {
      System.err.println("Initializing model from previous file.")
      val params1 = io.Source.fromFile(initFile).getLines().map(_.toDouble).toArray
      val params = Array[Double](0.0) ++ params1
      if (pvsize > params.size) {
        params ++ Array.fill(pvsize - (params.size+2))(0.0)
      }
      else {
        params
      }
    }
  }
  */


/*
    overfitting opt method

        val variance = options.VARIANCE * DATA_SIZE
        val scale = -1.0 * options.RATE
        val reg = 1 - (-scale / variance)
 //       println("scale = " + scale)
//        println("reg = " + reg)
     //   for (i <- 0 until params.size) {
      //    params(i) = params(i) * reg
     //   }
        for (idx <- avg.keys) {
 //         println("update mult = " + (t - lastUpdated(idx)))
          params(idx) = params(idx) * math.pow(reg, t - lastUpdated(idx)) // * reg
          params(idx) += avg(idx) * scale //  * (t - lastUpdated(idx))
          lastUpdated(idx) = t
        }
        t += 1
 */

/*
class HiddenStructureOptimizer(model: Model, options: OptimizerOptions) extends Optimizer(model, options) {

  override def train(data: Iterable[PotentialExample]): Array[Double] = {
    var params = init(options.INIT_FILE, options.PV_SIZE)
    val VERBOSE = options.VERBOSE
    for (i <- 0 until options.TRAIN_ITERATIONS) {
      for (batch <- order(data, i, options)) {
        if (VERBOSE) System.err.println("Batchsize = " + batch.size)
        val updates = batch.map { ex =>

          val instance1 = model.constructFromExample(ex, params)
          val instance2 = model.constructFromExample(ex.clone(), params)

          model.observedVariableFactors(instance2.graph.factors).foreach(_.clamp())

          infer(instance1, options)
          infer(instance2, options)
          val denMargs = instance1.marginals
          val numMargs = instance2.marginals

          /*
          val instance1 = model.constructFromExample(ex, params)
          val instance2 = model.constructFromExample(ex.clone(), params)

          instance2.observedVariableFactors.foreach(_.clamp())

          infer(instance1, options)
          infer(instance2, options)
          val denMargs = instance1.marginals
          val numMargs = instance2.marginals
           */

          for (i <- 0 until denMargs.size) {
            denMargs(i).value = denMargs(i).value - numMargs(i).value
          }
           val beliefs = denMargs.sortBy(_.name)

          val rate = options.RATE
          val pv = new ParameterUpdate
          val feats = ex.features

          val margs = beliefs.map(_.value)//.map( b => if (b.isCorrect) b.value - 1.0 else b.value)
          val fnames = beliefs.collect{case p if feats.contains(p.name) => feats(p.name)}

          if (VERBOSE) System.err.println("DEBUG: POST-EXP / BEFORE BP:")
          if (VERBOSE) beliefs.foreach(b => System.err.println("DEBUG: BEFORE: " + b))

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
        if (VERBOSE) System.err.println("PVV")
        if (VERBOSE) params.zipWithIndex.foreach {case(p,pi) => System.err.println(pi + "\t" + p)}
      }
      writeParams(params, i, options)
    }
    params
  }
}

             */


                                       /*


class UpgradeableOptimizer(model: Model, options: OptimizerOptions) extends Optimizer(model, options) {

}

                                         */






























                //      val PRINT_TRAIN_ACCURACY = true
//      val PRINT_DEV_ACCURACY = true

/*
      if (options.PRINT_TRAIN_ACCURACY) {
        val sums = data.foldLeft((0,0)){ case(pair, ex) =>
          val (correct, incorrect) = evaluateExample(params, ex)
          (pair._1 + correct, pair._2 + incorrect)
        }
        System.err.println("Training Accuracy: %f".format(sums._1 / (sums._1 + sums._2 * 1.0)))
      }
      if (options.PRINT_DEV_ACCURACY) {
        model.getType match {
          case t: Scorable => {
            System.err.println("Dev Accuracy for Scoreable!")
          }
          case _ => {
            val sums = new PotentialReader(options.DEV_DATA_FILE).foldLeft((0,0)){ case(pair, ex) =>
              val (correct, incorrect) = evaluateExample(params, ex)
              (pair._1 + correct, pair._2 + incorrect)
            }
            System.err.println("Dev Accuracy: %f".format(sums._1 / (sums._1 + sums._2 * 1.0)))
          }
        }
      }
     */













/*

class TwoStepOptimizer(model: Model) extends Optimizer(model) {

  override def train(data: Iterable[PotentialExample], options: OptimizerOptions): Array[Double] = {
    var params = init(options.INIT_FILE, options.PV_SIZE)
    val VERBOSE = options.VERBOSE
    for (i <- 0 until options.TRAIN_ITERATIONS) {
      for (batch <- order(data, i, options)) {
        if (VERBOSE) System.err.println("Batchsize = " + batch.size)
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

          if (VERBOSE) System.err.println("DEBUG: POST-EXP / BEFORE BP:")
          if (VERBOSE) beliefs.foreach(b => System.err.println("DEBUG: BEFORE: " + b))

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
        if (VERBOSE) System.err.println("PVV")
        if (VERBOSE) params.zipWithIndex.foreach {case(p,pi) => System.err.println(pi + "\t" + p)}
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
    val VERBOSE = options.VERBOSE
    for (i <- 0 until options.TRAIN_ITERATIONS) {
      for (batch <- order(data, i, options)) {
        if (VERBOSE) System.err.println("Batchsize = " + batch.size)
        val updates = batch.map { ex =>
 //         if (VERBOSE) System.err.println("DEBUG: GRAPH")
 //         if (VERBOSE) System.err.println(instance.graph)


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

          if (VERBOSE) System.err.println("DEBUG: POST-EXP / BEFORE BP:")
          if (VERBOSE) beliefs.foreach(b => System.err.println("DEBUG: BEFORE: " + b))

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
        if (VERBOSE) System.err.println("PVV")
        if (VERBOSE) params.zipWithIndex.foreach {case(p,pi) => System.err.println(pi + "\t" + p)}
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
						val nparams = SGDTrainer.train(params, fidxFile, constructor, maxExamples = nrExamples, bpIters = 10, VERBOSE = VERBOSE)
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
					
					if (VERBOSE) println("\nInitial Params:\n%s".format(params.zipWithIndex.mkString("\n")))
					if (VERBOSE) println("\nGraph:\n%s".format(model.graph.toString))
					println("\nGraph:\n%s".format(model.graph.toString))
					if (VERBOSE) println("\nPre-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
					System.err.println("Construction time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		

					startTime = System.currentTimeMillis()
					val stats = runBP(model, bpIters, drate=rate, dinit=1, VERBOSE = VERBOSE)
					System.err.println("Propagation time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
					System.err.println("Converged? " + stats._1 + " in " + stats._2 + " iterations.")



					startTime = System.currentTimeMillis()
					val beliefs = model.graph.potentialBeliefs.filter(_.name != "null")
					System.err.println("Gathering beliefs time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		

					if (VERBOSE) {
						println("Post-BP1 (%d beliefs)".format(beliefs.size))
						for (i <- 0 until beliefs.size) { System.err.println(beliefs(i)) }
					}

					startTime = System.currentTimeMillis()
					assert(!beliefs.exists(_.value.isNaN), "NaN detected in post-BP marginals.\nExample:\n%s\nParams:\n%s".format(ex.attributes, pvv.mkString("\n")))
					val margs = beliefs.map( b => if (b.isCorrect) b.value - 1.0 else b.value) //.map(truncate(_))

					if (VERBOSE) {
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
					if (VERBOSE) println("\nUpdated Param Vector:\n%s".format(pvv.zipWithIndex.map{case(e,i) => "%d\t%f".format(i,e)}.mkString("\n")))
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
							rate: Double = -0.01, variance: Double = 1.0, bpIters: Int = 5, VERBOSE: Boolean = false): Array[Double] = {
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
				if (VERBOSE) println("\nInitial Params:\n%s".format(params.zipWithIndex.mkString("\n")))
				if (VERBOSE) println("\nGraph:\n%s".format(model.graph.toString))
				println("\nGraph:\n%s".format(model.graph.toString))
				if (VERBOSE) println("\nPre-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
				System.err.println("Construction time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		

				startTime = System.currentTimeMillis()
				val stats = runBP(model, bpIters, drate=rate, dinit=1, VERBOSE = VERBOSE)
				System.err.println("Propagation time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
				System.err.println("Converged? " + stats._1 + " in " + stats._2 + " iterations.")

				
				
				startTime = System.currentTimeMillis()
				val beliefs = model.graph.potentialBeliefs.filter(_.name != "null")
				System.err.println("Gathering beliefs time: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
				
				if (VERBOSE) {
					println("Post-BP1 (%d beliefs)".format(beliefs.size))
					for (i <- 0 until beliefs.size) { System.err.println(beliefs(i)) }
				}

				startTime = System.currentTimeMillis()
				assert(!beliefs.exists(_.value.isNaN), "NaN detected in post-BP marginals.\nExample:\n%s\nParams:\n%s".format(ex.attributes, pvv.mkString("\n")))
				val margs = beliefs.map( b => if (b.isCorrect) b.value - 1.0 else b.value) //.map(truncate(_))

				if (VERBOSE) {
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
				if (VERBOSE) println("\nUpdated Param Vector:\n%s".format(pvv.zipWithIndex.map{case(e,i) => "%d\t%f".format(i,e)}.mkString("\n")))
				System.err.println
				count += 1
			}
			return pvv
		}	


		def test(params: Array[Double], constructor: (PotentialExample, Array[Potential]) => FactorGraphModel, ex: PotentialExample, 
		         bpIters: Int = 5, VERBOSE: Boolean = false): Model = {
			val rate = 1.0
			val variance = 0.0
			var count = 0
			var pvv = params.clone
			val damp = 0.099
			val feats   = ex.getFeatures				
			val pots    = ex.getPotentials
			if (VERBOSE) println("\nInitial Params:\n%s".format(params.zipWithIndex.mkString("\n")))
			
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
			if (VERBOSE) println("\nPre-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
			pots.foreach { pot => pot.value = Math.exp(pot.value) }
			if (VERBOSE) println("\nPre-Exp-BP Beliefs:\n%s".format(pots.map(_.toString).mkString("\n")))
			

			val model = constructor(ex, pots)
			if (VERBOSE) println("\nGraph:\n%s".format(model.graph.toString))
			if (VERBOSE) {
				println("Post-Construction")
				val beliefs = model.graph.potentialBeliefs
				for (i <- 0 until beliefs.size) {
					System.err.println(i + ": " + beliefs(i))
				}
			}
			runBP(model, bpIters, dinit = 1.0, VERBOSE = VERBOSE)
			if (VERBOSE) {
				println("Post-BP")
				val beliefs = model.graph.potentialBeliefs
				for (i <- 0 until beliefs.size) {
					System.err.println(i + ": " + beliefs(i))
				}
			}
			model
		}	

		def runBP(model: FactorGraphModel, bpiters: Int = 5, drate: Double = 0.99, dinit: Double = -0.03, threshold: Double = .001, VERBOSE: Boolean = false): (Boolean, Int) = {
			val graph = model.graph
			var damp = dinit
			for (i <- 0 until bpiters) {
				var maxDiff = -1.0
				var seenVar = false
				var startTime = System.currentTimeMillis()
				for (n <- model.messageOrder(graph)) {
					val diff = if (n.isFactor && graph.edgesFrom(n).toArray.size == 1) {
						n.computeMessages(graph, damp=1.0, VERBOSE)
					}
					else {
						n.computeMessages(graph, damp=damp, VERBOSE)						
					}
					if (VERBOSE) System.err.println("...computing messages for " + n.name + " [Update difference of " + diff + "].")
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
		def runBP(graph: FactorGraph, bpiters: Int = 5, drate: Double = 0.99, dinit: Double = -0.03, threshold: Double = .001, VERBOSE: Boolean = false): (Boolean, Int) = {
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
				val diff = u.computeMessages(model, damp=1, VERBOSE)
				if (VERBOSE) println("QUEUE: " + u.name + " = " + diff)
			}
			System.err.println("Time for unary factor propagation: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")		
			
			// Do the others with successive damping
			for (i <- 0 until bpiters) {
				if (VERBOSE) println("QUEUE Performing BP iteration %d".format(i))
				var maxDiff = -1.0
				var seenVar = false
				startTime = System.currentTimeMillis()
				for (v <- mqueue) { //.sortBy(graph.edgesFrom(_).size
					if (v.isVariable && !seenVar) {
						System.err.println("Time for other factor propagation: " + (System.currentTimeMillis() - startTime) / 1000.0 + "s.")
						startTime = System.currentTimeMillis()						
						seenVar = true
					}
					var diff = v.computeMessages(model, damp=damp, VERBOSE) //damp=damp)
					if (VERBOSE) System.err.println("QUEUE " + v.name + " = " + diff)
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










