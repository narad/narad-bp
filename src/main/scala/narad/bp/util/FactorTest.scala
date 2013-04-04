package narad.bp.util

import narad.bp.structure._
import collection.mutable.ArrayBuffer
import narad.bp.optimize.OptimizerOptions
import narad.bp.inference.BeliefPropagation

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 1/30/13
 * Time: 11:13 PM
 * To change this template use File | Settings | File Templates.
 */
object FactorTest extends BeliefPropagation {

  def main(args: Array[String]) {
    val instance = args(0).toUpperCase() match {
      case "IMPLIES" => testImpliesFactor
    }

    System.out.println("Pre-BP Marginals:")
    for (marg <- instance.marginals) {
      println(marg)
    }

    val options = new TesterOptions(args)
    val (converged, inferIters) = infer(instance, options)

    System.out.println("Post-BP Marginals:")
    for (marg <- instance.marginals) {
      println(marg)
    }
  }

  def testImpliesFactor: ModelInstance = {
    val pb = new ArrayBuffer[Potential]()
    pb += new Potential(1.70, "antpot", true)
    pb += new Potential(0.35, "conpot", false)
    val pots = pb.toArray
    val fg = new FactorGraphBuilder(pots)
    val iidx = fg.addVariable("antvar", arity=2)
    fg.addUnaryFactor("antvar", "antfac", pots(0))
    val aidx = fg.addVariable("convar", arity=2)
    fg.addUnaryFactor("convar", "confac", pots(1))
    fg.addHardImpliesFactorByIndex(iidx, Array(aidx), "implies")
    new ModelInstance(fg.toFactorGraph, null)
  }
}

class TesterOptions(args: Array[String]) extends ArgParser(args) with OptimizerOptions {

  def PRINT_INTERVAL = getInt("--print.interval", 100)
  def VARIANCE = getDouble("--variance", 1.0)
  def RATE = getDouble("--rate", .01)
  def PV_SIZE = getInt("--pv.size", -1)
  def TRAIN_ITERATIONS = getInt("--train.iterations", 10)
  def TRAIN_ORDER = getString("--train.order", "NORMAL")
  def MODEL_OUTPUT_FILE = getString("--model.output.file", "model")
  def INIT_FILE = getString("--init.file")
  def BATCH_SIZE = getInt("--batch.size", 1)
  def AVERAGE_LAST = getBoolean("--average.last", false)
  def TIME = getBoolean("--time", false)
  def DAMP_INIT = getDouble("--damp.init", 1.0)
  def DAMP_RATE = getDouble("--damp.rate", 0.001)
  def DIFF_THRESHOLD = getDouble("--diff.threshold", 0.001)
  def INFERENCE_ITERATIONS = getInt("--inference.iterations", 10)
  def VERBOSE = getBoolean("--verbose", false)
  def GROUP1_REG = getDouble("--group.reg.1", 1.0)
  def GROUP2_REG = getDouble("--group.reg.2", 1.0)
  def GROUP3_REG = getDouble("--group.reg.3", 1.0)
}


class ArgParser(argArray: Array[String]) {
  var args = argArray

  def contains(str: String): Boolean = args.contains(str)

  def getInt(arg: String): Int = getString(arg).toInt

  def getInt(arg: String, default: Int): Int = getString(arg, default.toString).toInt

  def getDouble(arg: String): Double = getString(arg).toDouble

  def getDouble(arg: String, default: Double): Double = getString(arg, default.toString).toDouble

  def getString(arg: String): String = getString(arg, null.asInstanceOf[String])

  def getString(arg: String, default: String): String = {
    if (args.contains(arg)) {
      return args(args.indexOf(arg)+1)
    }
    else {
      return default
    }
  }

  def getBoolean(arg: String, default: Boolean=false): Boolean = {
    if (args.contains(arg)) {
      return args(args.indexOf(arg)+1).toLowerCase == "true"
    }
    else {
      return default
    }
  }

  def addOption(arg: String, value: String) = args = (Array(arg, value) ++ args)
}