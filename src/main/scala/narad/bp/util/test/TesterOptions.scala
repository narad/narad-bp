package narad.bp.util.test

import narad.bp.optimize.OptimizerOptions
import narad.bp.util.ArgParser

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/1/13
 * Time: 11:12 AM
 * To change this template use File | Settings | File Templates.
 */
class TesterOptions(args: Array[String]) extends ArgParser(args) with OptimizerOptions {

  def PRINT_INTERVAL = getInt("--print.interval", 100)
  def VARIANCE = getDouble("--variance", 1.0)
  def RATE = getDouble("--rate", .01)
  def PV_SIZE = getInt("--pv.size", -1)
  def TRAIN_ITERATIONS = getInt("--train.iterations", 1)
  def TRAIN_ITERATIONS_OFFSET = getInt("--train.iterations.offset", 0)
  def TRAIN_ORDER = getString("--train.order", "NORMAL")
  def MODEL_OUTPUT_FILE = getString("--model.output.file", "model")
  def INIT_FILE = getString("--init.file")
  def BATCH_SIZE = getInt("--batch.size", 1)
  def AVERAGE_BATCH = getBoolean("--average.batch", false)
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
  def CHECK_FOR_NAN = getBoolean("--check.nan", true)
  def PRINT_GRAPH = getBoolean("--print.graph", false)
  def PRINT_TRAIN_ACCURACY = getBoolean("--print.train.accuracy", false)
  def PRINT_DEV_ACCURACY = getBoolean("--print.dev.accuracy", false)
  def DEV_DATA_FILE = getString("--dev.data")
}



/*
  def TRAIN_ITERATIONS_OFFSET: Int = {
    val fname = INIT_FILE
    if (fname == null) {
      0
    }
    else {
      val pidx = fname.indexOf(".")
      fname.substring(pidx, fname.indexOf(".", pidx+1)).toInt
    }
  }

  */