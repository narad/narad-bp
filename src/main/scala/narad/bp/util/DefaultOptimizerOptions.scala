package narad.bp.util

import narad.bp.optimize.OptimizerOptions

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 10/31/13
 * Time: 5:05 PM
 */
class DefaultOptimizerOptions(args: Array[String]) extends ArgParser(args) with OptimizerOptions {
    lazy val PRINT_INTERVAL = getInt("--print.interval", 100)
    lazy val VARIANCE = getDouble("--variance", 1.0)
    lazy val RATE = getDouble("--rate", .01)
    lazy val DECAY = getDouble("--decay", 0.01)
    lazy val PV_SIZE = getInt("--pv.size", -1)
    lazy val PV_SET = getDouble("--pv.set", 0.0)
    lazy val PV_SET_RANGE = getRange("--pv.set.range", (0,0))
    lazy val TRAIN_ITERATIONS = getInt("--train.iterations", 1)
    lazy val TRAIN_ITERATIONS_OFFSET = getInt("--train.iterations.offset", 0)
    lazy val TRAIN_ORDER = getString("--train.order", "NORMAL")
    lazy val CONCURRENCY = getString("--concurrency", "HOGWILD")
    lazy val MARGINALIZATION = getBoolean("--marg", false)
    lazy val MODEL_OUTPUT_FILE = getString("--model.output.file", "model")
    lazy val NUM_CORES = getInt("--num.cores", 1)
    lazy val INIT_FILE = getString("--init.file")
    lazy val BATCH_SIZE = getInt("--batch.size", 1)
    lazy val AVERAGE_BATCH = getBoolean("--average.batch", false)
    lazy val AVERAGE_LAST = getBoolean("--average.last", false)
    lazy val TIME = getBoolean("--time", false)
    lazy val DAMP_INIT = getDouble("--damp.init", 1.0)
    lazy val DAMP_RATE = getDouble("--damp.rate", 0.001)
    lazy val DIFF_THRESHOLD = getDouble("--diff.threshold", 0.001)
    lazy val INFERENCE_ITERATIONS = getInt("--inference.iterations", 10)
    lazy val VERBOSE = getBoolean("--verbose", false)
    lazy val GROUP1_REG = getDouble("--group.reg.1", 1.0)
    lazy val GROUP2_REG = getDouble("--group.reg.2", 1.0)
    lazy val GROUP3_REG = getDouble("--group.reg.3", 1.0)
    lazy val CHECK_FOR_NAN = getBoolean("--check.nan", true)
    lazy val PRINT_GRAPH = getBoolean("--print.graph", false)
    lazy val PRINT_TRAIN_ACCURACY = getBoolean("--print.train.accuracy", false)
    lazy val PRINT_DEV_ACCURACY = getBoolean("--print.dev.accuracy", false)
    lazy val DEV_DATA_FILE = getString("--dev.data")
  }