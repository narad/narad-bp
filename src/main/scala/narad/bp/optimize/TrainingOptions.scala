package narad.bp.optimize

import narad.bp.inference.InferenceOptions

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/17/13
 * Time: 12:15 PM
 * To change this template use File | Settings | File Templates.
 */
trait OptimizerOptions extends TrainingOptions with InferenceOptions {}

trait TrainingOptions {

  def AVERAGE_BATCH: Boolean

  def AVERAGE_LAST: Boolean

  def BATCH_SIZE: Int

  def INIT_FILE: String

  def MODEL_OUTPUT_FILE: String

  def CHECK_FOR_NAN: Boolean

  def TRAIN_ORDER: String

  def PV_SIZE: Int

  def RATE: Double

  def TRAIN_ITERATIONS: Int

  def TRAIN_ITERATIONS_OFFSET: Int

  def VARIANCE: Double

  def TIME: Boolean

  def GROUP1_REG: Double

  def GROUP2_REG: Double

  def GROUP3_REG: Double

  def DEV_DATA_FILE: String

  def PRINT_DEV_ACCURACY: Boolean

  def PRINT_TRAIN_ACCURACY: Boolean

  def PRINT_GRAPH: Boolean
}