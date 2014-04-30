package narad.bp.structure
import narad.bp.inference._
import narad.bp.util.PotentialExample
import scala.util.matching.Regex
import narad.bp.util.index.Index

abstract class Model[+T] {

	def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance

	def decode(instance: ModelInstance): T

	def options: ModelOptions

//  def observedVariableFactors: Iterator[Factor] = Array()

  def fromPotentialExample(ex: PotentialExample, pv: Array[Double]): T = {
    val instance = constructFromExample(ex, pv)
    instance.marginals.foreach { b => if (b.isCorrect) b.value = 1.0 else b.value = 0.0 }
    decode(instance)
  }

  def usesClampedTraining = false
}

abstract class  FactorGraphModel[T] extends Model[T] with InferenceOrder {}

trait ModelOptions {}



class ModelInstance(val graph: FactorGraph, val ex: PotentialExample) extends InferenceOrder {

  override def clone = new ModelInstance(graph.copy, ex)

  def features = ex.getFeatures

  def marginals = graph.potentialBeliefs.filter(_.name != "null")

  def isExact = false

  def clampedFactors = Iterator[Factor]()

}

trait UpgradeableTo[T <: ModelInstance] {

  def upgrade(ex: ModelInstance, dict: Index[String]): T

}







//  def hiddenVariableFactors = Array[Factor]()

//	def graph: FactorGraph
	
//	def construct: (PotentialExample, Array[Potential]) => FactorGraphModel
	
//}

/*
trait HiddenStructure {

//  def hiddenVariableFactors(factors: Array[Factor]): Array[Factor]

  def observedVariableFactors(factors: Array[Factor]): Array[Factor]

}
*/

/*
abstract class HiddenStructureModel[T] extends Model[T] with HiddenStructure {
/*
  def constructFromExample(ex: PotentialExample, pv: Array[Double]): HiddenStructureModelInstance

  def decode(instance: HiddenStructureModelInstance)
*/
}

abstract class HiddenStructureModelInstance(override val graph: FactorGraph, override val ex: PotentialExample) extends ModelInstance(graph, ex) {

  def hiddenVariableFactors: Array[Factor]

  def observedVariableFactors: Array[Factor]

}

*/


