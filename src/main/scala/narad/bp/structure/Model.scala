package narad.bp.structure
import narad.bp.inference._
import narad.bp.util.PotentialExample
import scala.util.matching.Regex
import narad.bp.util.index.Index

abstract class Model {  // wrapper around Parser, DependencyParser, and SRLModel that could take in an example and create a model instance
		
	def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance

	def decode(instance: ModelInstance)

	def options: ModelOptions

  def observedVariableFactors(factors: Array[Factor]): Array[Factor] = Array()
	
}

abstract class FactorGraphModel extends Model with InferenceOrder {
	
//	def graph: FactorGraph
	
//	def construct: (PotentialExample, Array[Potential]) => FactorGraphModel
	
}

trait HiddenStructure {

//  def hiddenVariableFactors(factors: Array[Factor]): Array[Factor]

  def observedVariableFactors(factors: Array[Factor]): Array[Factor]

}

abstract class HiddenStructureModel extends Model with HiddenStructure {
/*
  def constructFromExample(ex: PotentialExample, pv: Array[Double]): HiddenStructureModelInstance

  def decode(instance: HiddenStructureModelInstance)
*/
}

abstract class HiddenStructureModelInstance(override val graph: FactorGraph, override val ex: PotentialExample) extends ModelInstance(graph, ex) {

  def hiddenVariableFactors: Array[Factor]

  def observedVariableFactors: Array[Factor]

}

class ModelInstance(val graph: FactorGraph, val ex: PotentialExample) extends InferenceOrder {

	def features = ex.getFeatures
	
	def marginals = graph.potentialBeliefs.filter(_.name != "null")

}

trait ModelOptions {}

trait UpgradeableTo[T <: ModelInstance] {

  def upgrade(ex: ModelInstance, dict: Index[String]): T

}




