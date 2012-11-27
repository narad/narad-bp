package narad.bp.structure
import narad.bp.inference._
import narad.bp.util.PotentialExample
import scala.util.matching.Regex

abstract class Model {  // wrapper around Parser, DependencyParser, and SRLModel that could take in an example and create a model instance
		
	def constructFromExample(ex: PotentialExample, pv: Array[Double]): ModelInstance

	def decode(instance: ModelInstance)

	def options: ModelOptions	
	
}

abstract class FactorGraphModel extends Model with InferenceOrder {
	
//	def graph: FactorGraph
	
//	def construct: (PotentialExample, Array[Potential]) => FactorGraphModel
	
}

class ModelInstance(val graph: FactorGraph, ex: PotentialExample) extends InferenceOrder {
	
	def features = ex.getFeatures
	
	def marginals = graph.potentialBeliefs.filter(_.name != "null")

}

trait ModelOptions {
	
}


