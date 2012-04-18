package narad.bp.structure

abstract class Model {  // wrapper around Parser, DependencyParser, and SRLModel that could take in an example and create a model instance
		
	def graph: FactorGraph
}