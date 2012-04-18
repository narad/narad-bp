package narad.bp.util
import narad.bp.structure._
import scala.collection.mutable.{HashMap, Map => MMap}


case class PotentialExample(attributes: MMap[String, String], potentials: Array[Potential], features: HashMap[String, Array[Feature]]){
	def hasAttribute(key: String): Boolean = {
		return attributes.contains(key)
	}
	
	def getPotentials = potentials
	
	def getFeatures = features
}

case class Feature(idx: Int, value: Double){}
