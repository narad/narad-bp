package narad.bp.util
import narad.bp.structure._
import scala.collection.mutable.{HashMap, Map => MMap}


case class PotentialExample(attributes: MMap[String, String], potentials: Array[Potential], features: HashMap[String, Array[Feature]]){
	
	def hasAttribute(key: String): Boolean = {
    attributes.contains(key)
	}
	
	def getPotentials = potentials
	
	def getFeatures = features
	
	def exponentiated(pvv: Array[Double]) = {
		val feats   = getFeatures				
		val pots    = getPotentials
//		System.err.println("pots size = " + pots.size)
		pots.foreach { pot => 
			pot.value = feats(pot.name).foldLeft(0.0){ (sum, feat) => 
				System.err.println("DEBUG: sum for %s = %f + %f = %f".format(pot.name, sum, pvv(feat.idx), sum + pvv(feat.idx) * feat.value))
				sum + pvv(feat.idx) * feat.value 
			}
		}
		for (p <- pots) println("pre-exp POTS: " + p)
		pots.foreach { pot => pot.value = scala.math.exp(pot.value) }
		pots
	}
}

case class Feature(idx: Int, value: Double, group: Int=0){}
