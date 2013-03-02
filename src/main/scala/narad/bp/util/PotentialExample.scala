package narad.bp.util
import narad.bp.structure._
import scala.collection.mutable.{HashMap, Map => MMap}
import java.io.FileWriter


case class PotentialExample(attributes: MMap[String, String], potentials: Array[Potential], features: HashMap[String, Array[Feature]]){

  override def clone() = new PotentialExample(attributes, potentials.map(p => Potential(p.value, p.name, p.label)), features)

  def getAttributes = attributes

	def getPotentials = potentials
	
	def getFeatures = features

  def hasAttribute(key: String): Boolean = attributes.contains(key)

  def exponentiated(pvv: Array[Double]) = {
		val feats   = getFeatures				
		val pots    = getPotentials
//		System.err.println("pots size = " + pots.size)
		pots.foreach { pot => 
			pot.value = feats(pot.name).foldLeft(0.0){ (sum, feat) => 
//				System.err.println("DEBUG: sum for %s = %f + %f = %f".format(pot.name, sum, pvv(feat.idx), sum + pvv(feat.idx) * feat.value))
				sum + pvv(feat.idx) * feat.value 
			}
		}
//		for (p <- pots) println("pre-exp POTS: " + p)
		pots.foreach { pot => pot.value = scala.math.exp(pot.value) }
		pots
	}

  def writeToFile(out: FileWriter) {
    for (a <- attributes.keys) out.write("@%s\t%s\n".format(a, attributes(a)))
    val batch = true
    if (batch) {
      for (p <- potentials) out.write("%s\t%s%s\n".format(p.name, if (p.isCorrect) "+" else "", features(p.name).map(_.toString).mkString(" ")))
    }
    else {
      val builder = new StringBuilder
      for (p <- potentials) {
        builder.append("%s\t%s".format(p.name, if (p.isCorrect) "+" else ""))
        if (features(p.name).size == 0) {
          builder.append("0")
        }
        else {
          for (f <- features(p.name).map(_.toString)) {
            builder.append(f.toString)
            builder.append(" ")
          }
        }
        out.write(builder.toString())
        builder.clear()
      }
    }
    out.write("\n")
  }
}

case class Feature(idx: Int, value: Double, group: Int=0){

  override def toString = if (value == 1) idx.toString else "%d=%f".format(idx, value)
}
