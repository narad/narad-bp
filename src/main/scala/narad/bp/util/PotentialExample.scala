package narad.bp.util
import narad.bp.structure._
import scala.collection.mutable.{ArrayBuffer, HashMap, Map => MMap}
import java.io.FileWriter
import collection.mutable


case class PotentialExample(attributes: MMap[String, String], potentials: ArrayBuffer[Potential], features: HashMap[String, Array[Feature]]){

  override def clone() = new PotentialExample(attributes, potentials.map(p => Potential(p.value, p.name, p.label)), features)

  def getAttributes = attributes

	def getPotentials = potentials
	
	def getFeatures = features

  def hasAttribute(key: String): Boolean = attributes.contains(key)

  def addPotential(pot: Potential, feats: Array[Feature]) {
    potentials += pot
    features(pot.name) = feats
  }

  def exponentiated(pvv: Array[Double]) = {
		val feats   = getFeatures				
		val pots    = getPotentials
//		System.err.println("pots size = " + pots.size)
		pots.foreach { pot =>
			pot.value = feats(pot.name).filter(_.idx > 0).foldLeft(0.0){ (sum, feat) =>
//				System.err.println("DEBUG: sum for %s = %f + %f = %f".format(pot.name, sum, pvv(feat.idx), sum + pvv(feat.idx) * feat.value))
        sum + pvv(feat.idx) * feat.value
			}
		}
//		for (p <- pots) println("pre-exp POTS: " + p)
		pots.foreach { pot => pot.value = scala.math.exp(pot.value) }
		pots.toArray
	}

  override def toString = {
    val ab = new StringBuilder()
      for (a <- attributes.keys) ab.append("@%s\t%s\n".format(a, attributes(a)))
      for (p <- potentials) {
        val feats = features(p.name).filter(_.idx > 0).map(_.toString)
        if (feats.isEmpty) {
          ab.append("%s\t%s0\n".format(p.name, if (p.isCorrect) "+" else ""))
        }
        else {
          ab.append("%s\t%s%s\n".format(p.name, if (p.isCorrect) "+" else "", feats.mkString(" ")))
        }
      }
    ab.toString()
  }

  def writeToFile(out: FileWriter) {
    for (a <- attributes.keys) out.write("@%s\t%s\n".format(a, attributes(a)))
    val batch = true
//    if (batch) {
      for (p <- potentials) {
        val feats = features(p.name).filter(_.idx > 0).map(_.toString)
        if (feats.isEmpty) {
          out.write("%s\t%s0\n".format(p.name, if (p.isCorrect) "+" else ""))
        }
        else {
          out.write("%s\t%s%s\n".format(p.name, if (p.isCorrect) "+" else "", feats.mkString(" ")))
        }
      }
 //   }
/*
    else {
      val builder = new StringBuilder
      for (p <- potentials) {
        builder.append("%s\t%s".format(p.name, if (p.isCorrect) "+" else ""))

        if (features(p.name).filter(_.idx > 0) == 0) {
          builder.append("0")
        }
        else {
          for (f <- features(p.name).filter(_.idx > 0).map(_.toString)) {
            builder.append(f.toString)
            builder.append(" ")
          }
        }
        out.write(builder.toString())
        builder.clear()
      }
    }
 */
    out.write("\n")
  }
}

case class Feature(idx: Int, value: Double = 1, group: Int=0){

  override def toString = {
    if (value == 1 && group == 0) {
      idx.toString
    } else if (value != 1 && group == 0) {
      "%d=%f".format(idx, value)
    }
    else if (value == 1 && group != 0) {
      "%d:%d".format(group, idx)
    }
    else {
      "%d:%d=%f".format(group, idx, value)
    }
  }
}

case class StringFeature(name: String, override val value: Double, override val group: Int=0) extends Feature(1, value, group) {

  override def toString = if (value == 1) name.toString else "%s=%f".format(name, value)

}