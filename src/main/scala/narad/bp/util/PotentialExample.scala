package narad.bp.util
import narad.bp.structure._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import java.io.FileWriter
import collection.mutable


class PotentialExample(){
  val attributes = new HashMap[String, String]()
  val potentials = new ArrayBuffer[Potential]
  val features = new HashMap[String, Array[Feature]]

  override def clone(): PotentialExample = {
    val copy = new PotentialExample()
    copy.attributes ++= attributes
    copy.potentials ++= potentials.map(p => Potential(p.value, p.name, p.label))
    copy.features ++= features
    copy
  }

  def += (other: PotentialExample) {
    attributes ++= other.attributes
    potentials ++= other.potentials
    features ++= other.features
  }

  def isEmpty = potentials.isEmpty

  def getAttributes = attributes

	def getPotentials = potentials
	
	def getFeatures = features

  def hasAttribute(key: String): Boolean = attributes.contains(key)

  def addPotential(pot: Potential, feats: Array[Feature]) {
    potentials += pot
    features(pot.name) = feats
  }

  def exponentiated(pvv: Array[Double], verbose: Boolean=false) = {
		val feats   = getFeatures				
		val pots    = getPotentials
		pots.foreach { pot =>
			pot.value = feats(pot.name).filter(_.idx > 0).foldLeft(0.0){ (sum, feat) =>
				if (verbose) System.err.println("DEBUG: sum for %s = %f + %f = %f".format(pot.name, sum, pvv(feat.idx), sum + pvv(feat.idx) * feat.value))
        sum + pvv(feat.idx) * feat.value
			}
		}
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
      for (p <- potentials) {
        val feats = features(p.name).filter(_.idx > 0).map(_.toString)
        if (feats.isEmpty) {
          out.write("%s\t%s0\n".format(p.name, if (p.isCorrect) "+" else ""))
        }
        else {
          out.write("%s\t%s%s\n".format(p.name, if (p.isCorrect) "+" else "", feats.mkString(" ")))
        }
      }
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

