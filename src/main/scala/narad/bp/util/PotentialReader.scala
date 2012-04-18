package narad.bp.util
import narad.bp.structure.Potential
import scala.collection.mutable.{ArrayBuffer, HashMap, Map => MMap}


object PotentialReader { 
	private val potsPattern  = """([^\t]+)\t(\+?)([0-9= ]+)""".r
	private val attrPattern  = """(@[^ ]+)\t(.*)""".r
	private val endPattern   = """[ \n\t]*""".r
	private val countPattern = """([0-9]+)=?([0-9]*)""".r
	private var processed = 0

	def read(filename: String): Iterator[PotentialExample] = {
		val lines = scala.io.Source.fromFile(filename).getLines.toArray
		Iterator.continually(readNext(lines)).takeWhile(_ != null)
	}
	
	def readNext(lines: Array[String]): PotentialExample = {
		var start = processed
		val map  = MMap[String, String]()
		val pots = new ArrayBuffer[Potential]
		val feats = new HashMap[String, Array[Feature]]
		
		while (processed < lines.size) {
			val line = lines(processed)
			processed += 1
			line match {
				case attrPattern(name, value) => {
					map(name) = value
			  }
				case potsPattern(name, label, featstr) => {
					val pfeats = featstr.split(" ").filter(_ != "0").map { feat =>
						val countPattern(idx, value) = feat
						val dval = if (value.size > 0) value.toDouble else 1.0
						new Feature(idx.toInt, dval)
					}
					pots += new Potential(0.0, name, label == "+")
					feats(name) = pfeats
				}
				case endPattern => {
//					println("Pushing examlpe with %d pots".format(pots.size))
					return new PotentialExample(map, pots.toArray, feats)
				}
		  }
		}
//		println("Returned the null pot.")
		processed = 0
		return null.asInstanceOf[PotentialExample]
	}
}





//	override def toString: String = {
//		return "%s\t%s%s".format(name, if (label) "+" else "", features.map(_.idx).mkString(" "))	
//	}
