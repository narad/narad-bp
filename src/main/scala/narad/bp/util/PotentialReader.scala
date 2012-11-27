package narad.bp.util
import narad.bp.structure.Potential
import scala.collection.mutable.{ArrayBuffer, HashMap, Map => MMap}


object PotentialReader { 
	private val potsPattern  = """([^\t]+)\t(\+?)([0-9=\\. ]+)""".r
	private val attrPattern  = """@([^ ]+)\t(.*)""".r
	private val endPattern   = """[ \n\t]*""".r
	private val countPattern = """([0-9]*:)?([0-9]+)=?([\\.0-9]*)""".r
	private var processed = 0

// NEED TO REWRITE TO NOT GULP IN ENTIRE FILE AT ONCE
	def read(filename: String): Iterator[PotentialExample] = {
		val lines = scala.io.Source.fromFile(filename).getLines.toArray
		Iterator.continually(readNext(lines)).takeWhile(_ != null)
	}
	
	def readNext(lines: Array[String]): PotentialExample = {
	//	System.err.println("reading next chunk of file...")
		var start = processed
		val map  = MMap[String, String]()
		val pots = new ArrayBuffer[Potential]
		val feats = new HashMap[String, Array[Feature]]
		
		while (processed < lines.size) {
			val line = lines(processed)
			processed += 1
//			System.err.println(line)
			line match {
				case attrPattern(name, value) => {
					map(name) = value
			  }
				case potsPattern(name, label, featstr) => {
					val pfeats = featstr.split(" ").filter(_ != "0").map { feat =>
						val countPattern(group, idx, value) = feat
//						System.err.println(line)
//						System.err.println("group? = |%s|".format(group))
//						System.err.println("id = |%s|".format(idx))
//						System.err.println("val = |%s|".format(value))
//						System.err.println
						val dval = if (value.size > 0) value.toDouble else 1.0
						val gval = if (group == null) 0 else group.toInt
						new Feature(idx.toInt, dval, gval)
					}
					pots += new Potential(0.0, name, label == "+")
					feats(name) = pfeats
				}
				case endPattern => {
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
