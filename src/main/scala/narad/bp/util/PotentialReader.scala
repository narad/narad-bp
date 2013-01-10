package narad.bp.util
import narad.bp.structure.Potential
import scala.collection.mutable.{ArrayBuffer, HashMap, Map => MMap}
import scala.util.matching.Regex
import collection.mutable


class PotentialReader(filename: String) extends mutable.Iterable[PotentialExample] {
	private val POTS_PATTERN  = """([^\t]+)\t(\+?)([0-9=\\. ]+)""".r
	private val ATTRIBUTE_PATTERN  = """@([^ ]+)\t(.*)""".r
	private val END_PATTERN   = """[ \n\t]*""".r
  private val FEAT_PATTERN1 = """([0-9]+)""".r
  private val FEAT_PATTERN2 = """([0-9]+)=([\\.0-9]*)""".r
  private val FEAT_PATTERN3 = """([0-9]+:)([0-9]+)""".r
  private val FEAT_PATTERN4 = """([0-9]+:)?([0-9]+)=?([\\.0-9]*)""".r

  def iterator: Iterator[PotentialExample] = {
		val lines = scala.io.Source.fromFile(filename).getLines()
		Iterator.continually(read(lines)).takeWhile(_ != null)
	}

  def read(lines: Iterator[String]) : PotentialExample = {
      val map  = MMap[String, String]()
      val pots = new ArrayBuffer[Potential]
      val feats = new HashMap[String, Array[Feature]]
      try {
           Iterator.continually(lines.next()).takeWhile{line => lines.hasNext && !matches(line, END_PATTERN)}.foreach { line =>
             if (line.contains("slen")) System.err.println("...reading line..." + line)
             line match {
            case ATTRIBUTE_PATTERN(name, value) => {
              map(name) = value
            }
            case POTS_PATTERN(name, label, featstr) => {
              val pfeats = featstr.split(" ").flatMap { _ match {
                case "0" => None
                case FEAT_PATTERN1(fidx) =>  Some(new Feature(fidx.toInt, 1.0, 0))
                case FEAT_PATTERN2(fidx, value) => Some(new Feature(fidx.toInt, value.toDouble, 0))
                case FEAT_PATTERN3(group, fidx) => Some(new Feature(fidx.toInt, 1.0, group.toInt))
                case FEAT_PATTERN4(group, fidx, value) =>  Some(new Feature(fidx.toInt, value.toDouble, group.toInt))
              }
              }
              pots += new Potential(0.0, name, label == "+")
              feats(name) = pfeats
            }
          }
        }
      }
      catch { case e: Exception => {} }

    System.err.println("Pot Reader pots.size = " + pots.size)
    if (pots.size > 0) {
      return new PotentialExample(map, pots.toArray, feats)
    }
    else {
      return null.asInstanceOf[PotentialExample]
    }
  }

  def matches(str: String, regex: Regex): Boolean = {
    regex.pattern.matcher(str).matches
  }
}
















/*

//  def hasNext : Boolean = lines.hasNext

  def readNext(lines: Iterator[String]): PotentialExample = {
    val map  = MMap[String, String]()
    val pots = new ArrayBuffer[Potential]
    val feats = new HashMap[String, Array[Feature]]
    try {
//        while ({line = lines.next();  line != null && !matches(line, END_PATTERN)}) {
      Iterator.continually(lines.next()).takeWhile{line => lines.hasNext && !matches(line, END_PATTERN)}.foreach { line =>
        line match {
          case ATTRIBUTE_PATTERN(name, value) => {
            map(name) = value
          }
          case POTS_PATTERN(name, label, featstr) => {
            val pfeats = featstr.split(" ").flatMap { _ match {
                case "0" => None
                case FEAT_PATTERN1(fidx) =>  Some(new Feature(fidx.toInt, 1.0, 0))
                case FEAT_PATTERN2(fidx, value) => Some(new Feature(fidx.toInt, value.toDouble, 0))
                case FEAT_PATTERN3(group, fidx) => Some(new Feature(fidx.toInt, 1.0, group.toInt))
                case FEAT_PATTERN4(group, fidx, value) =>  Some(new Feature(fidx.toInt, value.toDouble, group.toInt))
              }
            }
            pots += new Potential(0.0, name, label == "+")
            feats(name) = pfeats
          }
        }
      }
      System.err.println("Pot Reader pots.size = " + pots.size)
      if (pots.size > 0) {
        return new PotentialExample(map, pots.toArray, feats)
      }
    }
    catch {
      case e: Exception => {
        assert(pots.size == 0, "Potentials found on error in PotentialReader.  Error: %s".format(e.getStackTrace().mkString("\n")))
      }
    }
    return null.asInstanceOf[PotentialExample]
  }

 */


    /*
                  val FEAT_PATTERN1(group, idx, value) = feat
              val dval = if (value.size > 0) value.toDouble else 1.0
              val gval = if (group == null) 0 else group.toInt
              new Feature(idx.toInt, dval, gval)
            }
     */









//	override def toString: String = {
//		return "%s\t%s%s".format(name, if (label) "+" else "", features.map(_.idx).mkString(" "))	
//	}





/*
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

*/