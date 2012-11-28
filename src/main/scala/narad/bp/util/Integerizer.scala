package narad.bp.util
import scala.collection.mutable.{ArrayBuffer, HashMap}
import java.io.{BufferedReader, File, FileReader, FileWriter}

class HashedFeatureIndex(n: Int) extends FeatureIndex {
	
	override def index(s: String): Int = {
		val h = s.hashCode
		return Math.abs(h % n)
	}
}

object HashedFeatureIndex {
	
	def main(args: Array[String]) = {
		val hasher = new HashedFeatureIndex(args(2).toInt)
		val trainfile = args(0)
		val testfile  = args(1)
		hasher.indexFile(new File(trainfile), new FileWriter(trainfile + ".fidx"))
		hasher.indexFile(new File(testfile), new FileWriter(testfile + ".fidx"))
	}
}


class FeatureIndex extends Index[String] {
	var frozen = false

	override def index(f: String): Int = {
		if (frozen) super.indexOf(f) else super.index(f)
	}

	def freeze = frozen = true
	
	def unfreeze = frozen = false

	def writeIndex(featsFile: File) = {
		if (featsFile != null) {
			val out = new java.io.FileWriter(featsFile)
			for (feat <- elements) {
				out.write(feat + "\n")
			}
			out.close
		}
	}

	def indexFile(fidxFile: File, out: FileWriter) = {
		val feats = new FeatureIndex
		val src = new BufferedReader(new FileReader(fidxFile))
		try {
			var line = src.readLine
			while (line != null) {
				if (line.startsWith("@")) {
					out.write(line + "\n")
				}
				else if (line.contains("\t")) {
					val cols = line.split("\t")
//					System.err.println("0 = " + cols(0))
//					System.err.println("1 = " + cols(1))
					val correct = cols(1).substring(0,1) == "+"
//					System.err.println("correct? " + correct)
					val features = if (correct) cols(1).substring(1) else (cols(1))
					val ints = features.split(" ").map(index(_))//.filter(_ > 0)
					val name = cols(0)
					val label = if (correct) "+" else ""
					if (ints.size == 0) {
						out.write("%s\t%s0\n".format(name, label))						
					}
					else {						
						out.write("%s\t%s".format(name, label))						
						val sorted = ints.groupBy(_*1).toList.sortBy(_._1*1).foreach{ g =>
							if (g._2.size == 1) {
								out.write(g._1.toString + " ")
							}
							else {
								out.write("%s=%s ".format(g._1, g._2.size))
							}
						}
						out.write("\n")
					}
				}
				else {
					out.write(line)
					out.write("\n")
				}
				line = src.readLine
			}
		}
		finally {
			src.close
			out.close
		}
	}
}

object FeatureIndex {

		def construct(fidxFile: String, correctOnly: Boolean=false): FeatureIndex = {
			val feats = new FeatureIndex
			val src = new BufferedReader(new FileReader(fidxFile))
			try {
				var line = src.readLine
				while (line != null) {
					if (line.contains("\t")) {
						val cols = line.split("\t")
						val correct = cols(1).substring(0,1) == "+"
						if (correct || !correctOnly) {
							cols(1).split(" ").foreach(feats.index(_))
						}
					}
					line = src.readLine
				}
			}
			finally {
				src.close
			}
			feats.freeze
			return feats
	}
}


class Index[T] {  
	var isDirty = false
	var elems = new ArrayBuffer[T] 
	var hash = new HashMap[T, Int]
	
	def contains(e: T): Boolean = {
		hash.contains(e)
	}
	
	def elements: Iterable[T] = {
		elems
	}
	
	def index(e: T): Int = {
		if (hash.contains(e)) {
			hash(e)
		}
		else {
			elems += e
			hash  += e -> (hash.size+1)
			hash.size
		}
	}
	
	def indexOf(e: T): Int = {
		hash.getOrElse(e, -1)
	}
	
	def get(idx: Int): T = {
		elems(idx-1)
	}
	
	def size = elems.size
}




/*
	def writeFeatures2(fidxFile: File, out: FileWriter) = {
//		println("PRUNE = " + prune)
		val featPattern = new Regex("(.+)\t(\\+?)(.+)")
		val commentPattern = new Regex("@(.*)")
		val src = scala.io.Source.fromFile(fidxFile) 
		try {
		  for (line <- src.getLines) {
			line match {
				case commentPattern(stuff) => out.write(line + "\n")
				case featPattern(name, label, features) => {
//					val ints = features.split(" ").map{ f => if (prune && !fires(f)) -1 else index(f) }.filter(_ > 0)
					val ints = features.split(" ").map(index(_)).filter(_ > 0)
					if (ints.size == 0) {
						out.write("%s\t%s0\n".format(name, label))						
					}
					else {						
						val sorted = ints.groupBy(_*1).toList.sortBy(_._1*1).map{ g =>
							if (g._2.size == 1) g._1.toString else "%s=%s".format(g._1, g._2.size)
						}
						out.write("%s\t%s%s\n".format(name, label, sorted.mkString(" ")))
					}
				}
				case _=> out.write(line + "\n")
			}
		}
	}
	finally src match { case b: scala.io.BufferedSource => b.close }	
//	out.write("\n")
		out.close
	}
}
*/




/*	
line match {
	case featPattern(name, label, features) => {
		val correct = label == "+"
		if (correct || !correctOnly) features.split(" ").foreach(feats.index(_))
	}
	case _=> {}


	def construct(fidxFile: String, correctOnly: Boolean=false): FeatureIndex = {
		val feats = new FeatureIndex
		val featPattern = new Regex("(.+)\t(\\+?)(.+)")
		val commentPattern = new Regex("@(.*)")
		val src = scala.io.Source.fromFile(fidxFile) 
		try {
		  for (line <- src.getLines) {
				line match {
					case commentPattern(stuff) => {}
					case featPattern(name, label, features) => {
						val correct = label == "+"
						if (correct || !correctOnly) features.split(" ").foreach(feats.index(_))
					}
					case _=>
				}			
			}
		}
		finally src match { case b: scala.io.BufferedSource => b.close }	
//		for (line <- LineReader.read(fidxFile)) {
//		}
		feats.freeze
		return feats
	}
	*/