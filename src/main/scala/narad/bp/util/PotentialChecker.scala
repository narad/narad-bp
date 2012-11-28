package narad.bp.util
import narad.bp.structure.Potential
import scala.collection.mutable.HashSet

object PotentialChecker {
	
	def main(args: Array[String]) = {
		val file1 = args(0)
		val file2 = args(1)
		val pots1 = PotentialReader.read(file1).toArray
		val pots2 = PotentialReader.read(file2).toArray

		var i = 1
		for (pair <- PotentialReader.read(file1).zip(PotentialReader.read(file2))) {
			val orphans = new HashSet[Potential]
			for (p1 <- pair._1.getPotentials) {
				if (!pair._2.getPotentials.exists(p2 => p1.name == p2.name && p1.isCorrect == p2.isCorrect)) {
					orphans += p1
				}
			}
			if (orphans.size > 0) {
				println("Orphans for example %d:".format(i))
				println(orphans.mkString("\n"))
				println				
			}
			i += 1
		}
	}
}