package narad.bp.optimize

import java.io.FileWriter
import collection.mutable.{HashMap, ArrayBuffer}

/*
class ParameterVector(n: Int) extends RandomAccessSeq[Double] {

  override def apply(i:Int) = {
    super.apply(i)
  }

	def length = n
	
}

class ParameterUpdate {

  def add(other: ParameterUpdate): ParameterUpdate = {
    val npu = new ParameterUpdate
    for (i <- 0 until this.size) {

    }
    val npu = new ParameterUpdate
    for (i <- keys) {
      npu(i) = this.getOrElse(i, 0.0) + other.getOrElse(i, 0.0)
    }
  }

}

*/

class ParameterUpdate extends HashMap[Int, Double] {

	def add(other: ParameterUpdate): ParameterUpdate = {
		val npu = new ParameterUpdate
		for (i <- keys) {
			npu(i) = this.getOrElse(i, 0.0) + other.getOrElse(i, 0.0)
		}
		npu
	}
	
}

