package narad.bp.structure

case class Potential(var value: Double, name: String, label: Boolean) { 

	def isCorrect: Boolean = label
	
	override def equals(that: Any): Boolean = that match { 
	   case other: Potential => this.name == other.name
	   case _ => false
	}
	
	override def toString = "%s%s[%f]".format(if (label) "+" else "-", name, value)
}
