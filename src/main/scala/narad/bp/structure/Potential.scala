package narad.bp.structure

case class Potential(var value: Double, name: String, label: Boolean) { 

  def copy = Potential(value, name, label)

	def isCorrect: Boolean = label
	
	override def equals(that: Any): Boolean = that match { 
	   case other: Potential => this.name == other.name
	   case _ => false
	}

  override def clone = new Potential(value, name, label)
	
//	override def toString = "%s\t%s".format(name, if (label) "+" else "")

  override def toString = "%s%s[%f]".format(if (label) "+" else "-", name, value)
}
