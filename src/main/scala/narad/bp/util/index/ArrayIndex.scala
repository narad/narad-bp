package narad.bp.util.index

import collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/27/12
 * Time: 12:27 AM
 * To change this template use File | Settings | File Templates.
 * Also need to add array growing capabilities
 * Indexes start at 1.
 */
class ArrayIndex[T: Manifest](init: Int=100000000, grow: Int=10000000) extends Index[T] {
  val indexed = new mutable.HashMap[T, Int]()
  val list = new Array[T](init)
  var max = 0

  def index(t: T): Int = {
    if (indexed.contains(t)) {
      indexed(t)
    }
    else {
      max += 1
      list(max) = t
      indexed(t) = max
      max
    }
  }

  def indexOf(t: T): Int = {
    indexed.getOrElse(t, -1)
  }

  def valueOf(i: Int): Option[T] = {
    if (i < max) Option(list(i)) else None
  }

}
