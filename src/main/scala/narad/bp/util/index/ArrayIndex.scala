package narad.bp.util.index

import collection.mutable.HashMap
import java.io.FileWriter

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
  val indexed = new HashMap[T, Int]()
  val list = new Array[T](init)
  var max = 0
  var frozen = false

  def freeze = frozen = true

  def unfreeze = frozen = false

  def index(t: T): Int = {
    if (indexed.contains(t)) {
      indexed(t)
    }
    else {
      if (frozen) {
        return 0
      }
      else {
        max += 1
        list(max) = t
        indexed(t) = max
        max
      }
    }
  }

  def indexOf(t: T): Int = {
    indexed.getOrElse(t, -1)
  }

  def size = max

  def valueOf(i: Int): Option[T] = {
    if (i < max) Option(list(i)) else None
  }

  def writeToFile(filename: String) {
    val out = new FileWriter(filename)
    for (e <- 1 to max) {
      out.write(list(e) + "\n")
    }
    out.close()
  }
}
