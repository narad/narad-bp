package narad.bp.util.index
import scala.math._

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/27/12
 * Time: 12:24 AM
 * To change this template use File | Settings | File Templates.
 */
class HashIndex(n: Int=1000000000, base: Int=0) extends Index[String] {

  def index(s: String): Int = {
    val h = abs(s.hashCode)+1
    base + (if (h < n) h else (h % n)+1)
  }

  def indexOf(s: String) = index(s)

  def setRange(low: Int, high: Int): HashIndex = {
    new HashIndex(high-low, low)
  }

  def size = -1

  def writeToFile(filename: String) = {
    // Does nothing
  }
}


/*
  def index(s: String, range: (Int, Int)): Int = {
    assert(range._1 < range._2, "Invalid range in HashIndex index().")
    val h = abs(s.hashCode)+1
    val w = range._2 - range._1
    w + (if (h < w) h else (h % w)+1)
  }
  */