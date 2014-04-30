package narad.bp.util.index

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/27/12
 * Time: 12:11 AM
 * To change this template use File | Settings | File Templates.
 */
abstract class Index[T] {

  def index(t: T): Int

  def indexOf(t: T): Int

  def setRange(low: Int, high: Int): Index[T]

  def size: Int

  def writeToFile(filename: String)

}

