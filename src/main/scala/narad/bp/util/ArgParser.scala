package narad.bp.util

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/1/13
 * Time: 11:11 AM
 * To change this template use File | Settings | File Templates.
 */
class ArgParser(argArray: Array[String]) {
  var args = argArray

  def contains(str: String): Boolean = args.contains(str)

  def getInt(arg: String): Int = getString(arg).toInt

  def getInt(arg: String, default: Int): Int = getString(arg, default.toString).toInt

  def getDouble(arg: String): Double = getString(arg).toDouble

  def getDouble(arg: String, default: Double): Double = getString(arg, default.toString).toDouble

  def getRange(arg: String, default: (Int, Int)): (Int,Int) = {
    val a = getString(arg, default.toString).substring(1, default.toString.size-1).split(",")
    (a(0).toInt, a(1).toInt)
  }

  def getString(arg: String): String = getString(arg, null.asInstanceOf[String])

  def getString(arg: String, default: String): String = {
    if (args.contains(arg)) {
      return args(args.indexOf(arg)+1)
    }
    else {
      return default
    }
  }

  def getBoolean(arg: String, default: Boolean=false): Boolean = {
    if (args.contains(arg)) {
      return args(args.indexOf(arg)+1).toLowerCase == "true"
    }
    else {
      return default
    }
  }

  def addOption(arg: String, value: String) = args = (Array(arg, value) ++ args)
}