package narad.bp.util.index


/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 3/7/13
 * Time: 11:16 PM
 * To change this template use File | Settings | File Templates.
 */
object PotentialIndexer {

  def main(args: Array[String]) {
    val filename = args(0)
    val index = if (args.size > 1) {
      val pvsize = args(1).toInt
      new HashIndex(pvsize)
    }
    else {
      new ArrayIndex[String]
    }
    for (line <- io.Source.fromFile(args(0), "UTF-8").getLines) {
      if (line.startsWith("@") || line.isEmpty) {
        println(line)
      }
      else {
        val Array(name, featstr) = line.split("\t")
        val correct = featstr.startsWith("+")
        if (correct) {
          val indexed = featstr.substring(1).split(" ").view.map(index.index(_)).sortBy(_*1).mkString(" ")
          println("%s\t+%s".format(name, indexed))
        }
        else {
          val indexed = featstr.split(" ").view.map(index.index(_)).sortBy(_*1).mkString(" ")
          println("%s\t%s".format(name, indexed))
        }
      }
    }
  }
}
