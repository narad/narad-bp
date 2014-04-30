package narad.bp.util

import collection.mutable.HashMap

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 11/11/13
 * Time: 8:32 PM
 */
object ParamInitializer {

  def main(args: Array[String]) {
    val options = new ArgParser(args)
    val feats = new HashMap[String, Double]()
    val p = options.getString("--set.params").split(",")
    for (f <- io.Source.fromFile(options.getString("--dict.file")).getLines) {
      if (f.startsWith(p(0))) {
        println(p(1))
      }
      else {
        println(0.0)
      }
    }
  }
}
