package narad.bp.util

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 4/25/14
 * Time: 7:34 PM
 * paste feats model.pv | column -s $'\t' -t
 */
object FeatureIndexHelper {

  def main(args: Array[String]) {
    val options = new ArgParser(args)
    val mFile = options.getString("--model.file")
    val fFile = options.getString("--feature.file")
    val pairs = io.Source.fromFile(fFile).getLines.zip(io.Source.fromFile(mFile).getLines()).toArray
    if (options.getBoolean("--sort.by.value")) {
      pairs.sortBy(_._2.toFloat).foreach { case(name,value) => println(name + "\t" + value)}
    }
    else {  // Sort by name
      pairs.sortBy(_._1.toString).foreach { case(name,value) => println(name + "\t" + value)}
    }
  }

}
