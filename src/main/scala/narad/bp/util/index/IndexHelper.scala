package narad.bp.util.index

import java.io.{FileWriter, File, FileReader, BufferedReader}

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 12/27/12
 * Time: 12:58 AM
 * To change this template use File | Settings | File Templates.
 */
object IndexHelper {

  def constructFromPotentialFile(input: String, index: Index[String]) {
    val src = new BufferedReader(new FileReader(input))
    try {
      var line = src.readLine
      while (line != null) {
        if (line.contains("\t")) {
          val cols = line.split("\t")(1).split(" ").foreach(index.index(_))
        }
        line = src.readLine
      }
    }
    finally {
      src.close
    }
  }

  def indexPotentialFile(input: String, output: String, index: Index[String]) {
    val src = new BufferedReader(new FileReader(input))
    val out = new FileWriter(output)
    try {
      var line = src.readLine
      while (line != null) {
        if (line.startsWith("@")) {
          out.write(line + "\n")
        }
        else if (line.contains("\t")) {
          val cols = line.split("\t")
          val correct = cols(1).substring(0,1) == "+"
          val features = if (correct) cols(1).substring(1) else (cols(1))
          val ints = features.split(" ").map(index.index(_)).filter(_ >= 0)
          val name = cols(0)
          val label = if (correct) "+" else ""
          if (ints.size == 0) {
            out.write("%s\t%s0\n".format(name, label))
          }
          else {
            out.write("%s\t%s".format(name, label))
            val sorted = ints.groupBy(_*1).toList.sortBy(_._1*1).foreach{ g =>
              if (g._2.size == 1) {
                out.write(g._1.toString + " ")
              }
              else {
                out.write("%s=%s ".format(g._1, g._2.size))
              }
            }
            out.write("\n")
          }
        }
        else {
          out.write(line)
          out.write("\n")
        }
        line = src.readLine
      }
    }
    finally {
      src.close
      out.close
    }
  }
}
