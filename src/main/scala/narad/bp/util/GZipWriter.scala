package narad.bp.util

import java.io.{FileOutputStream, OutputStreamWriter}
import java.util.zip.GZIPOutputStream

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 3/19/13
 * Time: 10:23 AM
 * To change this template use File | Settings | File Templates.
 */
class GZipWriter(filename: String) extends java.io.FileWriter(filename){
  val stream = new FileOutputStream(filename);
  val out = new OutputStreamWriter(new GZIPOutputStream(stream), "UTF-8")

  override def close() {
    out.close()
  }

  override def write(str: String) {
    out.write(str)
  }
}
