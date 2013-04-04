package narad.bp.util
import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.util.zip.GZIPInputStream

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 3/16/13
 * Time: 11:23 AM
 * To change this template use File | Settings | File Templates.
 */
class GZipReader(filename: String) extends Iterator[String] {
  val gzip = new GZIPInputStream(new FileInputStream(filename))
  val br = new BufferedReader(new InputStreamReader(gzip))

  var nextUp = br.readLine()

  def close = br.close()

  def hasNext = nextUp != null

  def next = {
    val cur = nextUp
    nextUp = br.readLine()
    cur
  }

}
