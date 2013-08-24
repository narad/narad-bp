package narad.bp.util

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 7/20/13
 * Time: 5:00 PM
 */
// abstract class Feature(value: Double = 1, group: Int = 0) {}

case class Feature(idx: Int, value: Double = 1, group: Int=0) { //extends Feature(value, group) {

  override def toString = {
    if (value == 1 && group == 0) {
      idx.toString
    } else if (value != 1 && group == 0) {
      "%d=%f".format(idx, value)
    }
    else if (value == 1 && group != 0) {
      "%d:%d".format(group, idx)
    }
    else {
      "%d:%d=%f".format(group, idx, value)
    }
  }
}

class StringFeature(val name: String, value: Double, group: Int=0) extends Feature(-1, value, group) {

  override def toString = if (value == 1) name.toString else "%s=%f".format(name, value)

}