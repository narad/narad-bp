package narad.bp.util

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 4/19/14
 * Time: 11:22 AM
 */
class Timer {
  private var startTime = 0.0
  private var stopTime = 0.0

  def elapsedTime() = (stopTime - startTime)

  def start() = startTime = System.currentTimeMillis()

  def stop() = stopTime = System.currentTimeMillis()

  override def toString = {
    val etime = elapsedTime() / 1000.0
    if (etime > 60)
      "%fm".format(etime/60)
    else
      "%fs".format(etime)
  }

}
