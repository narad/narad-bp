package narad.bp.optimize

/**
 * Created with IntelliJ IDEA.
 * User: narad
 * Date: 5/3/13
 * Time: 12:38 PM
 * To change this template use File | Settings | File Templates.
 */
abstract trait Scorable {

  def score(other: Scorable): EvalContainer

}


//abstract class EvalContainer {

trait EvalContainer {

  def combine(other: EvalContainer): EvalContainer

}



/*
class EvalContainer extends HashCounter[String] {

  def combine(that: EvalContainer): EvalContainer = {
    val e = new EvalContainer
    for (k <- this.keys) {
      e.increment(k, this(k))
    }
    for (k <- that.keys) {
      e.increment(k, that(k))
    }
    return e
  }
}
*/