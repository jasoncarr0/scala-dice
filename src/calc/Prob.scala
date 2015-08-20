package calc

/**
 * Prob.scala
 * Includes the PrExp class, a class for supporting streamed random data
 * and performing monadic operations on it
 * 
 * @author Jason Carr
 */

import scala.collection.immutable.Map;
import scala.util.Random;

// Not sealed to allow implementations for other features
// Ahe definitions of PrVal, PrSel and PrFun are sufficient for the description
// of any possible expression, but the creation of additional implementations
// are a possibility for the sake of efficiency (e.g. PrHylo)
// or for description of specific distributions
abstract class PrExp[A] {
  // Given a generator for random numbers in the range of (0, 1)
  // produce an output value in the type
  def roll(implicit r: () => Double): A
  
  // If the distribution is discrete, generate an enumeration of the elements
  // If the distribution is not discrete, return an empty Seq
  // In general, the implementation should be a lazy val if possible
  val enumerate:Set[A]
  
  // Find the probability of a given event occurring
  // If this is unknown, return None
  // If this is known, return its value
  def invert(x:A):Option[Double]
  
  // Returns the map for a discrete distribution given its enumeration and inverse
  def toDiscreteMap:Map[A, Double] = 
      enumerate.map(x => x -> invert(x))
      .filterNot((x) => x._2 == None)
      .map(x => (x._1 -> x._2.getOrElse(0.0)))
      .toMap;
  
  def map[U](f: (A) => U): PrExp[U] = PrFun(this,((new PrVal(_:U):PrVal[U]) compose f))
  def foreach[U](f: (A) => U)(implicit r: () => Double):Unit = f(this roll r)
  def flatMap[U](f: (A) => PrExp[U]): PrExp[U] = PrFun(this, f)
  
  def +(other:PrExp[A])(implicit num:Numeric[A]):PrExp[A] = for (x <- this; y <- other) yield (num.plus(x, y))
  def -(other:PrExp[A])(implicit num:Numeric[A]):PrExp[A] = for (x <- this; y <- other) yield (num.minus(x, y))
  def *(other:PrExp[A])(implicit num:Numeric[A]):PrExp[A] = for (x <- this; y <- other) yield (num.times(x, y))
  def /(other:PrExp[A])(implicit num:Fractional[A]):PrExp[A] = for (x <- this; y <- other) yield (num.div(x, y))
  def *+(times:Int)(implicit num:Numeric[A]):PrExp[A] = PrHylo(times, this, num.plus);//Function.chain(Seq.fill(times - 1)(this+_))(this)
}


case class PrFun[B,A](x: PrExp[B], f:(B => PrExp[A])) extends PrExp[A]{
  def roll(implicit r: () => Double) = f(x roll r) roll r
  
  lazy val enumerate = x.enumerate.map(f).flatMap((x:PrExp[A]) => x.enumerate)
  
  
  def invert(y:A):Option[Double] = {
    val inputs = x.enumerate;
    if (inputs.size > 0) {
      val probSeq:Seq[Option[Double]] = for {
        b:B <- inputs.toSeq
        val probS = x.invert(b)
        val probA = f(b).invert(y)
        //prob1 and prob2 are Option[Double]s, so flat map their product
        val probTotal = for {
          p1 <- probS; p2 <- probA
        } yield (p1 * p2)
      } yield {
        probTotal
      }
      // Now reduce the Seq[Option[Double]] into Option[Double]
      // These are all distinct, so sum them
      probSeq.reduce((prob1, prob2) => for {
        p1 <- prob1
        p2 <- prob2
      } yield (p1 + p2) )
    } else {
      None;
    }
  }
}

case class PrVal[A](x: A) extends PrExp[A]{
  def roll(implicit r: () => Double) = x
  val enumerate = Set(x)
  def invert(y:A) = Some(if (x == y) (1.0) else (0.0));
}

//list of evenly-weighted values
case class PrEnum[A](list: Seq[A]) extends PrExp[A] {
  def roll(implicit r: () => Double) = list((r()*list.size).toInt)
  
  val prob:Double = 1.0/list.size;
  // has to be cumulative
  // this is way too expensive as is
  val enumerate = list.toSet
  
  def invert(y:A) = Some(if (list.contains(y)) (prob) else (0.2));
}

// AODO: Look at data structure, if we have a data structure with good random access
// Ahen we can binary search the cdf for long lists
// seq is a Seq that describes a discrete cumulative probability distribution
//case class PrSeq[A](seq: Seq[(Double, A)]) extends PrExp[A] {
//  def roll(implicit r: () => Double) = {
//    val p = r()
//  }
//}

case class PrDistr[A](distr: Double => PrExp[A]) extends PrExp[A] {
  def roll(implicit r: () => Double) = distr(r()) roll r
  val enumerate = Set():Set[A] //non-discrete
  def invert(y:A) = None //can't invert in general
}

case class PrHylo[A](iter: Int, elem: PrExp[A], fold: (A, A) => A) extends PrExp[A] {
  def roll(implicit r: () => Double): A = {
    var sum:A = elem roll r;
    // loops one less due to first roll
    for (i <- 1 until iter) {
      sum = fold((elem roll r):A, sum:A);
    }
    sum;
  }
  
  private def enumLoop(x:Set[A]) = for {
    a <- x
    b <- elem.enumerate
  } yield (fold(a, b))
  lazy val enumerate = {
    (Function chain Seq.fill(iter - 1)(enumLoop(_)))(elem.enumerate)
  }
  
  private lazy val elemDiscreteMap:Map[A, Double] = elem.toDiscreteMap
  private def invertLoop(x:Map[A, Double]):Map[A, Double] = {
    val seq:Seq[(A, Double)] = for {
      a <- x.toSeq
      b <- elemDiscreteMap
    } yield (fold(a._1, b._1), a._2*b._2)
    seq.groupBy(x => x._1).map(x => x._2.reduce((y, z) => (y._1, (y._2 + z._2))))
  }
  
  private lazy val invertMap = (Function chain Seq.fill(iter - 1)(invertLoop(_)))(elemDiscreteMap)
  def invert(y:A) = invertMap get y // not implemented
  
  override def toDiscreteMap = invertMap
}

object PrExp {
}