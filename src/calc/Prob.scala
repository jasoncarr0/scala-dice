package calc

/**
 * Prob.scala
 * Includes the PrExp class, a class for supporting streamed random data
 * and performing monadic operations on it
 * 
 * @author Jason Carr
 */

import scala.util.Random;

// Not sealed to allow implementations for other features
// While subclassing will never be necessary, more efficient implementations 
// are a possibility (ex. PrHylo instead of iterations)
abstract class PrExp[T] {
  def roll(implicit r: () => Double): T
  
  def map[U](f: (T) => U): PrExp[U] = PrFun(this,((new PrVal(_:U):PrVal[U]) compose f))
  def foreach[U](f: (T) => U)(implicit r: () => Double):Unit = f(this roll r)
  def flatMap[U](f: (T) => PrExp[U]): PrExp[U] = PrFun(this, f)
  
  def +(other:PrExp[T])(implicit num:Numeric[T]):PrExp[T] = for (x <- this; y <- other) yield (num.plus(x, y))
  def -(other:PrExp[T])(implicit num:Numeric[T]):PrExp[T] = for (x <- this; y <- other) yield (num.minus(x, y))
  def *(other:PrExp[T])(implicit num:Numeric[T]):PrExp[T] = for (x <- this; y <- other) yield (num.times(x, y))
  def /(other:PrExp[T])(implicit num:Fractional[T]):PrExp[T] = for (x <- this; y <- other) yield (num.div(x, y))
  def *+(times:Int)(implicit num:Numeric[T]):PrExp[T] = PrHylo(times, this, num.plus);//Function.chain(Seq.fill(times - 1)(this+_))(this)
}
case class PrFun[S,T](x: PrExp[S], f:(S => PrExp[T])) extends PrExp[T]{
  def roll(implicit r: () => Double) = f(x roll r) roll r
}
case class PrVal[T](x: T) extends PrExp[T]{
  def roll(implicit r: () => Double) = x
}
case class PrSel[T](sel: Double => PrExp[T]) extends PrExp[T]{
  def roll(implicit r: () => Double) = sel(r()) roll r
}
case class PrHylo[T](iter: Int, elem: PrExp[T], fold: (T, T) => T) extends PrExp[T] {
  def roll(implicit r: () => Double): T = {
    var sum:T = elem roll r;
    // loops one less due to first roll
    for (i <- 1 until iter) {
      sum = fold((elem roll r):T, sum:T);
    }
    sum;
  }
}

object PrExp {
  
}