package calc

object CmpUt {
  def cond[T](cond: => Boolean, succ: => T, fail: => T):T = if (cond) (succ) else (fail)
}