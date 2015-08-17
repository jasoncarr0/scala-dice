package calc

/**
 * Statistics.scala
 * Class for generating simple statistics (n, mean, variance) out of a PrExp
 * and operating on that statistics data
 * 
 * @author Jason Carr
 */


//n is number of elements, sum is sum of these elements = mean*n, nsvar = n^2 * variance
class Statistics[T](val n:Int, val sum:T, val nsvar:T)(implicit val numInst:Fractional[T]) {
}
object Statistics {
  def mergeStat[T](st1:Statistics[T], st2:Statistics[T]):Statistics[T] = {
    val n_new = st1.n + st2.n;
    val nums = st1.numInst;
    val sum_new = nums.plus(st1.sum, st2.sum)
    // merge the n^2 * variance
    // this is a lot of things, nsvar_new = (n_new) * (sum (nsvar + sum^2)/n) - sum_new^2
    val nvar_new = nums.minus(nums.times(nums.plus(
      nums.div(nums.plus(st1.nsvar, nums.times(st1.sum, st1.sum)), nums.max(nums.fromInt(st1.n), nums.fromInt(0))),
      nums.div(nums.plus(st2.nsvar, nums.times(st2.sum, st2.sum)), nums.max(nums.fromInt(st2.n), nums.fromInt(0)))
    ), nums.fromInt(n_new)), nums.times(sum_new, sum_new));
    return new Statistics(n_new, sum_new, nvar_new)(nums);
  }
  
  
  // this is not efficient for large imputs
  // It's still O(n), 1000000 times a small PrExp runs in a few hundred millis on my comp
  // If improvement is necessary, the data can be collected in one buffer/chunked in several
  // Then statistics gathered, then merged if necessary
  def genStat[T](exp:PrExp[T], times:Int)(implicit num:Fractional[T]):PrExp[Statistics[T]] = {
    val base = PrFun(exp, ((x:T) => PrVal(new Statistics(1, x, num.fromInt(0))(num))));
    return PrHylo(times, base, mergeStat)
  }
}