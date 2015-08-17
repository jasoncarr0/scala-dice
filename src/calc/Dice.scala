package calc

/**
 * Dice.scala
 * Contains operations on Int PrExps to construct dice from numerical
 * or string input
 * 
 * @author Jason Carr
 */

object Dice {
  def apply(n: Int): PrExp[Int] = {
    new PrSel[Int]((p) => PrVal(1 + (p * n) toInt))
  }
  
  def parseString(s: String):Option[PrExp[Int]] = {
      val parens = "\\((.*)\\)".r
      val parenparsee = parens findFirstMatchIn s
    
      val patterns = Seq(
          "(.*)\\s*(\\+|-)\\s*([^\\+-]*)".r,
          "(.*)\\s*(\\*|/)\\s*([^\\*/]*)".r,
          "(.*)\\s*(d)\\s*([^d]*)".r)
      for (pattern <- patterns) {
        val parsed = pattern findFirstMatchIn s
        for (m <- parsed) {
          //println(m.group(1) + " || " + m.group(2) + " || " + m.group(3))
          return for (
            x <- parseString(m.group(1));
            y <- parseString(m.group(3));
            op:((PrExp[Int], PrExp[Int]) => PrExp[Int]) = {
              //println(m.group(2).charAt(0));
              m.group(2).charAt(0) match {
              case '+' => (_+_)
              case '-' => (_-_)
              case '*' => (_*_)
              case '/' => ((x, y) => for (a <- x; b <- y) yield (a / b))
              case 'd' => ((x, y) => for (a <- x; b <- y; c <- Dice(b) *+ a) yield (c))
              }}
          ) yield (op(x, y))
        }
      }
      if (s.trim.size == 0)
        return Some(PrVal(1))
      try {
        return Some(PrVal(s.trim.toInt))
      } catch {
        case e: Exception => None
      }
  }
}