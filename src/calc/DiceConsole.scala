package calc

import scala.collection.Seq;
import scala.util.Random

object HelloWorld{
  def main(args: Array[String]) {
    implicit val rand:() => Double = (new Random).nextDouble
    
    val d = for (x <- PrEnum(Seq(1,4))) yield (x + 2);
    //println(d.enumerate)
    
    for (ln <- io.Source.stdin.getLines) {
      val (command, dice) = ln.trim.span(c => !c.isWhitespace)
      command.toLowerCase() match {
        case "roll" => {
          println(Dice.parseString(dice).getOrElse(new PrVal("Invalid dice")).roll)
        }
        case "enum" => {
          println(Dice.parseString(dice).getOrElse(new PrVal("Invalid dice")).enumerate)
        }
        case "invert" => {
          println(Dice.parseString(dice).getOrElse(new PrVal("Invalid dice")).toDiscreteMap)
        }
        case "stats" => {
          val d:PrExp[Int] = Dice.parseString(dice).getOrElse(new PrVal(0));
          val d2 = PrFun(d, (i:Int) => PrVal((i.toDouble)))
          val stats = Statistics.genStat(d2, 10000)
          val output = stats.roll;
          printf("%d, %f, %f\n", output.n, output.sum/output.n, Math.sqrt(output.nsvar/output.n)/output.n)
        }
        case _ => println("Usage: roll [dice]")
      }
    }
  }
}