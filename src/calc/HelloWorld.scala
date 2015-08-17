package calc

import scala.collection.Seq;
import scala.util.Random

object HelloWorld{
  def main(args: Array[String]) {
    implicit val rand:() => Double = (new Random).nextDouble
    
    val sdie = Dice(6).map(Seq(_));
    var iteration = 0;
    //target
    for (tgt <- 2 until 8) {
      for (stat:Int <- 0 until 7) {
        for (numdice <- 1 until 7) {
          val dice = for (xs <- PrHylo(numdice, sdie, Seq.concat(_:Seq[Int], _:Seq[Int]))) yield (xs.sorted.reverse)
          //for a hack below
          var sum:Int = 0;
          val length = for (xs <- dice;
              //hacks, make sure sum is reset inside the PrExp block
              //so that it resets for each roll
              val ignorethishack = {sum = 0;};
              //update sum inside a block, then return it
              val zs = for (x <- xs; if x >= tgt || ((stat:Int) >= ({sum += (tgt - x); sum}))) yield x
          ) yield (zs.length.toDouble)
          
          
          val stats = Statistics.genStat(length, 100000);
          for (output <- stats) {
            printf("%.4f n:%d x:%d s:%d", output.sum/output.n, numdice, tgt, stat)
            if (iteration % 6 == 5) 
              printf("\n\n");
            else
              printf("|");
            iteration += 1;
          }
        }
      }
        
    }
    for (ln <- io.Source.stdin.getLines) {
      val (command, dice) = ln.trim.span(c => !c.isWhitespace)
      command.toLowerCase() match {
        case "roll" => {
          println(Dice.parseString(dice).getOrElse(new PrVal("Invalid dice")).roll)
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