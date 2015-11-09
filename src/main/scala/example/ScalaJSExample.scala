package example

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import scala.scalajs.js.timers._


object ScalaJSExample extends js.JSApp {

  def echo(s: String) : Unit = {
    println(s);
    def echoLine(s: String) = {
      val paragraph = dom.document.createElement("p")
      paragraph.innerHTML = s"<strong>$s</strong>"
      dom.document.getElementById("playground").appendChild(paragraph)
    }

    s.split('\n').foreach(echoLine)
  }

  def main(): Unit = {
    val rand = new scala.util.Random() 
    
    val specials = Stream(Double.PositiveInfinity, Double.NegativeInfinity,
      Double.MaxValue, Double.MinValue, Double.MinPositiveValue, Double.NaN, 0)
    
    val ints = (1 to 1000) map ( _ => rand.nextInt.toDouble)

    val powers = (-1025 to 1025) map ( pow => rand.nextGaussian * math.pow(2, pow))

    val randoms = (1 to 1000) map ( _ => rand.nextDouble)
   
    val doubles = specials ++ ints ++ powers ++ randoms
   
    println("Generated doubles");
    setTimeout(0){
      val success = for(d <- doubles) yield {
        val mine = toStringDouble(d)
        val ref = d.toString
        if(mine != ref){
          println("nks : " + getNKS(d))
          println("realnks : " + realNKS(d))
          echo(s"Mine : $mine should be \nRef : $ref\n");
          None
        } else { 
          Some(d)
        }
      }
    
      val successCount = success.count(_.isDefined)
      echo(s"Succeeded ${successCount} out of ${doubles.length}.")
      //success.collect{ case Some(t) => t}.foreach(println)
    }
  }

  def getNKS(m : Double) : (Int, Int, BigInt) = {
    val n = math.ceil(math.log10(m)).toInt
    
    val decimal = BigDecimal.decimal(m)
    val k = decimal.precision
    val s = (decimal / BigDecimal(10).pow(n - k)).toBigInt

    (n, k, s)
  }

  def realNKS(m: Double) : (Int, Int, BigInt) = {
    val sString = math.abs(m).toString.takeWhile(_ != 'e').filter(_ != '.').dropWhile(_ == '0')
    val s = BigInt(sString)
    val k = sString.length
    val n =  math.ceil(math.log10(m)).toInt

    (n, k, s)
  }

  def toStringDouble(m: Double): String = m match {
    case n if n.isNaN => "NaN"
    case 0 => "0"
    case m if m < 0 => "-" + toStringDouble(-m)
    case i if i.isInfinity => "Infinity"
    case _ => {
      val (n, k, s) = getNKS(m)
      if( k <= n && n <= 21 ) {
        s.toString
      } else if (0 < n && n <= 21) {
        val (int, part) = s.toString.splitAt(n)
        int + "." + part
      } else if (-6 < n && n <= 0){
        "0." + ("0" * -n) + s.toString
      } else if ( k == 1 ){
        val exp = n - 1;
        s.toString + "e" + (if(exp < 0) "-" else "+") + math.abs(exp).toInt.toString
      } else {
        val (head, rest) = s.toString.splitAt(1)
        val exp = n - 1
        head + '.' + rest + "e" + (if(exp < 0) "-" else "+") + math.abs(exp).toInt.toString
      }
    }
  }
}
