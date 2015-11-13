import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import scala.scalajs.js.timers._
import JSNumber.test
import JSNumber.p

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


  implicit class DoubleElements(value : Double) {
    val bits = scala.scalajs.runtime.Bits.doubleToLongBits(value)
    val exponent = (bits & 0x7ff0000000000000L) >> 52
    val mantissa= bits & 0x000fffffffffffffL

    val realExponent = exponent - 1023
    
    val bigIntOpt = if (realExponent < 52) None else Some{
      BigInt((1L << 52) + mantissa) << (realExponent - 52).toInt
    }
  }

  def getInfo(problem: Double, s: String){
    val realS = problem.toString
    if(realS != s){
      println(s"Real:               ${realS}")
      println(s"Mine:               ${s}")
      println(s"Mantissa:           ${problem.mantissa}")
      println(s"Exponent:           ${problem.exponent}")
      println(s"Real exponent:      ${problem.realExponent}")
      println(s"Binary mantissa:    ${problem.mantissa.toBinaryString}")
      println(s"Binary exponent:    ${problem.exponent.toBinaryString}")
      println(s"Big Int opt:        ${problem.bigIntOpt}")
      println(s"Real Big Int:       ${BigInt(realS)}")
      println(s"Mine Big Int:       ${BigInt(s)}")
      println(s"Real Big Int bin:   ${BigInt(realS).toString(2)}")
      println(s"Mine Big Int bin:   ${BigInt(s).toString(2)}")
      problem.bigIntOpt.foreach { bi =>
        println(s"Binary Int opt bin: ${bi.toString(2)}")
      }
      println(s"53:                 ${" " * 52}^")
      println("")
    } else {
      println(s"!!!!!! NO PROBLEM !!!!!! -> $problem")
      println("")
    }
  }

  
def main(): Unit = {  
      var passed = 0    

      // Special cases
      passed += test(0.0)
      passed += test(-0.0)
      passed += test(Double.NaN)
      passed += test(Double.PositiveInfinity)
      passed += test(Double.NegativeInfinity)
      
      // k <= n <= 21
      passed += test(1.0)
      passed += test(12.0)
      passed += test(123.0)
      passed += test(1234.0)
      passed += test(12345.0)
      passed += test(123456.0)
      passed += test(1234567.0)
      passed += test(12345678.0)
      passed += test(123456789.0)
      passed += test(1234567890.0)
      passed += test(12345678901.0)
      passed += test(123456789012.0)
      passed += test(1234567890123.0)
      passed += test(12345678901234.0)
      passed += test(123456789012345.0)
      passed += test(1234567890123456.0)
      passed += test(12345678901234657.0)
      passed += test(123456789012345678.0)
      //passed += test(1234567890123456789.0)
      passed += test(12345678901234567890.0)
      passed += test(123456789012345678901.0)

      // 0 < n <= 21
      passed += test(1.42)
      passed += test(12.42)
      passed += test(123.42)
      passed += test(1234.42)
      passed += test(12345.42)
      passed += test(123456.42)
      passed += test(1234567.42)
      passed += test(12345678.42)
      passed += test(123456789.42)
      passed += test(1234567890.42)
      passed += test(12345678901.42)
      passed += test(123456789012.42)
      passed += test(1234567890123.42)
      passed += test(12345678901234.42)
      passed += test(123456789012345.42)
      passed += test(1234567890123456.42)
      passed += test(12345678901234657.42)
      passed += test(123456789012345678.42)
      //passed += test(1234567890123456789.42)
      passed += test(12345678901234567890.42)
      passed += test(123456789012345678901.42)

      // -6 < n <= 0
      passed += test(0.1)
      passed += test(0.01)
      passed += test(0.001)
      passed += test(0.0001)
      passed += test(0.00001)
      passed += test(0.000001)

      // k == 1
      passed += test(1e22)
      passed += test(2e25)
      passed += test(3e50)
      passed += test(4e100)
      passed += test(5e200)
      passed += test(6e300)
      passed += test(7e307)
      passed += test(1e-22)
      passed += test(2e-25)
      passed += test(3e-50)
      passed += test(4e-100)
      passed += test(5e-200)
      passed += test(6e-300)
      passed += test(7e-307)

      // else
      passed += test(1.42e22)
      passed += test(2.42e25)
      passed += test(3.42e50)
      passed += test(4.42e100)
      passed += test(5.42e200)
      passed += test(6.42e300)
      passed += test(7.42e307)
      passed += test(1.42e-22)
      passed += test(2.42e-25)
      passed += test(3.42e-50)
      passed += test(4.42e-100)
      passed += test(5.42e-200)
      passed += test(6.42e-300)
      passed += test(7.42e-307)  
   
      println(s"Total test passed : $passed")

      // Doesn't work
      var problems = Seq(
        p(18271179521433728.0),
        p(1.15292150460684685E18),
        p(1234567890123456770.0),
        p(2234567890123456770.0),
        p(4234567890123450000.0),
        p(149170297077708820000.0),
        p(296938164846899230000.0),
        p(607681513323520000000.0)
      )

      problems.foreach((getInfo _).tupled)
      // 54-72
      val samples = JSNumber.getRandomSample(100, 54) ++
      JSNumber.getRandomSample(100, 55) ++ 
      JSNumber.getRandomSample(100, 56) ++
      JSNumber.getRandomSample(100, 57) ++ 
      JSNumber.getRandomSample(100, 58) ++
      JSNumber.getRandomSample(100, 59) ++ 
      JSNumber.getRandomSample(100, 60) ++
      JSNumber.getRandomSample(100, 61) ++ 
      JSNumber.getRandomSample(100, 62) ++
      JSNumber.getRandomSample(100, 63) ++ 
      JSNumber.getRandomSample(100, 64) ++ 
      JSNumber.getRandomSample(100, 65) ++ 
      JSNumber.getRandomSample(100, 66) ++
      JSNumber.getRandomSample(100, 67) ++ 
      JSNumber.getRandomSample(100, 68) ++
      JSNumber.getRandomSample(100, 69) ++ 
      JSNumber.getRandomSample(100, 70) ++
      JSNumber.getRandomSample(100, 71) ++ 
      JSNumber.getRandomSample(100, 72) 

      val (correct, wrong) = samples.partition{ case (num, string) => num.toString == string }
      
      wrong.foreach { case (num, string) =>
        println(s"Problem with $string, expected : ${num.toString}")
        println("Wanted: " + BigInt(num.toString).toString(2))
        println("Got:    " + BigInt(string).toString(2))
        println(BigInt(string).toString(2).length)
        println()
      }

      def dropZeros(s: String) : String = s.reverse.dropWhile(_=='0').reverse

      val ks = wrong.map { case (num, string) => 
        val myK = dropZeros(string).length
        val trueK = dropZeros(num.toString).length
        (myK, trueK).toString
      }

      ks.toSeq.distinct.foreach(println)


      println(s"Got ${correct.size} out of ${correct.size + wrong.size}")
  }
}
