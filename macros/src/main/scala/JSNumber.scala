
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object JSNumber {

  def getLiteral[@specialized T](ctx: Context)(lit : ctx.Expr[T]) : T = {
    import ctx.universe._
    reify{lit.splice}.tree match {
      case Literal(Constant(c)) => c.asInstanceOf[T]
      case _ => ctx.abort(ctx.enclosingPosition, "Should be a literal")
    }
  }

  def test(value: Double): Int = macro testImpl

  def getRandomSample(num: Int, exp: Int) : Map[Double, String] = macro getRandomSampleImpl 

  def getRandomSampleImpl(ctx: Context)(num: ctx.Expr[Int], exp: ctx.Expr[Int]): ctx.Expr[Map[Double, String]] = {
    import ctx.universe._ 
    
    val random = new scala.util.Random()
    val numSamples = getLiteral(ctx)(num)
    val exponent = getLiteral(ctx)(exp)

    val sampleTuples = for(i <- 1 to numSamples) yield {
      val preSample = random.nextDouble
      val bits = java.lang.Double.doubleToLongBits(preSample)
      val expClear = ~0x7fff000000000000L
      val expMask = ((exponent + 1023) & 0x7fff).toLong << 52
      val corrected = (bits & expClear) | expMask
      val sample = java.lang.Double.longBitsToDouble(corrected)
      sample.toString + "-> \"" + jsNumberToString(sample) + "\""
    }

    
    val samples = ctx.Expr(ctx.parse(sampleTuples.mkString("Map(", ",", ")")))

    reify{ samples.splice }
  }

  def testImpl(ctx: Context)(value: ctx.Expr[Double]): ctx.Expr[Int] =  {
    import ctx.universe._

    val string = toStringImpl(ctx)(value)
    val pos = ctx.enclosingPosition.line.toString 

    val posExpr = ctx.Expr(Literal(Constant(pos)))

    reify { 
      val real = value.splice.toString
      val fromJVM = string.splice
    
      if(fromJVM != real){
        println("Error at line " + posExpr.splice)
        println(s"expected : $real")
        println(s"got      : $fromJVM")
        println("")
        0
      } else {
        1
      }
    }
  }
  
  def staticToString(value: Double): String = macro toStringImpl

  def toStringImpl(ctx: Context)(value: ctx.Expr[Double]): ctx.Expr[String] = {
    import ctx.universe._
    val double = getLiteral(ctx)(value)
    val res = jsNumberToString(double)
    ctx.Expr(Literal(Constant(res)))
  }

  def p(value: Double) : (Double, String) = macro getPairImpl


  def getPairImpl(ctx: Context)(value : ctx.Expr[Double]): ctx.Expr[(Double, String)] = {
    import ctx.universe._
    val str = toStringImpl(ctx)(value)
    reify { (value.splice, str.splice) }
  }

  private def jsNumberToString(value: Double): String = {
    @inline def getJVMnks: (Int, Int, BigInt) = {
      
      // The JVM's to String works in most cases
      val jvmString = value.toString
      val n = math.floor(math.log10(value)).toInt + 1
      
      val sString = jvmString.filter(_ != '.').takeWhile(_.isDigit)

      // The JVM does love its .0
      val corrected = sString.stripSuffix("0")
      
      val s = BigInt(corrected)
      val k = corrected.length

      val bits = java.lang.Double.doubleToLongBits(value)
      val offsetExp = (bits & 0x7fff000000000000L) >> 52
      val exp = (offsetExp - 1023).toInt

      // Semantics are different only when ulp > 1 (exp > 53)
      // And js doesn't use scientific notation (n <= 21)
      if(exp >= 53 && n <= 21){
        val shorterS = (0 to (k - 13)) flatMap { i => 
          val part = s.toString.take(k - i)
          val one = part + ("0" * i)
          if(part.last < '9') {
            val two = part.init + (part.last + 1).toChar + ("0" * i)
            Seq(one, two)
          } else {
            Seq(one)
          }
        } map { BigInt(_) }
        
        val possibleS = shorterS filter { s =>
          (s * BigInt(10).pow(n-k)).toDouble == value
        } map {
          s => BigInt(s.toString.reverse.dropWhile(_ == '0').reverse)
        }

        val realK = possibleS.map(_.toString.length).min

        val realS = possibleS.filter(_.toString.length == realK).distinct
        
        val mantissa = (bits & 0x000fffffffffffffL) + (1L << 52) 
        val exactM = BigInt(mantissa) << (exp - 52)

        val distance = realS.map( s => (s * BigInt(10).pow(n-s.toString.length))
        - exactM).map(_.abs).min

        val closestS = realS.filter { s =>
          ((s * BigInt(10).pow(n-s.toString.length)) - exactM).abs == distance
        }

        val finalS = if(closestS.length == 1) closestS.head else closestS.filter(_ % 2 == 0).head

        (n, realK, finalS)
      } else {
        (n, k, s)
      }
    }

    if (1.0.toString == "1") { 
      // We are in a JS environment, my macro-foo failed me.
      "Macro-foo has failed"
    } else {
      value match {
        case _ if value.isNaN       => "NaN"
        case 0                      => "0"
        case _ if value < 0         => "-" + jsNumberToString(-value)
        case _ if value.isInfinity  => "Infinity"

        case _ => 
          val (n, k, s) = getJVMnks
          if (k <= n && n <= 21) {
            s.toString + ("0" * (n-k))
          } else if (0 < n && n <= 21) {
            val (int, part) = s.toString.splitAt(n)
            int + "." + part
          } else if (-6 < n && n <= 0) {
            "0." + ("0" * -n) + s.toString
          } else if (k == 1) {
            val exp = n - 1
            s.toString + "e" + (if(exp < 0) "-" else "+") + math.abs(exp).toInt.toString
          } else {
            val (head, rest) = s.toString.splitAt(1)
            val exp = n - 1
            head + '.' + rest + "e" + (if(exp < 0) "-" else "+") + math.abs(exp).toInt.toString
          }
      }
    }  
  }
}
