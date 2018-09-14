import scala.annotation.tailrec
import scala.util.matching.Regex

object Main
{

  object ModMath
  {
    val PRIME: Int = 1000000007

    def add(a: Int, b: Int): Int = (a + b) % PRIME

    def sub(a: Int, b: Int): Int = (a - b + PRIME) % PRIME

    def mul(a: Int, b: Int): Int = ((a.asInstanceOf[Long] * b.asInstanceOf[Long]) % PRIME).asInstanceOf[Int]

    def neg(a: Int): Int = (PRIME - a) % PRIME

    def pow(a: Int, b: Int): Int = repsqr(a, b, 1)

    def inv(a: Int): Int = pow(a, PRIME - 2)

    def div(a: Int, b: Int): Int = mul(a, inv(b))

    @tailrec
    private def repsqr(a: Int, b: Int, c: Int): Int = b match
    {
      case 0 => c
      case 1 => mul(a, c)
      case _ => b & 1 match
      {
        case 0 => repsqr(mul(a, a), b / 2, c)
        case _ => repsqr(mul(a, a), b / 2, mul(a, c))
      }
    }
  }

  def regexStrip(reg: String): Regex =
  {
    ("^" + reg + """\s*(.*)""").r
  }

  def regexStripCapture(reg: String): Regex =
  {
    ("^(" + reg + """)\s*(.*)""").r
  }

  val open: Regex = regexStrip("\\(")

  val close: Regex = regexStrip("\\)")

  val plus: Regex = regexStrip("\\+")

  val minus: Regex = regexStrip("-")

  val times: Regex = regexStrip("\\*")

  val divide: Regex = regexStrip("/")

  val number: Regex = regexStripCapture("[0-9]{1,10}")

  case class ParserException(what: String) extends Exception

  object Parser
  {

    def parse(input: String): Int =
    {
      val strip = """^\s*(.*)\s*$""".r
      val string = input match
      {
        case strip(rest) => rest
        case _ => ""
      }
      expression(string)._1
    }

    private def expression(string: String): (Int, String) =
    {
      val left = term(string)
      expressionOp(left)
    }

    private def term(string: String): (Int,String) =
    {
      val left = factor(string)
      (termOp(left)._1, left._2)
    }

    private def expressionOp(left: (Int, String)): (Int,String) =
    {
      left._2 match
      {
        case plus(rest) => val right = expression(rest)
          (ModMath.add(left._1, right._1),right._2)
        case minus(rest) => val right = expression(rest)
          (ModMath.sub(left._1, right._1),right._2)
        case _ => left
      }
    }

    private def termOp(left: (Int, String)): (Int, String) =
    {
      left._2 match
      {
        case times(rest) => val right = term(rest)
          (ModMath.mul(left._1, right._1), right._2)
        case divide(rest) => val right = term(rest)
          (ModMath.div(left._1, right._1), right._2)
        case _ => left
      }
    }

    private def factor(string: String):(Int, String) =
    {
      string match
      {
        case number(num, rest) => (num.toInt, rest)
        case plus(rest) => factor(rest)
        case minus(rest) => val fac = factor(rest)
          (ModMath.neg(fac._1), fac._2)
        case open(rest) =>
          val expr = expression(rest)
          closeBracket(expr)
        case _ => throw ParserException("unexpected symbol, got: " + string)
      }
    }

    private def closeBracket(expr: (Int, String)): (Int,String) =
    {
      expr._2 match
      {
        case close(rest) => (expr._1,rest)
        case _ => throw ParserException("expected closing bracket, got: " + expr._2)
      }
    }
  }

  def main(args: Array[String]): Unit =
  {
    println(Parser.parse("4/-2/(2 + 8)"))
  }
}
