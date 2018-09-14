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

  case class Parser()
  {
    var string = ""

    def parse(input: String): Int =
    {
      val strip = """\s*(.*)""".r
      string = input match
      {
        case strip(rest) => rest
        case _ => ""
      }
      expression()
    }

    def expression(): Int =
    {
      val left = term()
      expressionOp(left)
    }

    def term(): Int =
    {
      val left = factor()
      termOp(left)
    }

    def expressionOp(left: Int): Int =
    {
      string match
      {
        case plus(rest) => string = rest
          val right = expression()
          ModMath.add(left, right)
        case minus(rest) => string = rest
          val right = expression()
          ModMath.sub(left, right)
        case _ => left
      }
    }

    def termOp(left: Int): Int =
    {
      string match
      {
        case times(rest) => string = rest
          val right = term()
          ModMath.mul(left, right)
        case divide(rest) => string = rest
          val right = term()
          ModMath.div(left, right)
        case _ => left
      }
    }

    def factor():Int =
    {
      string match
      {
        case number(num, rest) => string = rest
          num.toInt
        case plus(rest) => string = rest
          factor()
        case minus(rest) => string = rest
          ModMath.neg(factor())
        case open(rest) => string = rest
          val expr = expression()
          closeBracket(expr)
        case _ => throw ParserException("unexpected symbol, got: " + string)
      }
    }

    def closeBracket(expr: Int): Int =
    {
      string match
      {
        case close(rest) => string = rest
          expr
        case _ => throw ParserException("expected closing bracket, got: " + string)
      }
    }
  }

  def main(args: Array[String]): Unit =
  {
    val parser = Parser()
    println(parser.parse("4/-2/(2 + 8)"))
  }
}
