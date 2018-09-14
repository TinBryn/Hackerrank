import scala.util.matching.Regex

object Simplify
{
  /*
  the language is an algebraic expression with the variable x
  tokens include numbers, x, (, ), +, -, *, /, ^
   */

  def regMod(reg: String): Regex =
  {
    (reg + """\s*(.*+)""").r
  }

  val open: Regex = regMod("\\(")
  val close: Regex = regMod("\\)")
  val plus: Regex = regMod("\\+")
  val minus: Regex = regMod("-")
  val times: Regex = regMod("\\*")
  val divide: Regex = regMod("/")
  val pow: Regex = regMod("\\^")
  val symbol: Regex = regMod("x")
  val number: Regex = """(\d+)\s*(.*+)""".r

  object Trees
  {
    def bracketFactor(tree: Tree): String = tree match
    {
      case PlusTree(_, _) | MinusTree(_, _) => "(" + tree.toString + ")"
      case NumberTree(_) | SymbolTree() |TimesTree(_, _) | DivideTree(_, _) | PowerTree(_, _) => tree.toString
    }

    def bracketExpression(tree: Tree): String = tree match
    {
      case PlusTree(_, _) | MinusTree(_, _) |TimesTree(_, _) | DivideTree(_, _)  => "(" + tree.toString + ")"
      case NumberTree(_) | SymbolTree() | PowerTree(_, _) => tree.toString
    }

    sealed trait Tree

    case class NumberTree(int: Int) extends Tree
    {
      override def toString: String = int.toString
    }

    case class SymbolTree() extends Tree
    {
      override def toString: String = "x"
    }

    case class PlusTree(left: Tree, right: Tree) extends Tree
    {
      override def toString: String = left.toString + "+" + right.toString
    }

    case class MinusTree(left: Tree, right: Tree) extends Tree
    {
      override def toString: String = left.toString + "-" + right.toString
    }

    case class TimesTree(left: Tree, right: Tree) extends Tree
    {
      override def toString: String = bracketFactor(left) + "*" + bracketFactor(right)
    }

    case class DivideTree(left: Tree, right: Tree) extends Tree
    {
      override def toString: String = bracketFactor(left) + "/" + bracketFactor(right)
    }

    case class PowerTree(left: Tree, right: Tree) extends Tree
    {
      override def toString: String = bracketExpression(left) + "^" + bracketExpression(right)
    }
  }

  import Trees._

  case class ParserException(what: String) extends Exception


  def main(args: Array[String]): Unit =
  {
    val a = SymbolTree()
    val b = NumberTree(5)
    val c = PowerTree(a, b)
    val d = NumberTree(2)
    val e = TimesTree(d, c)

    println(e)
  }

}
