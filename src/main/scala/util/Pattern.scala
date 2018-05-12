package util

object Pattern {

  def main(args: Array[String]): Unit = {
    //CASE CLASSES
    //adds factory method
    val v = Var("x")
    val op = BinOp("+", Number(1), v)

    //arguments get implicitly val prefix
    val name = v.name

    //compiler adds toString, equal, hashCode
    //println(op)

    //adds Copy method
    val opC = op.copy(operator = "-")

    //pattern matching
    println(simplifyTop(UnOp("-", UnOp("-", Var("x")))))

    println(describe(Number(40)))

    val capitals = Map("France" -> "Paris", "Japan" -> "Tokyo")
    show(capitals get "Japan")

    val withDefault: Option[Int] => Int = {
      case Some(x) => x
      case None => 0
    }

    withDefault(Some(0))

    val secondElement: List[Int] => Int = {
      case x :: y :: _ => y
      case _ => 0
    }
  }

  def show(x: Option[String])=x match {
    case Some(s) => s
    case None => "?"
  }


  def simplifyAdd(expr: Expr) = expr match {
    case BinOp("+", x, y) if x == y => BinOp("*", x, Number(2))
    case _ => expr
  }

  def simplifyTop(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => {println("here"); e}
    case BinOp("+", e, Number(0)) => e
    case BinOp("+", e, Number(1)) => e
    case _ => expr
  }

  def describe(expr: Expr) = (expr: @unchecked) match {
    case Number(_) => "a number"
    case Var(_) => "a variable"
  }

  sealed abstract class Expr
  case class Var(name:String) extends Expr
  case class Number(num:Double) extends Expr
  case class UnOp(operator: String, arg:Expr) extends Expr
  case class BinOp(operator:String, left: Expr, right: Expr) extends Expr


}
