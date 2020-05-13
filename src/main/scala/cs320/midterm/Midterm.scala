package cs320

trait Midterm extends Homework {
  sealed trait Expr
  case class Num(num: Int) extends Expr
  case class Add(left: Expr, right: Expr) extends Expr
  case class Sub(left: Expr, right: Expr) extends Expr
  case class Val(name: String, value: Expr, body: Expr) extends Expr  //     | {val x=e;e}
  case class Id(name: String) extends Expr                            //     | x
  case class App(func: Expr, args: List[Expr], namedArgs: List[(String, Expr)]) extends Expr           //     | e(e,...,e)
  case class Fun(params: List[String], body: Expr) extends Expr       //     | {(x,...,x)=>e}
  case class Rec(items: RecItems) extends Expr                 //     | {x=e,...,x=e}
  case class Acc(expr: Expr, name: String) extends Expr               //     | e.x

  trait RecItems
  case class Starred(rec: List[Expr]) extends RecItems
  case class KeyVal(rec: Map[String, Expr]) extends RecItems

  // Record map
  type RecMap = Map[String, Value]

  // Value type
  trait Value
  case class NumV(n: Int) extends Value
  case class CloV(params: List[String], body: Expr, env: Env) extends Value
  case class RecV(map: RecMap) extends Value

  // Environment
  type Env = Map[String, Value]

  def interp(expr: Expr, env: Env): Value

  def toStr(e: Value): String = e match {
    case NumV(n) => n.toString
    case RecV(map) => "{" + map.map(e => e._1 + " = " + toStr(e._2)).mkString(", ") + "}"
    case _: CloV => "function"
  }

  def run(e: Expr): String = tostring(interp(e, Map.empty))
}
