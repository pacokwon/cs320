package cs320

package object ex05 extends Exercise05 {
  def numOp(op: (Int, Int) => Int): (Value, Value) => Value =
    (_, _) match {
      case (NumV(x), NumV(y)) => NumV(op(x, y))
      case (x, y) => error("invalid operation")
    }

  val numAdd = numOp(_ + _)
  val numSub = numOp(_ - _)
  def appHelper(func: Value, args: List[Value]): Value =
    func match {
      case CloV(params, body, env) =>
        if (params.length == args.length)
          interp(body, env ++ (params zip args))
        else
          error("wrong arity")
      case _ => error(s"wrong type: $func is not CloV")
    }

  def accHelper(rec: Value, name: String): Value =
    rec match {
      case RecV(map) => map.getOrElse(name, error("no such field"))
      case _ => error(s"wrong type: $rec is not RecV")
    }

  def interp(expr: Expr, env: Env): Value =
    expr match {
      case Num(num) => NumV(num)
      case Add(left, right) => numAdd(interp(left, env), interp(right, env))
      case Sub(left, right) => numSub(interp(left, env), interp(right, env))
      case Val(name, value, body) => interp(body, env + (name -> interp(value, env)))
      case Id(name) => env.getOrElse(name, error(s"free identifier $name"))
      case Fun(params, body) => CloV(params, body, env)
      case App(func, args) => appHelper(interp(func, env), args.map(arg => interp(arg, env)))
      case Rec(rec) => RecV(rec.map(r => (r._1 -> interp(r._2, env))))
      case Acc(expr, name) => accHelper(interp(expr, env), name)
    }

  def tests: Unit = {
    test(run("{ (x, y) => (x + y) }(1, 2)"), "3")
    test(run("{ () => (3 + 4) }()"), "7")
    testExc(run("{ (x, y) => (x + y) }(1)"), "wrong arity")
    test(run("{ x = 1, y = 2 }.x"), "1")
    testExc(run("{ x = 1, y = 2 }.z"), "no such field")
    testExc(run("{ x = { y = 1 }.z }"), "no such field")
    test(run("42"), "42")
    test(run("{ x => x }"), "function")
    test(run("{ x = 1 }"), "record")

    /* Write your own tests */
  }
}
