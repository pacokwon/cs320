package cs320

package object midterm extends Midterm {
  def binOp(op: (Int, Int) => Int): (Value, Value) => Value =
    (_, _) match {
      case (NumV(l), NumV(r)) => NumV(op(l, r))
      case (_, _) => error("Error!")
    }

  def appHelper(func: Value, args: List[Value], namedArgs: List[(String, Value)]): Value =
    func match {
      case CloV(params, body, env) =>
        if (params.length == args.length + namedArgs.length)
          interp(body, env ++ (params zip args) ++ namedArgs)
        else
          error("wrong arity")
      case _ => error(s"wrong type: $func is not CloV")
    }

  def unpack(rec: List[Value], env: Env): RecMap = {
    rec.foldLeft(Map.empty[String, Value])((acc, cur) => acc ++ (cur match {
      case RecV(map) => map
      case default => error(s"Failed unpack. $default is not RecV!")
    }))
  }

  def interp(expr: Expr, env: Env): Value = {
    expr match {
      case Num(num) => NumV(num)
      case Add(left, right) => binOp(_ + _)(interp(left, env), interp(right, env))
      case Sub(left, right) => binOp(_ - _)(interp(left, env), interp(right, env))
      case Val(name, value, body) => interp(body, env + (name -> interp(value, env)))
      case Id(name) => env.getOrElse(name, error(s"Free Identifier $name!"))
      case App(func, args, namedArgs) =>
        appHelper(
          interp(func, env),
          args.map(arg => interp(arg, env)),
          namedArgs.map(narg => ( narg._1, interp(narg._2, env) ))
        )
      case Fun(params, body) => CloV(params, body, env)
      case Rec(items) => items match {
        case KeyVal(rec) => RecV(rec.map(r => (r._1 -> interp(r._2, env))))
        case Starred(rec) => RecV(unpack(rec.map(r => interp(r, env)), env))
      }
      case Acc(expr, name) =>
        interp(expr, env) match {
          case RecV(map) => map.getOrElse(name, error(s"$name is not member of RecV"))
          case _ => error(s"wrong type: not RecV")
        }
    }
  }

  def tests: Unit = {
    // =================== Named Arguments  ===================
    test(
      interp(
        App(
          Fun(
            List("x", "y", "z"),
            Sub(Add(Id("x"), Id("y")), Id("z"))
          ),
          List(Num(1), Num(2), Num(3)),
          List()
        ), Map.empty
      ),
      NumV(0)
    ) // { (x, y, z) => ((x + y) - z) }(1, 2, 3)

    test(
      interp(
        App(
          Fun(
            List("x", "y", "z"),
            Sub(Add(Id("x"), Id("y")), Id("z"))
          ),
          List(Num(1)),
          List(("y", Num(2)), ("z", Num(10)))
        ), Map.empty
      ),
      NumV(-7)
    ) // { (x, y, z) => ((x + y) - z) }(1, y=2, z=10)

    test(
      interp(
        App(
          Fun(
            List("x", "y", "z"),
            Sub(Add(Id("x"), Id("y")), Id("z"))
          ),
          List(),
          List(("z", Num(3)), ("y", Num(2)), ("x", Num(1)))
        ), Map.empty
      ),
      NumV(0)
    ) // { (x, y, z) => ((x + y) - z) }(z=3, y=2, x=1)

    testExc(
      interp(
        App(
          Fun(
            List("x", "y", "z"),
            Sub(Add(Id("x"), Id("y")), Id("z"))
          ),
          List(Num(1)),
          List(("y", Num(2)))
        ), Map.empty
      ),
      "wrong arity"
    ) // { (x, y, z) => ((x + y) - z) }(1, y=2)

    testExc(
      interp(
        App(
          Fun(
            List("x", "y", "z"),
            Sub(Add(Id("x"), Id("y")), Id("z"))
          ),
          List(Num(1)),
          List(("x", Num(2)), ("y", Num(3)))
        ), Map.empty
      ),
      "Free Identifier z!"
    ) // { (x, y, z) => ((x + y) - z) }(1, x=2, y=3)

    // =================== Record Unpacking  ===================
    test(
      run(
        Val("rec1", Rec(KeyVal(Map("x"->Num(1), "y"->Num(2)))),
          Val("rec2", Rec(KeyVal(Map("z"->Num(3), "w"->Num(4)))),
            Rec(Starred(
              List(Id("rec1"), Id("rec2"))
            ))
          )
        )
      ),
      "{x = 1, y = 2, z = 3, w = 4}"
    )
    /*
    {
      val rec1 = { x = 1, y = 2 };
      {
        val rec2 = { z = 3, w = 4 };
        { **rec1, **rec2 }
      }
    }
    */

    test(
      run(
        Val("rec1", Rec(KeyVal(Map("x"->Num(1), "y"->Num(2)))),
          Val("rec2", Rec(KeyVal(Map("z"->Num(3), "w"->Num(4)))),
            Rec(Starred(
              List(Id("rec1"), Id("rec2"), Rec(KeyVal(Map("x"->Num(10), "z"->Num(30)))))
            ))
          )
        )
      ),
      "{x = 10, y = 2, z = 30, w = 4}"
    )
    /*
    {
      val rec1 = { x = 1, y = 2 };
      {
        val rec2 = { z = 3, w = 4 };
        { **rec1, **rec2, **{ x = 10, z = 30 } }
      }
    }
    */

    test(
      run(
        Val("rec1", Rec(KeyVal(Map("x"->Num(1), "y"->Num(2)))),
          Val("rec2", Rec(KeyVal(Map("z"->Num(3), "w"->Num(4)))),
            Rec(Starred(
              List(Id("rec1"), Rec(KeyVal(Map("x"->Num(10), "z"->Num(30)))), Id("rec2"))
            ))
          )
        )
      ),
      "{x = 10, y = 2, z = 3, w = 4}"
    )
    /*
    {
      val rec1 = { x = 1, y = 2 };
      {
        val rec2 = { z = 3, w = 4 };
        { **rec1, **{ x = 10, z = 30 }, **rec2 }
      }
    }
    */
  }
}
