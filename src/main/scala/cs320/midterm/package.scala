package cs320

package object midterm extends Midterm {
  def binOp(op: (Int, Int) => Int): (Value, Value) => Value =
    (_, _) match {
      case (NumV(l), NumV(r)) => NumV(op(l, r))
      case (_, _) => error("Wrong type: both operands must be NumV!")
    }

  def appHelper(func: Value, args: List[Value], namedArgs: Map[String, Value]): Value =
    func match {
      case CloV(params, body, env) =>
        if (!namedArgs.foldLeft(true)((contains, narg) => contains && params.contains(narg._1)))
          error("Unexpected named argument!")
        else if (!namedArgs.foldLeft(true)((contains, narg) => contains && params.indexOf(narg._1) != -1 && params.indexOf(narg._1) >= args.length ))
          error("Got both positional and named arguments for one parameter!")
        else if (params.length != args.length + namedArgs.size)
          error("Wrong arity!")
        else
          interp(body, env ++ (params zip args) ++ namedArgs)
      case _ => error(s"Wrong type: $func is not CloV!")
    }

  def unpack(rec: List[Value]): Map[String, Value] = {
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
      case Mul(left, right) => binOp(_ * _)(interp(left, env), interp(right, env))
      case Div(left, right) => binOp(_ / _)(interp(left, env), interp(right, env))
      case Val(name, value, body) => interp(body, env + (name -> interp(value, env)))
      case Id(name) => env.getOrElse(name, error(s"Free Identifier $name!"))
      case AppNamedArgs(func, args, namedArgs) =>
        appHelper(
          interp(func, env),
          args.map(arg => interp(arg, env)),
          namedArgs.map(narg => (narg._1 -> interp(narg._2, env)))
        )
      case AppStarredArgs(func, args, starredArgs) =>
        appHelper(
          interp(func, env),
          args.map(arg => interp(arg, env)),
          unpack(starredArgs.map(sarg => interp(sarg, env)))
        )
      case Fun(params, body) => CloV(params, body, env)
      case RecNamed(rec) => RecV(rec.map(r => (r._1 -> interp(r._2, env))))
      case RecStarred(rec) => RecV(unpack(rec.map(r => interp(r, env))))
      case Acc(expr, name) =>
        interp(expr, env) match {
          case RecV(map) => map.getOrElse(name, error(s"Missing field: $name is not member of RecV!"))
          case default => error(s"Wrong type: $default is not RecV!")
        }
    }
  }

  def tests: Unit = {
    // =================== Named Arguments  ===================
    test(
      interp(
        AppNamedArgs(
          Fun(
            List("x", "y", "z"),
            Sub(Add(Id("x"), Id("y")), Id("z"))
          ),
          List(Num(1), Num(2), Num(3)),
          Map.empty[String, Expr]
        ), Map.empty
      ),
      NumV(0)
    ) // { (x, y, z) => ((x + y) - z) }(1, 2, 3)

    test(
      interp(
        AppNamedArgs(
          Fun(
            List("x", "y", "z"),
            Sub(Add(Id("x"), Id("y")), Id("z"))
          ),
          List(Num(1)),
          Map("y"->Num(2), "z"->Num(10))
        ), Map.empty
      ),
      NumV(-7)
    ) // { (x, y, z) => ((x + y) - z) }(1, y=2, z=10)

    test(
      interp(
        AppNamedArgs(
          Fun(
            List("x", "y", "z"),
            Sub(Add(Id("x"), Id("y")), Id("z"))
          ),
          List(),
          Map("z"->Num(3), "y"->Num(2), "x"->Num(1))
        ), Map.empty
      ),
      NumV(0)
    ) // { (x, y, z) => ((x + y) - z) }(z=3, y=2, x=1)

    testExc(
      interp(
        AppNamedArgs(
          Fun(
            List("x", "y", "z"),
            Sub(Add(Id("x"), Id("y")), Id("z"))
          ),
          List(Num(1)),
          Map("y"->Num(2))
        ), Map.empty
      ),
      "Wrong arity!"
    ) // { (x, y, z) => ((x + y) - z) }(1, y=2)

    testExc(
      interp(
        AppNamedArgs(
          Fun(
            List("x", "y"),
            Add(Id("x"), Id("y"))
          ),
          List(),
          Map("x"->Num(1), "y"->Num(2), "z"->Num(3))
        ), Map.empty
      ),
      "Unexpected named argument!"
    )
    // { (x, y) => (x + y) }(x = 1, y = 2, z = 3)

    testExc(
      interp(
        AppNamedArgs(
          Fun(
            List("x", "y"),
            Id("x")
          ),
          List(Num(10)),
          Map("x"->Num(1), "y"->Num(20))
        ), Map.empty
      ),
      "Got both positional and named arguments for one parameter!"
    )
    // { (x, y) => (x + y) }(x = 1, y = 2, z = 3)

    // =================== Argument Unpacking  ===================
    test(
      interp(
        AppStarredArgs(
          Fun(
            List("x", "y", "z", "w"),
            Add(Sub(Id("x"), Id("y")), Sub(Id("z"), Id("w")))
          ),
          List(Num(4), Num(3)),
          List(RecNamed(Map("z"->Num(2), "w"->Num(1))))
        ), Map.empty
      ),
      NumV(2)
    ) // { (x, y, z, w) => ((x - y) + (z - w)) }(4, 3, **{ z = 2, w = 1 })

    test(
      interp(
        Val(
          "NBMI",
          Fun(
            List("height", "weight", "age"),
            Div(Div(Mul(Id("height"), Id("height")), Id("weight")), Id("age"))
          ),
          Val(
            "person",
            RecNamed(Map("height"->Num(175), "weight"->Num(75), "age"->Num(30))),
            AppStarredArgs(Id("NBMI"), List(), List(Id("person")))
          )
        ), Map.empty
      ),
      NumV(13)
    )
    /*
    {
      val NBMI = { (height, weight, age) => (((height * height) / weight) / age) };
      {
        val person = { height = 175, weight = 75, age = 30 };
        NBMI(**person)
      }
    }
    */

    testExc(
      interp(
        AppStarredArgs(
          Fun(
            List("x", "y", "z", "w"),
            Add(Sub(Id("x"), Id("y")), Sub(Id("z"), Id("w")))
          ),
          List(Num(4)),
          List(RecNamed(Map("z"->Num(2), "w"->Num(1))))
        ), Map.empty
      ),
      "Wrong arity!"
    ) // { (x, y, z, w) => ((x - y) + (z - w)) }(4, 3, 2, **{ z = 2, w = 1 })

    testExc(
      interp(
        AppStarredArgs(
          Fun(
            List("x", "y"),
            Add(Id("x"), Id("y"))
          ),
          List(),
          List(RecNamed(Map("x"->Num(1), "y"->Num(2), "z"->Num(3))))
        ), Map.empty
      ),
      "Unexpected named argument!"
    )
    // { (x, y) => (x + y) }(**{x = 1, y = 2, z = 3})

    // =================== Record Unpacking  ===================
    test(
      run(
        Val("rec1", RecNamed(Map("x"->Num(1), "y"->Num(2))),
          Val("rec2", RecNamed(Map("z"->Num(3), "w"->Num(4))),
            RecStarred(List(Id("rec1"), Id("rec2")))
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
        Val("rec1", RecNamed(Map("x"->Num(1), "y"->Num(2))),
          Val("rec2", RecNamed(Map("z"->Num(3), "w"->Num(4))),
            RecStarred(List(Id("rec1"), Id("rec2"), RecNamed(Map("x"->Num(10), "z"->Num(30)))))
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
      interp(
        Val("rec1", RecNamed(Map("x"->Num(1), "y"->Num(2))),
          Val("rec2", RecNamed(Map("z"->Num(3), "w"->Num(4))),
            Val(
              "merged",
              RecStarred(List(Id("rec1"), RecNamed(Map("x"->Num(10), "z"->Num(30))), Id("rec2"))
                ),
              Acc(Id("merged"), "x")
            )
          )
        ), Map.empty
      ),
      NumV(10)
    )
    /*
    {
      val rec1 = { x = 1, y = 2 };
      {
        val rec2 = { z = 3, w = 4 };
        {
          val merged = { **rec1, **{ x = 10, z = 30 }, **rec2 }
          merged.x
        }
      }
    }
    */

    testExc(
      interp(
        Val("rec1", Num(1),
          Val("rec2", RecNamed(Map("z"->Num(3), "w"->Num(4))),
            RecStarred(List(Id("rec1"), Id("rec2")))
          )
        ), Map.empty
      ),
      "Failed unpack. NumV(1) is not RecV!"
    )
    /*
    {
      val rec1 = 1;
      {
        val rec2 = { z = 3, w = 4 };
        { **rec1, **rec2 }
      }
    }
    */

    test(
      run(
        Val(
          "NBMI",
          Fun(
            List("height", "weight", "age"),
            Div(Div(Mul(Id("height"), Id("height")), Id("weight")), Id("age"))
          ),
          Val(
            "person",
            RecNamed(Map("height"->Num(175), "weight"->Num(75), "age"->Num(30))),
            Val(
              "nbmi",
              AppStarredArgs(Id("NBMI"), List(), List(Id("person"))),
              RecStarred(List(Id("person"), RecNamed(Map("nbmi"->Id("nbmi")))))
            )
          )
        )
      ),
      "{height = 175, weight = 75, age = 30, nbmi = 13}"
    )
    /*
    {
      val NBMI = { (height, weight, age) => (((height * height) / weight) / age) };
      {
        val person = { height = 175, weight = 75, age = 30 };
        {
          val nbmi = NBMI(**person);
          { **person, **{ nbmi = nbmi } }
        }
      }
    }
    */

    test(
      run(
        Val(
          "NBMI",
          Fun(
            List("height", "weight", "age"),
            Div(Div(Mul(Id("height"), Id("height")), Id("weight")), Id("age"))
          ),
          Val(
            "person",
            RecNamed(Map("height"->Num(175), "weight"->Num(75), "age"->Num(30))),
            Val(
              "skinnier",
              RecStarred(List(Id("person"), RecNamed(Map("weight"->Num(60))))),
              Id("skinnier")
            )
          )
        )
      ),
      "{height = 175, weight = 60, age = 30}"
    )
    /*
    {
      val NBMI = { (height, weight, age) => (((height * height) / weight) / age) };
      {
        val person = { height = 175, weight = 75, age = 30 };
        {
          val skinnier = { **person, **{weight = 60} };
          skinnier
        }
      }
    }
    */

  }
}
