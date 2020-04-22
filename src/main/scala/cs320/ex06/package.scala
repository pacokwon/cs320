package cs320

package object ex06 extends Exercise06 {
  def malloc(sto: Sto): Addr =
    sto.foldLeft(0) {
      case (max, (addr, _)) => math.max(max, addr)
    } + 1

  def binOpHelper(l: Value, r: Value, op: (Int, Int) => Int): Value =
    (l, r) match {
      case (NumV(n), NumV(m)) => NumV(op(n, m))
      case (_, _) => error("not a number")
    }

  def binOp(left: Expr, right: Expr, env: Env, sto0: Sto, op: (Int, Int) => Int): (Value, Sto) = {
    val (leftV, sto1) = interp(left, env, sto0)
    val (rightV, sto2) = interp(right, env, sto1)

    (binOpHelper(leftV, rightV, op), sto2)
  }

  def appHelper(fs: (Value, Sto), argE: Expr, env: Env): (Value, Sto) = {
    val (fun, sto) = fs
    val (argV, newSto) = interp(argE, env, sto)
    fun match {
      case CloV(param, body, env) => interp(body, env + (param -> argV), newSto)
      case _ => error(s"$fun is not CloV")
    }
  }

  def newBoxHelper(valandsto: (Value, Sto)): (Value, Sto) = {
    val (v, sto) = valandsto
    val newAddr = malloc(sto)
    (BoxV(newAddr), sto + (newAddr -> v))
  }

  def setBoxHelper(boxE: Expr, expr: Expr, env: Env, sto: Sto): (Value, Sto) = {
    val (box, tmpsto) = interp(boxE, env, sto)
    val (value, newSto) = interp(expr, env, tmpsto)
    box match {
      case BoxV(addr) => (value, sto + (addr -> value))
      case _ => error(s"$box is not BoxV")
    }
  }

  def openBoxHelper(vs: (Value, Sto)): (Value, Sto) =
    vs._1 match {
      case BoxV(addr) => (vs._2.getOrElse(addr, error(s"$addr is empty")), vs._2)
      case _ => error(s"not a box")
    }

  def seqHelper(left: Expr, right: List[Expr], env: Env, sto: Sto): (Value, Sto) = {
    val (leftV, leftS) = interp(left, env, sto)

    right match {
      case Nil => (leftV, leftS)
      case h :: t => seqHelper(h, t, env, leftS)
      case _ => error(s"$right is not a list")
    }
  }

  def recHelper(fields: List[(String, Expr)], sto: Sto, env: Env): (Value, Sto) = {
    val (newSto, recFields) = fields.foldLeft((sto, Map[String, Addr]())) {
      case (acc, (field, exp)) =>
        val (stoAcc, mapAcc) = acc
        val (v, s) = interp(exp, env, stoAcc)
        val newAddr = malloc(s)
        (s + (newAddr -> v), mapAcc + (field -> newAddr))
    }

    (RecV(recFields), newSto)
  }

  def getHelper(vs: (Value, Sto), field: String): (Value, Sto) = {
    val (record, sto) = vs
    (record match {
      case RecV(fields) => sto.getOrElse(
        fields.getOrElse(field, error(s"no such field $field")),
        error(s"address is empty")
      )
    case _ => error(s"$record is not RecV")
    }, sto)
  }

  def setHelper(record: Expr, field: String, expr: Expr, env: Env, sto0: Sto): (Value, Sto) = {
    val (recordV, sto1) = interp(record, env, sto0)

    recordV match {
      case RecV(fields) =>
        val (result, sto2) = interp(expr, env, sto1)
        (result, sto2 + (fields.getOrElse(field, error(s"$field is not String")) -> result))
      case _ => error(s"$recordV is not RecV")
    }
  }

  def interp(e: Expr, env: Env, sto: Sto): (Value, Sto) =
    e match {
      case Num(num) => (NumV(num), sto)
      case Add(left, right) => binOp(left, right, env, sto, _ + _)
      case Sub(left, right) => binOp(left, right, env, sto, _ - _)
      case Id(name) => (env.getOrElse(name, error(s"$name is a free identifier")), sto)
      case Fun(param, body) => (CloV(param, body, env), sto)
      case App(fun, arg) => appHelper(interp(fun, env, sto), arg, env)
      case NewBox(expr) => newBoxHelper(interp(expr, env, sto))
      case SetBox(box, expr) => setBoxHelper(box, expr, env, sto)
      case OpenBox(box) => openBoxHelper(interp(box, env, sto))
      case Seqn(left, right) => seqHelper(left, right, env, sto)
      case Rec(fields) => recHelper(fields, sto, env)
      case Get(record, field) => getHelper(interp(record, env, sto), field)
      case Set(record, field, expr) => setHelper(record, field, expr, env, sto)
    }


  def tests: Unit = {
    test(run("""{
                  b => {
                    b.set((2 + b.get));
                    b.set((3 + b.get));
                    b.set((4 + b.get));
                    b.get
                  }
                }(Box(1))"""), "10")
    // println(Expr("{ x = 1 }.y"))
    testExc(run("{ x = 1 }.y"), "no such field")
    test(run("""{
                  r => {
                    { r.x = 5 };
                    r.x
                  }
                }({ x = 1 })"""), "5")
    test(run("42"), "42")
    test(run("{ x => x }"), "function")
    test(run("Box(1)"), "box")
    test(run("{}"), "record")

    /* Write your own tests */
  }
}
