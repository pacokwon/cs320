package cs320

package object proj01 extends Project01 {
  def bare(value: Value): Int =
    value match {
      case IntV(n) => n
      case _ => error("value not integer value!")
    }

  def proj_helper(value: Value, idx: Int): Value =
    value match {
      case TupleV(es) => es(idx - 1)
      case _ => error("value not tuple value!")
    }

  def empty_helper(value: Value): Value =
    value match {
      case NilV => BooleanV(true)
      case ConsV(_, _) => BooleanV(false)
      case _ => error("value is neither nil nor cons")
    }

  def head_helper(value: Value): Value =
    value match {
      case ConsV(h, t) => h
      case _ => error("value is not cons")
    }

  def tail_helper(value: Value): Value =
    value match {
      case ConsV(h, t) => t
      case _ => error("value is not cons")
    }

  def app_helper(f: Value, as: List[Value]): Value =
    f match {
      case clov: CloV => {
        clov.env = (clov.ps zip as).foldLeft(clov.env)(_ + _)
        interp(clov.b, clov.env)
      }
      case _ => error("value is not CloV")
    }

  def type_helper(value: Value, t: Type): Value = BooleanV(
    t == (value match {
      case IntV(_) => IntT
      case BooleanV(_) => BooleanT
      case TupleV(_) => TupleT
      case NilV | ConsV(_, _) => ListT
      case CloV(_, _, _) => FunctionT
    })
  )

  def cond_helper(c: Value, t: Expr, f: Expr, e: Env): Value =
    c match {
      case BooleanV(b) => if (b) interp(t, e) else interp(f, e)
      case _ => error("condition is not BooleanV")
    }

  def fun_helper(ds: List[FunDef], b: Expr, e: Env): Value = {
    val reducedEnv = ds.foldLeft(e)((acc, fd) => {
      val c = CloV(fd.ps, fd.b, acc)
      c.env += fd.n -> c
      acc + (fd.n -> c)
    })

    interp(b, reducedEnv)
  }

  def interp(e: Expr, env: Env): Value = {
    e match {
      case IntE(n) => IntV(n)
      case Add(l, r) => IntV(bare(interp(l, env)) + bare(interp(r, env)))
      case Mul(l, r) => IntV(bare(interp(l, env)) * bare(interp(r, env)))
      case Div(l, r) => IntV(bare(interp(l, env)) / bare(interp(r, env)))
      case Mod(l, r) => IntV(bare(interp(l, env)) % bare(interp(r, env)))
      case BooleanE(b) => BooleanV(b)
      case Eq(l, r) => if (bare(interp(l, env)) == bare(interp(r, env))) BooleanV(true) else BooleanV(false)
      case Lt(l, r) => if (bare(interp(l, env)) < bare(interp(r, env))) BooleanV(true) else BooleanV(false)
      case TupleE(es) => TupleV(es.map(exp => interp(exp, env)))
      case Proj(t, i) => proj_helper(interp(t, env), i)
      case NilE => NilV
      case ConsE(h, t) => ConsV(interp(h, env), interp(t, env))
      case Empty(l) => empty_helper(interp(l, env))
      case Head(l) => head_helper(interp(l, env))
      case Tail(l) => tail_helper(interp(l, env))
      case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
      case Id(x) => env.getOrElse(x, error(s"undefined variable name $x"))
      case Fun(ps, b) => CloV(ps, b, env)
      case App(f, as) => app_helper(interp(f, env), as.map(exp => interp(exp, env)))
      case Test(e, t) => type_helper(interp(e, env), t)
      case If(c, t, f) => cond_helper(interp(c, env), t, f, env)
    }
  }

  def tests: Unit = {
    // test-int
    test(run("42"), "42")
    // test-add
    test(run("1 + 2"), "3")
    // test-sub
    test(run("7 - 2"), "5")
    // test-mul
    test(run("2 * 4"), "8")
    // test-div
    test(run("5 / 2"), "2")
    // test-mod
    test(run("13 % 5"), "3")
    // test-neg
    test(run("1 - -1"), "2")

    // test-boolean
    test(run("true"), "true")
    // test-eq
    test(run("1 == 3 - 2"), "true")
    // test-lt
    test(run("1 < 3 - 2"), "false")

    // test-tuple1
    test(run("(1, 2 + 3, true)"), "(1, 5, true)")
    // test-tuple2
    test(run("((42, 3 * 2), false)"), "((42, 6), false)")
    // test-proj1
    test(run("(1, 2 + 3, true)._1"), "1")
    // test-proj2
    test(run("((42, 3 * 2), false)._1._2"), "6")

    // test-nil
    test(run("Nil"), "Nil")
    // test-cons
    test(run("1 :: 1 + 1 :: Nil"), "(1 :: (2 :: Nil))")
    // test-isempty1
    test(run("Nil.isEmpty"), "true")
    // test-isempty2
    test(run("(1 :: Nil).isEmpty"), "false")
    // test-head
    test(run("(1 :: Nil).head"), "1")
    // test-tail
    test(run("(1 :: Nil).tail"), "Nil")
    // test-tail-head
    test(run("(1 :: 2 :: Nil).tail.head"), "2")
    
    // test-local1
    test(run("""
      val x = 1 + 2;
      val y = x * 4 + 1;
      y / (x - 1)
    """), "6")
    // test-local2
    test(run("""
      val (x, y) = (1 + 2, 3 + 4);
      val z = x * y;
      val (a, b, c) = (z, z + z, z + z + z);
      c - b
    """), "21")

    // test-fun
    test(run("x => x + x"), "<function>")
    // test-app1
    test(run("(x => x + x)(1)"), "2")
    // test-app2
    test(run("(x => y => x + y)(1)(2)"), "3")
    // test-app3
    test(run("((x, y) => x + y)(1, 2)"), "3")

    // test-type1
    test(run("1.isInstanceOf[Int]"), "true")
    // test-type2
    test(run("1.isInstanceOf[Boolean]"), "false")
    // test-type3
    test(run("(1 :: Nil).isInstanceOf[List]"), "true")
    // test-type4
    test(run("(x => x + x).isInstanceOf[Function]"), "true")

    // test-if
    test(run("if (true) 1 else 2"), "1")
    // test-not
    test(run("!true"), "false")
    // test-and
    test(run("true && false"), "false")
    // test-or
    test(run("true || false"), "true")
    // test-neq
    test(run("1 != 2"), "true")
    // test-lte
    test(run("1 <= 1"), "true")
    // test-gt
    test(run("1 > 1"), "false")
    // test-gte
    test(run("1 >= 1"), "true")
    // test-nonempty
    test(run("Nil.nonEmpty"), "false")

    // test-rec1
    test(run("""
      def f(x) = x - 1;
      f(2)
    """), "1")
    // test-rec2
    test(run("""
      def f(x) = if (x < 1) 0 else x + f(x - 1);
      f(10)
    """), "55")
  }
}
