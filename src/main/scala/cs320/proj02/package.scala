package cs320

package object proj02 extends Project02 {
  def intOp(op: (Int, Int) => Int): (Value, Value) => Value =
    (_, _) match {
      case (IntV(l), IntV(r)) => IntV(op(l, r))
      case (_, _) => error("Wrong Type: operands are not IntV")
    }

  def boolOp(op: (Int, Int) => Boolean): (Value, Value) => Value =
    (_, _) match {
      case (IntV(l), IntV(r)) => BooleanV(op(l, r))
      case (_, _) => error("Wrong Type: operands are not IntV")
    }

  val intVAdd = intOp(_ + _)
  val intVMul = intOp(_ * _)
  val intVEq = boolOp(_ == _)
  val intVLt = boolOp(_ < _)

  val intVDiv: (Value, Value) => Value = (_, _) match {
    case (IntV(l), IntV(r)) => if (r != 0) IntV(l / r) else error("Zero Division")
    case (_, _) => error("Wrong Type: operands are not IntV")
  }
  val intVMod: (Value, Value) => Value = (_, _) match {
    case (IntV(l), IntV(r)) => if (r != 0) IntV(l % r) else error("Zero Division")
    case (_, _) => error("Wrong Type: operands are not IntV")
  }

  def interp(e: Expr, env: Env, k: Cont, ek: ECont): Value =
    e match {
      case IntE(value) => k(IntV(value))
      case BooleanE(value) => k(BooleanV(value))
      case Add(e1, e2) =>
        interp(e1, env, v1 =>
          interp(e2, env, v2 =>
            k(intVAdd(v1, v2)),
            ek
          ),
          ek
        )
      case Mul(e1, e2) =>
        interp(e1, env, v1 =>
          interp(e2, env, v2 =>
            k(intVMul(v1, v2)),
            ek
          ),
          ek
        )
      case Div(e1, e2) =>
        interp(e1, env, v1 =>
          interp(e2, env, v2 =>
            k(intVDiv(v1, v2)),
            ek
          ),
          ek
        )
      case Mod(e1, e2) =>
        interp(e1, env, v1 =>
          interp(e2, env, v2 =>
            k(intVMod(v1, v2)),
            ek
          ),
          ek
        )
      case Eq(e1, e2) =>
        interp(e1, env, v1 =>
          interp(e2, env, v2 =>
            k(intVEq(v1, v2)),
            ek
          ),
          ek
        )
      case Lt(e1, e2) =>
        interp(e1, env, v1 =>
          interp(e2, env, v2 =>
            k(intVLt(v1, v2)),
            ek
          ),
          ek
        )

      case If(cond, tb, fb) =>
        interp(cond, env, v =>
          v match {
            case BooleanV(b) => interp(if (b) tb else fb, env, bv => k(bv), ek)
            case _ => error("Wrong Type: condition not BooleanV")
          },
          ek
        )

      case TupleE(exps) => k(TupleV(exps.map(exp => interp(exp, env, v => v, None))))

      case Proj(exp, idx) =>
        interp(exp, env, v =>
          v match {
            case TupleV(values) => k(values(idx - 1))
            case _ => error("Wrong Type: operand is not TupleV")
          },
          ek
        )
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

    // test-val1
    test(run("""
      val x = 1 + 2;
      val y = x * 4 + 1;
      y / (x - 1)
    """), "6")
    // test-val2
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

    // test-vcc1
    test(run("""
      vcc x;
      1 + x(1) + 1
    """), "1")
    // test-vcc2
    test(run("""
      (x => x * x)(
        1 + vcc x; 1 + x(2) + 3
      )
    """), "9")

    // test-return1
    test(run("(x => (return 1) + x)(2)"), "1")
    // test-return2
    test(run("""
      def div(x) = (x => 10 / x)(
        if (x == 0) return 0 else x
      );
      div(0) + div(10)
    """), "1")

    // test-throw1
    testExc(run("throw 1"), "")
    // test-throw2
    testExc(run("throw throw 1"), "")

    // test-try1
    test(run("""
      try {
        throw 1
      } catch (
        x => x + x
      )
    """), "2")
    // test-try2
    test(run("""
      1 + vcc x;
        try {
          throw 1
        } catch x
    """), "2")
  }
}
