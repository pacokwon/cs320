package cs320

package object proj00 extends Project00 {
  def numOp(op: (Int, Int) => Int): (Value, Value) => Value =
    (_, _) match {
      case (IntV(l), IntV(r)) => IntV(op(l, r))
      case (_, _) => error("wrong type for number operation")
    }

  def boolOp(op: (Int, Int) => Boolean): (Value, Value) => Value =
    (_, _) match {
      case (IntV(l), IntV(r)) => BooleanV(op(l, r))
      case (_, _) => error("wrong type for boolean operation")
    }

  val numAdd = numOp(_ + _)
  val numSub = numOp(_ + _)
  val numMul = numOp(_ * _)
  val numDiv = numOp(_ / _)
  val numMod = numOp(_ % _)

  val numEq = boolOp(_ == _)
  val numLt = boolOp(_ < _)

  def projHelper(t: Value, i: Int): Value =
    t match {
      case TupleV(vs) => vs(i - 1)
      case _ => error(s"wrong type for value $t")
    }

  def headHelper(l: Value): Value =
    l match {
      case ConsV(h, t) => h
      case _ => error(s"wrong type $l")
    }

  def tailHelper(l: Value): Value =
    l match {
      case ConsV(h, t) => t
      case _ => error(s"wrong type $l")
    }

  def appHelper(f: Value, as: List[Value]): Value =
    f match {
      case CloV(ps, b, env) => interp(b, env ++ (ps zip as))
      case _ => error(s"wrong type: $f is not CloV")
    }

  def typeHelper(e: Value, t: Type): Value =
    BooleanV(t == (e match {
      case IntV(_) => IntT
      case BooleanV(_) => BooleanT
      case TupleV(_) => TupleT
      case NilV | ConsV(_, _) => ListT
      case CloV(_, _, _) => FunctionT
    }))

  def condHelper(c: Value, t: Expr, f: Expr, e: Env): Value =
    c match {
      case BooleanV(b) => interp(if (b) t else f, e)
      case _ => error(s"wrong type: $c is not BooleanV")
    }

  def interp(exp: Expr, env: Env): Value =
    exp match {
      case IntE(n) => IntV(n)
      case Add(l, r) => numAdd(interp(l, env), interp(r, env))
      case Mul(l, r) => numMul(interp(l, env), interp(r, env))
      case Div(l, r) => numDiv(interp(l, env), interp(r, env))
      case Mod(l, r) => numMod(interp(l, env), interp(r, env))
      case BooleanE(b) => BooleanV(b)
      case Eq(l, r) => numEq(interp(l, env), interp(r, env))
      case Lt(l, r) => numLt(interp(l, env), interp(r, env))
      case TupleE(es) => TupleV(es.map(e => interp(e, env)))
      case Proj(t, i) => projHelper(interp(t, env), i)
      case NilE => NilV
      case ConsE(h, t) => ConsV(interp(h, env), interp(t, env))
      case Empty(l) => BooleanV(interp(l, env) == NilV)
      case Head(l) => headHelper(interp(l, env))
      case Tail(l) => tailHelper(interp(l, env))
      case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
      case Id(x) => env.getOrElse(x, error(s"free identifier $x"))
      case Fun(ps, b) => CloV(ps, b, env)
      case App(f, as) => appHelper(interp(f, env), as.map(arg => interp(arg, env)))
      case Test(e, t) => typeHelper(interp(e, env), t)
      case If(c, t, f) => condHelper(interp(c, env), t, f, env)
    }

  def tests: Unit = {
    // // test-int
    // test(run("42"), "42")
    // // test-add
    // test(run("1 + 2"), "3")
    // // test-sub
    // test(run("7 - 2"), "5")
    // // test-mul
    // test(run("2 * 4"), "8")
    // // test-div
    // test(run("5 / 2"), "2")
    // // test-mod
    // test(run("13 % 5"), "3")
    // // test-neg
    // test(run("1 - -1"), "2")

    // // test-boolean
    // test(run("true"), "true")
    // // test-eq
    // test(run("1 == 3 - 2"), "true")
    // // test-lt
    // test(run("1 < 3 - 2"), "false")

    // // test-tuple1
    // test(run("(1, 2 + 3, true)"), "(1, 5, true)")
    // // test-tuple2
    // test(run("((42, 3 * 2), false)"), "((42, 6), false)")
    // // test-proj1
    // test(run("(1, 2 + 3, true)._1"), "1")
    // // test-proj2
    // test(run("((42, 3 * 2), false)._1._2"), "6")

    // // test-nil
    // test(run("Nil"), "Nil")
    // // test-cons
    // test(run("1 :: 1 + 1 :: Nil"), "(1 :: (2 :: Nil))")
    // // test-isempty1
    // test(run("Nil.isEmpty"), "true")
    // // test-isempty2
    // test(run("(1 :: Nil).isEmpty"), "false")
    // // test-head
    // test(run("(1 :: Nil).head"), "1")
    // // test-tail
    // test(run("(1 :: Nil).tail"), "Nil")
    // // test-tail-head
    // test(run("(1 :: 2 :: Nil).tail.head"), "2")

//     // test-local1
//     test(run("""
//       val x = 1 + 2;
//       val y = x * 4 + 1;
//       y / (x - 1)
//     """), "6")
//     // test-local2
//     test(run("""
//       val (x, y) = (1 + 2, 3 + 4);
//       val z = x * y;
//       val (a, b, c) = (z, z + z, z + z + z);
//       c - b
//     """), "21")

//     // test-fun
//     test(run("x => x + x"), "<function>")
//     // test-app1
//     test(run("(x => x + x)(1)"), "2")
//     // test-app2
//     test(run("(x => y => x + y)(1)(2)"), "3")
//     // test-app3
//     test(run("((x, y) => x + y)(1, 2)"), "3")

//     // test-type1
//     test(run("1.isInstanceOf[Int]"), "true")
//     // test-type2
//     test(run("1.isInstanceOf[Boolean]"), "false")
//     // test-type3
//     test(run("(1 :: Nil).isInstanceOf[List]"), "true")
//     // test-type4
//     test(run("(x => x + x).isInstanceOf[Function]"), "true")

//     // test-if
//     test(run("if (true) 1 else 2"), "1")
//     // test-not
//     test(run("!true"), "false")
//     // test-and
//     test(run("true && false"), "false")
//     // test-or
//     test(run("true || false"), "true")
//     // test-neq
//     test(run("1 != 2"), "true")
//     // test-lte
//     test(run("1 <= 1"), "true")
//     // test-gt
//     test(run("1 > 1"), "false")
//     // test-gte
//     test(run("1 >= 1"), "true")
//     // test-nonempty
//     test(run("Nil.nonEmpty"), "false")

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
