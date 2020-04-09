package cs320

package object ex05 extends Exercise05 {

  def interp(expr: Expr, env: Env): Value = ???

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
