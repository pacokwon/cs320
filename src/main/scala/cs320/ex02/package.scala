package cs320

package object ex02 extends Exercise02 {
  // Problem 1
  def freeIds(expr: Expr): Set[String] = {
    def freeIdsRecursive(bindingids: Set[String], exp: Expr): Set[String] = {
      exp match {
        case Num(_) => Set()
        case Add(left, right) => freeIdsRecursive(bindingids, left) | freeIdsRecursive(bindingids, right)
        case Sub(left, right) => freeIdsRecursive(bindingids, left) | freeIdsRecursive(bindingids, right)
        case Val(name, expr, body) => freeIdsRecursive(bindingids | Set(name), expr) | freeIdsRecursive(bindingids | Set(name), body)
        case Id(id) => if (bindingids contains id) Set() else Set(id)
      }
    }

    freeIdsRecursive(Set(), expr)
  }

  // Problem 2
  def bindingIds(expr: Expr): Set[String] = {
    def bindingIdsRecursive(exp: Expr): Set[String] = {
      exp match {
        case Num(_) => Set()
        case Add(left, right) => bindingIdsRecursive(left) | bindingIdsRecursive(right)
        case Sub(left, right) => bindingIdsRecursive(left) | bindingIdsRecursive(right)
        case Val(name, expr, body) => Set(name) | bindingIdsRecursive(expr) | bindingIdsRecursive(body)
        case Id(id) => Set()
      }
    }

    bindingIdsRecursive(expr)
  }

  // Problem 3
  def boundIds(expr: Expr): Set[String] = {
    def boundIdsRecursive(ids: Set[String], exp: Expr): Set[String] = {
      exp match {
        case Num(_) => Set()
        case Add(left, right) => boundIdsRecursive(ids, left) | boundIdsRecursive(ids, right)
        case Sub(left, right) => boundIdsRecursive(ids, left) | boundIdsRecursive(ids, right)
        case Val(name, expr, body) => boundIdsRecursive(ids | Set(name), expr) | boundIdsRecursive(ids | Set(name), body)
        case Id(id) => if (ids contains id) Set(id) else Set()
      }
    }

    boundIdsRecursive(Set(), expr)
  }

  // Tests
  def tests: Unit = {
    test(freeIds(Expr("{ val x = 1; (x + y) }")), Set("y"))
    test(freeIds(Expr("{ val z = 2; 1 }")), Set())
    test(bindingIds(Expr("{ val x = 1; (x + y) }")), Set("x"))
    test(bindingIds(Expr("{ val z = 2; 1 }")), Set("z"))
    test(boundIds(Expr("{ val x = 1; (x + y) }")), Set("x"))
    test(boundIds(Expr("{ val z = 2; 1 }")), Set())

    /* Write your own tests */
  }
}
