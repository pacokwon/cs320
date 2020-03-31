package cs320

import cs320._

package object ex03 extends Exercise03 {
  // applies a binary numeric function on all combinations of numbers from
  // the two input lists, and return the list of all of the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = ???
      rs.map(f) ++ binOp(op, rest, rs)
  }

  def equalized(nums1: List[Int], nums2: List[Int]): List[(Int, Int)] = {
    val diff = nums1.length - nums2.length
    val short = if (diff > 0) nums2 ++ List.fill(diff)(nums2.last) else nums1 ++ List.fill(-diff)(nums1.last)
    val long = if (diff > 0) nums1 else nums2

    if (diff > 0) ( long zip short ) else ( short zip long )
  }

  def add_helper(nums1: List[Int], nums2: List[Int]): List[Int] = equalized(nums1, nums2).map(e => e._1 + e._2)

  def sub_helper(nums1: List[Int], nums2: List[Int]): List[Int] = equalized(nums1, nums2).map(e => e._1 - e._2)

  def cross(l1: List[Int], l2: List[Int], l3: List[Int]): List[(Int, Int, Int)] =
    l1.flatMap(x => l2.flatMap(y => l3.map(z => (x, y, z))))

  def min_helper(left: List[Int], mid: List[Int], right: List[Int]): List[Int] =
    cross(left, mid, right).map(e => e._1 min e._2 min e._3)

  def max_helper(left: List[Int], mid: List[Int], right: List[Int]): List[Int] =
    cross(left, mid, right).map(e => e._1 max e._2 max e._3)

  def interp(expr: Expr, env: Env): List[Int] = {
    expr match {
      case Num(nums) => nums
      case Add(left, right) => add_helper(interp(left, env), interp(right, env))
      case Sub(left, right) => sub_helper(interp(left, env), interp(right, env))
      case Val(name, expr, body) => interp(body, env + (name -> interp(expr, env)))
      case Id(id) => env.getOrElse(id, error("undefined identifier"))
      case Min(left, mid, right) => min_helper(interp(left, env), interp(mid, env), interp(right, env))
      case Max(left, mid, right) => max_helper(interp(left, env), interp(mid, env), interp(right, env))
    }
  }

  def tests: Unit = {
    test(run("(3 + 7)"), List(10))
    test(run("(10 - (3, 5))"), List(7, 5))
    test(run("{ val x = (5 + 5); (x + x) }"), List(20))
    test(run("min(3, 4, 5)"), List(3))
    test(run("max((1 + 2), 4, 5)"), List(5))
    test(run("min((1, 4), (2, 9), 3)"), List(1, 1, 2, 3))
    test(run("max((1, 6), (2, 5), (3, 4))"), List(3, 4, 5, 5, 6, 6, 6, 6))

    /* Write your own tests */
  }
}
