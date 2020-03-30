package cs320

trait Exercise01 extends Homework {

  def volumeOfCuboid(a: Int, b: Int, c: Int): Int
  def concat(x: String, y: String): String

  def addN(n: Int): Int => Int
  def twice(f: Int => Int): Int => Int
  def compose(f: Int => Int, g: Int => Int): Int => Int

  def double(l: List[Int]): List[Int]

  def sum(l: List[Int]): Int

  def getKey(m: Map[String, Int], s: String): Int

  trait Tree
  case class Branch(left: Tree, value: Int, right: Tree) extends Tree
  case class Leaf(value: Int) extends Tree
  def countLeaves(t: Tree): Int
  def flatten(t: Tree): List[Int]
}
