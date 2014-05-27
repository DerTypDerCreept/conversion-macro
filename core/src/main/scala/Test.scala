import TestTypes._
import language.implicitConversions
import language.higherKinds
import org.scalatest._

object Helper{
    //print test seperator
    def pts (text: String, seperator: String, length: Int){
        println(seperator * length)
        println(text)
    }
    //standard test seperator
    def spts(text: String) = pts(text,"=",75)
}

object Test extends App with ConvertMe{
    import Helper.spts
    //import ConvertMe._
    spts("hello")
    //ConvertMe.saySomething
    val tree: Tree =
    Branch(List(
      Branch(List(
        Leaf(1),
        Leaf(2))),
      Leaf(3),
      Branch(List(
        Leaf(4),
        Leaf(5)))))

  def sumLeaves(t: Tree): Int = t.fold[Int] {
    case LeafF(n) => n
    case BranchF(ns) => ns.sum
  }
  println(s"sumLeaves( [[1, 2], 3, [4, 5]] ) = ${sumLeaves(tree)}")
}
