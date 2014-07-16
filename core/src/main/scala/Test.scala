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

object Test extends App with ConvertMe with ConvertMe2 with ConvertMe3 with ConvertMe4{
    import Helper.spts
	
	
        
    //import ConvertMe._
    spts("hello")
    //ConvertMe.saySomething
    
    val tree =
    Branch(List(
      Branch(List(
        Leaf(1),
        Leaf(2))),
      Leaf(3),
      Branch(List(
        Leaf(4),
        Leaf(5)))))
    
    val tree2 =
    Branch2(Set(
      Branch2(Set(
        Leaf2(1),
        Leaf2(2))),
      Leaf2(3),
      Branch2(Set(
        Leaf2(4),
        Leaf2(5)))))    
        
    val tree3 =
    Branch3(List(Leaf3(5)),Set(
      Branch3(List(Leaf3(5)),Set(
        Leaf3(1),
        Leaf3(2))),
      Leaf3(3),
      Branch3(List(Leaf3(5)),Set(
        Leaf3(4),
        Leaf3(5)))))      

val tree4 =
    Branch4(List(Leaf4(5)),Set(
      Branch4(List(Leaf4(5)),Set(
        Leaf4(1),
        Leaf4(2))),
      Leaf4(3),
      Branch4(List(Leaf4(5)),Set(
        Leaf4(4),
        Leaf4(5)))))          
        
  def sumLeaves(t: Tree): Int = t.fold[Int] {
    case Leaf(n) => n
    case Branch(ns) => ns.sum
  }
  
  def sumLeaves2(t: Tree2[Int]): Int = t.fold[Int] {
    case Leaf2(n) => n
    case Branch2(ns) => ns.sum
  }
  
  def sumLeaves3(t: Tree3): Int = t.fold[Int] {
    case Leaf3(n) => n
    case Branch3(na,nb) => na.sum + nb.sum
  }
  def sumLeaves4(t: Tree4[Int]): Int = t.fold[Int] {
    case Leaf4(n) => n
    case Branch4(na,nb) => na.sum + nb.sum
  }
  
	println(tree2)
  println(s"sumLeaves( [[1, 2], 3, [4, 5]] ) = ${sumLeaves(tree)}")
  println(s"sumLeaves( [[1, 2], 3, [4, 5]] ) = ${sumLeaves2(tree2)}")
  println(s"sumLeaves( ... ) = ${sumLeaves3(tree3)}")
  println(s"sumLeaves( ... ) = ${sumLeaves4(tree4)}")
}
