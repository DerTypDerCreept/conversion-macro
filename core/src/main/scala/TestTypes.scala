import language.higherKinds
object TestTypes{

@convert
trait ConvertMe{
  trait Tree
  
  case class Leaf(tag:Int) extends Tree
  case class Branch(children:List[Tree]) extends Tree
}

}