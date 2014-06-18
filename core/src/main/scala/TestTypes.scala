import language.higherKinds
object TestTypes{

	trait Functor[F[_]] {
        def fmap[A, B](f: A => B)(fa: F[A]): F[B]
    }

        implicit def listFunctor: Functor[List] = new Functor[List] {
                def fmap[A, B](f: A => B)(fa: List[A]): List[B] = fa map f
            }
        implicit def setFunctor: Functor[Set] = new Functor[Set] {
                def fmap[A, B](f: A => B)(fa: Set[A]): Set[B] = fa map f
            }
    
    

@convert
trait ConvertMe{
  trait Tree
  
  case class Leaf(tag:Int) extends Tree
  case class Branch(children:List[Tree]) extends Tree

}
@convert
trait ConvertMe2{
      trait Tree2
  case class Leaf2(tag:Int) extends Tree2
  case class Branch2(children:Set[Tree2]) extends Tree2 
  
}

}