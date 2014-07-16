import language.higherKinds
import language.implicitConversions
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
    

@convertToGeneric
trait ConvertMe{
  trait Tree
  
  case class Leaf(tag:Int) extends Tree
  case class Branch(children:List[Tree]) extends Tree

}
@convertToGeneric
trait ConvertMe2{
  trait Tree2[T]
  case class Leaf2[T](tag:T) extends Tree2[T]
  case class Branch2[T](children:Set[Tree2]) extends Tree2[T] 
}

@convertToGeneric
trait ConvertMe3{
  trait Tree3
  case class Leaf3(tag:Int) extends Tree3
  case class Branch3(childrena:List[Tree3],childrenb:Set[Tree3]) extends Tree3 
}

@convertToGeneric
trait ConvertMe4{
  trait Tree4[T]
  case class Leaf4[T](tag:T) extends Tree4[T]
  case class Branch4[T](childrena:List[Tree4],childrenb:Set[Tree4]) extends Tree4[T] 
}
//@convertToGeneric
trait ConvertMe5{
    trait MyList[T]
    case class Cons[T](n:T,ns:MyList[T])  extends MyList[T]
    case class Nil[T] extends MyList[T]
}

}