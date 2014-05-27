import TestTypes._
import language.implicitConversions
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

object Test extends App with ConvertMe with Expr{
    import Helper.spts
    //import ConvertMe._
    spts("hello")
    //ConvertMe.saySomething
    val xs = Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))
      def sum1(xs: Lists[Int]): Int = xs.fold[Int] {
        case Nil() => 0
        case Cons(head, tail) => head + tail
      }
      def sum2(xs: Lists[Int]): Int = xs match {
        case Nil() => 0
        case Cons(head, tail) => head + sum2(tail)
      }
      println(s"sum1 = ${sum1(xs)}")
      println(s"sum2 = ${sum2(xs)}")
	  val value = Mul(Add(Num(5),Num(5)),Num(2))
	  val value2 = Mul(Add(Num(5),Num(5)),Num(3))
	  val value3 = Add(Add(Num(5),Num(5)),Num(2))
	  val value4 = Mul(Add(Num(5),Num(5)),Num(2))
	  // /*
	  assert(!value.equals(null))
	  assert(value.equals(value))
	  assert(value.equals(value4))
	  assert(!value.equals(value2))
	  assert(!value.equals(value3))
	  assert(!value.equals("No Way"))
	  println("equals assertions went through")
	  println(value)
	  // */
	  /*
	  val n = Num(5)
	  val m = Num.unapply(n)
	  println(m)
	  println(value)
	  
	  val p = n match {
		case Num(n) => n+1
	  }
	  println(p)
	  
	  def eval(e:Exp):Int = e match{
		case Num(a) => a
		case Add(a,b) => eval(a)+eval(b)
		case Mul(a,b) => eval(a)*eval(b)
		
	  }
	  
	  println(eval(value))
	  */
	  def foldExp(e: Exp) : Int = e.fold[Int]{
		case Num(n) => n
		//case Id(x) => x
		case Add(lhs,rhs) => lhs + rhs
		case Mul(lhs,rhs) => lhs * rhs
	  }
	  
	  println(s"exp1 = ${foldExp(value)}")
	
	}

    	

// /*
trait DataTypes
extends Companies
   with BinTrees
///*
class BinTreeSanityTest extends FlatSpec with DataTypes {
  "completeBinaryTree" should "produce complete binary trees" in {
    val List(bin1, bin2, bin3, bin4, bin5, bin6, bin7, bin8, bin9) =
      (1 to 9).map(completeBinTree).toList
    implicit def convert(n: Int): BinTree[Int] = Leaf(n)
    assert(bin1 == Leaf(1))
    assert(bin2 == Bin(1, 2))
    assert(bin3 == Bin(Bin(1, 2), 3))
    assert(bin4 == Bin(Bin(1, 2), Bin(3, 4)))
    assert(bin5 == Bin(Bin(Bin(1, 2), 3), Bin(4, 5)))
    assert(bin6 == Bin(Bin(Bin(1, 2), Bin(3, 4)), Bin(5, 6)))
    assert(bin7 == Bin(Bin(Bin(1, 2), Bin(3, 4)), Bin(Bin(5, 6), 7)))
    assert(bin8 == Bin(Bin(Bin(1, 2), Bin(3, 4)), Bin(Bin(5, 6), Bin(7, 8))))
    assert(bin9 == Bin(Bin(Bin(Bin(1, 2), 3), Bin(4, 5)), Bin(Bin(6, 7), Bin(8, 9))))
  }
}
// */
// this example is for correctness.
// @converMe macro doesn't make it easier to write
// due to not supporting mutually recursive data types.
//*
class SelectSalary extends FlatSpec with DataTypes {

  // collects all salary nodes in a list
  def selectSalary(company: Company): List[Salary] = {
    def selectSalary(department: Department): List[Salary] =
      department.manager.salary :: (
        for {
          unit <- department.dUnits
          salary = unit match {
            case PU(employee) => List(employee.salary)
            case DU(department) => selectSalary(department)
          }
          _ <- NoReturn
        }
        yield salary
      )
    company.departments flatMap selectSalary
  }

  "selectSalary" should "collect salary nodes in some order" in {
    val sortedSalaries = selectSalary(genCom).sortBy(_.amount)
    assert(sortedSalaries == List[Float](1000, 2000, 8000, 100000).map(Salary.apply))
  }
}
// */
object NoReturn {
  def map[T](f: Unit => T): T = f(())
}

// */