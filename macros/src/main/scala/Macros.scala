import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

/** The name of the Annotation, used to invoke the macro
  *
  * Annotate a trait or object with @converToGeneric, to convert all the traits and case classes,
  * contained in it
  *
  * Remember to provide the definition of the Functor you wish to use
  * the basic definition:
  * trait Functor[F[_]] {
  *      def fmap[A, B](f: A => B)(fa: F[A]): F[B]
  * }
  * the List Functor
  * implicit def listFunctor: Functor[List] = new Functor[List] {
  *     def fmap[A, B](f: A => B)(fa: List[A]): List[B] = fa map f
  * }
  *
  *
  *
  * for example:
  * @convertToGeneric
  * trait ConvertMe{
  *   trait Tree[T]
  *   case class Leaf[T](tag:T) extends Tree[T]
  *   case class Branch[T](children:List[Tree]) extends Tree[T] 
  * }
  * would convert to something like this:
  * trait ConvertMe{
  *   trait Tree[T] extends TreeF[T,Tree[T]]{
  *     def fold[E](f: TreeF[T,E] => E): E = f(map (_ fold f))
  *   }
  *   
  *   trait TreeF[T,R] {
  *     def map[S](f: R => S)
  *       (implicit sf: Functor[List]):
  *         TreeF[T,S]
  *   }
  *    
  *   object TreeF {
  *     implicit def patternFunctor: Functor[({ type lambda[R] = $newtrait[T,R] })#lambda] = new Functor[({ type lambda[R] = $newtrait[T,R] })#lambda] {
  *       def fmap[A, B](f: A => B)(fa: TreeF): TreeF = fa map f
  *     }
  *   }
  *   
  *   class Leaf[T,R](val tag: T) extends TreeF[T,R] {
  *       def map[S](f: R => S)(implicit sf: Functor[List]): TreeF[T,S] = new Leaf[T,S](tag)
  *       override def toString: String = s"Leaf($tag)"
  *       override def equals(that:Any): Boolean = that match {
  *         case Leaf(x) => x == tag
  *         case _ => false
  *       }
  *       override def hashCode: Int = tag.hashCode     
  *   }
  *   object Leaf{
  *     def apply[T](n:T):Tree[T] = new Leaf[T,Tree[T]](n) with Tree[T]
  *     def unapply[T,R](tf:TreeF[T,R]): Option[T] = tf match{
  *         case l:Leaf[T,R] => Some(l.tag)
  *         case _ => None
  *     }
  *   }
  *    
  *   class Branch[T,R](val children: List[R]) extends TreeF[T,R] {
  *     def map[S](f: R => S)(implicit sf: Functor[List]): TreeF[T,S] = new Branch(sf.fmap(f)(children))
  *     override def toString: String = s"Branch(${children.toString})"
  *     override def equals(that:Any): Boolean = that match {
  *         case Branch(x) => x.equals(children)
  *         case _ => false
  *     }
  *     override def hashCode: Int = children.map(_.hashCode).fold(0)((x:Int,y:Int)=>x+y)
  *   }
  *   object Branch{
  *      def apply[T](l:List[Tree[T]]):Tree[T] = new Branch[T,Tree[T]](l) with Tree[T]//R?
  *      def unapply[T,R](tf:TreeF[T,R]): Option[List[R]] = tf match{
  *             case b:Branch[T,R] => Some(b.children)
  *             case _ => None
  *     }
  *   }
  * }
 
  */
class convertToGeneric extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro convertMacro.impl
}

/** This object contains the actual macro (in the impl function)
 *  and should only be used via the @convertToGeneric annotation
 */
object convertMacro {
    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
        import c.universe._
        
        
        
        
        //The new interface
        //represents the trait, we wish to treat as the fixed Point
        /** A simplified representation of a trait, containing all the information, needed to construct the generic Traits
          * 
          * @constructor creates a representation of a Trait, using its name and its generic Types
          * @param name the name of the trait 
          * @param typeParams a List of TypeDefs (the generic types of the trait)
          */
        case class FixedPoint(name: TypeName, typeParams: List[TypeDef])
        /** A simplified representation of a case class, containing all the information, needed to construct the generic class and object
          * 
          * @constructor creates a representation of a case class, using its name and its generic Types
          * @param name the name of the class 
          * @param typeParams a List of TypeDefs (the generic types of the class)
          * @param valParams a List of ValDefs containing the fields of the class
          * @param extend the type, extended by this class
          * @param extendTypes a List of Trees (the TypeRefs and applied TypeRefs, that are passed to the extended type)
          */
        case class Variant(name: TypeName, typeParams: List[TypeDef], valParams: List[ValDef], extend: Tree, extendTypes: List[Tree])
        /** The Collection of traits and case classes that are to be converted, in their simplified forms, as well as other classes and objects, that are passed trough, unchanged
          * 
          * @constructor creates the simplified input, that can then be expanded by the macro
          * @param fixed a List of traits (in their simplified form), that are to be expanded 
          * @param variants a List of classes (in their simplified form), that are to be expanded
          * @param passThrough a List of Trees, containing classes and objects, that are passed trough unchanged
          */
        case class BusinessInput(fixed: List[FixedPoint], variants: List[Variant], passThrough: List[Tree])
    
        /** returns a List of Trees, representing the contents of the object or trait that has been passed in
          *
          * matches the passed in Tree to a object and trait Definition and extracts the List of definitions contained in that object or trait
          * returns the extracted List or throws an Exception if the passed in Tree was not an object or trait
          *
          * @param module the Tree representing the object or trait
          * @return the List of Trees contained in the object or trait
          * @throw Exception if the passed in Tree is not a object or trait
          */
        def extractDefList(module:Tree):List[Tree] = module match {
            case ModuleDef(a, b, Template(_, _, list))   => list
            case ClassDef(a, b, c, Template(_, _, list)) => list
            case _                                       => throw new Exception("Could not extract Def List from"+module)
        }
        
        /** returns a List of FixedPoints, representing the traits found in the List of Trees that has been passed in
          *
          * matches each element of the passed in List of Trees to find trees of the following format:
          *     trait Traitname
          *     trait Traitname[Types]
          * returns the List of traits, that match the above format in the simple FixedPoint format
          *
          * @param raw the List of Trees that are to be filtered for traits
          * @return the List of FixedPoints contained in the List
          */
        def findFixedPoint(raw: List[Tree]): List[FixedPoint] = raw match {
            case q"trait $traitname[..$types]" :: tail => FixedPoint(traitname,reconstructTypes(types).asInstanceOf[List[TypeDef]]) :: findFixedPoint(tail)
            case head :: tail                          => findFixedPoint(tail)
            case _                                     => Nil//throw new Exception("Could not find Fixed Point (no trait in annotated object)")
        }

        /** returns a List of Variants, representing the case classes found in the List of Trees that has been passed in
          *
          * matches each element of the passed in List of Trees to find trees of the following format:
          *     case class Classname[Types](Vals) extends Extendname[ExtendTypes]
          * returns the List of case classes, that match the above format in the simple Variants format
          *
          * @param raw the List of Trees that are to be filtered for case classes
          * @return the List of Variants contained in the List
          */
        def findVariants(raw: List[Tree]): List[Variant] = raw match{
            case q"case class $name[..$types](..$fields) extends $fix[..$smth]" :: tail
                              => Variant(name,reconstructTypes(types).asInstanceOf[List[TypeDef]],fields,fix,smth) :: findVariants(tail)
            case head :: tail => findVariants(tail)
            case _            => Nil
            //case _ => throw new Exception("Find Variants Malfunctioned")
        }
        /** returns a List of Trees, representing all the objects or TypeDefs etc. that are not touched by the macro
          *
          * returns a List of all the Trees that do not match the following formats:
          *     case class Classname[Types](Vals) extends Extendname[ExtendTypes]
          *     trait Traitname
          *     trait Traitname[Types]
          *
          * @param raw the List of Trees that are to be filtered
          * @return the List of Trees that do not match the above formats contained in the List
          */
        def findOthers(raw: List[Tree]):List[Tree] = raw match{
            case q"case class $name[..$types](..$fields) extends $fix[..$smth]" :: tail => findOthers(tail)
            case q"trait $traitname[..$types]" :: tail                                  => findOthers(tail)
            case q"def $init(..$smth) = {..$smth2}" :: tail                             => findOthers(tail)
            case head :: tail                                                           => head :: findOthers(tail)
            case _                                                                      => Nil
        }
        /** returns a List of Trees, that is equal to the one that has been passed in, except that the type-bounds have been reconstructed
          *
          * when extracting TypeDefs of classes or traits, using quasiqotes, the type-bounds sometimes are not constructed correctly
          * thankfully all the information is there and we can reconstruct them
          * returns a List of all the Trees as they were, except for TypeDefs and AppliedTypeTrees, in which the type-bounds are reconstructed
          *
          * @param typeDefs the List of Trees that are to be checked for misconstructed TypeBounds
          * @return the List of Trees with reconstructed type-bounds
          */
        def reconstructTypes(typeDefs:List[Tree]):List[Tree] = typeDefs match{
            case TypeDef(a,b,c,d) :: tail     => TypeDef(a,b,c,reconstructTypesSub(d)) :: reconstructTypes(tail)
            case AppliedTypeTree(a,b) :: tail => AppliedTypeTree(reconstructTypes(List(a)).head,reconstructTypes(b)) :: reconstructTypes(tail)
            case head :: tail                 => head :: reconstructTypes(tail)
            case _                            => Nil
        }
        /** returns the Trees, that has been passed in, if it was a TypeBoundsTree, the type-bounds have been reconstructed
          *
          * when extracting TypeDefs of classes or traits, using quasiqotes, the type-bounds sometimes are not constructed correctly
          * thankfully all the information is there and we can reconstruct them
          * this is e helper-function for reconstructTypes, that manages the actual reconstruction
          *
          * @param typeDefs the List of Trees that are to be checked for misconstructed TypeBounds
          * @return the List of Trees with reconstructed type-bounds
          */
        def reconstructTypesSub(typeDefs:Tree):Tree = typeDefs match{
            case TypeBoundsTree(Select(Select(a, b), c), Select(Select(d,e), f)) => TypeBoundsTree(Select(Select(a, newTermName(b.toString)), c), Select(Select(d,newTermName(e.toString)), f))
            case _                                                               => typeDefs        
        }
        /** returns a List of Trees, containing the types of the corresponding values defined by the ValDefs in the Input List
          *
          * takes a List of ValDefs and returns a List of Trees, containing the Types of the defined values (in the same order as these values)
          *
          * @param valDefs the List of ValDefs to be converted to a List of their types
          * @return the List of Trees containing the Types of the passed in ValDefs
          */
        def extractTypes(valDefs:List[ValDef]):List[Tree] = valDefs match{
            case ValDef(_,_,c,_) :: tail => c :: extractTypes(tail)
            case _                       => Nil
        }
        /** returns a List of ValDefs, containing the ValDefs that have been passed in, with the override modifier
          *
          * As the modifiers are not available in the Context, we either have to cast them or extract them via qusiquote deconstruction
          * or, as we do here, just reconstruct the values using quasiqotes
          * takes a List of ValDefs and returns a List of ValDefs, that have the override modifier
          *
          * @param valDefs the List of ValDefs that needs the override modifier
          * @return the List of ValDefs passed in, with the override modifier added to them
          */
        def valDefsToOverride(valDefs:List[ValDef]):List[ValDef] = valDefs match{
            //case _ => valDefs
            case q"val $name: $typ" :: z         => q"override val $name: $typ" :: valDefsToOverride(z)
            case q"..$smth val $name: $typ" :: z => q"override val $name: $typ" :: valDefsToOverride(z)
            case _                               => Nil
        }
        /** returns a List of ValDefs, containing the ValDefs that have been passed in, reconstructed in a neutral environment (to remove the case modifier)
          *
          * As the modifiers are not available in the Context, we can't modify them easily
          * therefore we just reconstruct the values using quasiqotes
          *
          * @param valDefs the List of ValDefs that need to be cleansed of the case modifier
          * @return the List of ValDefds cleansed of the case modifier
          */
        def valDefsToNoCase(x:List[ValDef]):List[ValDef] = x match{
            //case _ => x
            case q"val $name: $typ" :: z         => q"val $name: $typ" :: valDefsToNoCase(z)
            case q"..$smth val $name: $typ" :: z => q"val $name: $typ" :: valDefsToNoCase(z)
            case _                               => Nil
        }
        /** returns a List of ValDefs, containing the ValDefs that have been passed in, with the val modifier
          *
          * As the modifiers are not available in the Context, we can't modify them easily
          * therefore we extract the desired set of modifiers from the following construct 
          *     q"class Num[FFunctor](val n:Int)..
          *
          * @param valDefs the List of ValDefs that need to be modified
          * @return the List of ValDefs with the extracted set of modifiers
          */
        def valDefsToNoCasePlusVal(valDefs:List[ValDef]):List[ValDef] = //x match{
            q"class Num[FFunctor](val n:Int) extends ExpF[FFunctor]" match{
                case q"class Num[FFunctor](..$vals) extends ExpF[FFunctor]" => vals match{
                    case ValDef(a,b,c,d) :: ignore => valDefs match { //Modifiers(scala.reflect.internal.Flags.ACCESSOR.toLong.asInstanceOf[FlagSet] | scala.reflect.internal.Flags.PARAMACCESSOR.toLong.asInstanceOf[FlagSet])
                        case ValDef(w,x,y,z) :: tail => {ValDef(a,x,y,z) :: valDefsToNoCasePlusVal(tail)}
                        case Nil => Nil
                        case _ => throw new Exception("Could not convert params3")
                    }
                    case _ => throw  new Exception("Could not convert params2")
                } 
                case _ => throw  new Exception("Could not convert params1")
            }
        /** returns a List of ValDefs, containing the ValDefs that have been passed in, with the private modifier
          *
          * As the modifiers are not available in the Context, we can't modify them easily
          * therefore we extract the desired set of modifiers from the following construct 
          *     q"private class ConsF[T](head: T, tail: Lists[T])..
          *
          * @param valDefs the List of ValDefs that need to be modified
          * @return the List of ValDefs with the extracted set of modifiers
          */    
        def valDefWithPrivate(valDefs:List[ValDef]):List[ValDef] = {
            val privMod = q"private class ConsF[T](head: T, tail: Lists[T]) extends Cons[T, Lists[T]](head, tail) with Lists[T]" match {
                case q"private class ConsF[T]($a, tail: Lists[T]) extends Cons[T, Lists[T]](head, tail) with Lists[T]" => a match {
                        case ValDef(mods,_,_,_) => mods
                        case _ => throw new Exception("Could not Construct Private Modifier")
                }
                case _ => throw new Exception("Could not Construct Private Modifier")
            }
            valDefs match {
                case ValDef(a,b,c,d) :: z => ValDef(privMod,b,c,d) :: valDefWithPrivate(z)
                case Nil                  => Nil
                case _                    => throw new Exception("Could not Construct Private Modifier"+valDefs)
            }
        }
        /** returns a List of Trees, containing references to the Types described by the passed in TypeDefs
          *
          * takes a List of Trees, that are either TypeDefs or AppliedTypeTrees and returns References to these Types
          *
          * @param typeDefs the List of TypeDefs that need to be converted
          * @return the List of Trees referencing the passed in types
          */ 
        def typeDefsToTypeRefs(typeDefs:List[Tree]):List[Tree] = typeDefs match {
            case TypeDef(a,b,List(),d) :: tail => Ident(b) :: typeDefsToTypeRefs(tail)
            case TypeDef(a,b,c,d) :: tail      => AppliedTypeTree(Ident(b),typeDefsToTypeRefs(c)) :: typeDefsToTypeRefs(tail)
            case AppliedTypeTree(a,b) :: tail  => AppliedTypeTree(a,typeDefsToTypeRefs(b)) :: typeDefsToTypeRefs(tail)
            case head :: tail                  => typeDefsToTypeRefs(tail)
            case _                             => Nil
            }
        /** returns a List of Trees, containing references to the passed in ValDefs, with values of a specific type, having the passed in function name applied to them
          *
          * takes a List of ValDefs and a type as well as a reference to a Function, iterates through the list and returns a list of references to the 
          * defined values, values of the type that has been passed in, have the passed in function applied to them
          *
          * @param valDefs the List of ValDefs that need to be referenced
          * @param typ the type of value, that needs to have the function applied to them
          * @param funName a reference to the function that needs to be applied to specific values
          * @return the List of Trees containing references to the passed in ValDefs
          */     
        def applyDefinedValsOfTypeTo(valDefs:List[ValDef], typ:Tree, funName:Ident ):List[Tree] = valDefs match {
            case ValDef(a,b,c,d) :: z => {
                if (c.canEqual(typ)) Apply(funName,List(Ident(b))) :: applyDefinedValsOfTypeTo(z,typ,funName)
                else Ident(b) :: applyDefinedValsOfTypeTo(z,typ,funName) 
            }
            case _                    => Nil
        }
        /** returns a List of Idents, containing references to the vales defined by the passed in ValDefs
          *
          *
          * @param valDefs the List of ValDefs that need to be referenced
          * @return the List of Trees containing references to the passed in ValDefs
          */ 
        def valDefsToValRefs(valDefs:List[ValDef]):List[Ident] = valDefs match {
            case ValDef(a,b,c,d) :: z => Ident(b) :: valDefsToValRefs(z) 
            case _                    => Nil
        }
        /** returns a List of Selects, selecting the values defined by the passed in ValDefs from the passed in name
          *
          * takes a List of ValDefs and the name of a value. returns a list of Selects (name.valueName) where name is the passed in 
          * name of a value and valueName is one of the values defined by the passed in ValDefs
          *
          * @param valDefs the List of ValDefs that need to be selected
          * @param modification name of the value, that the values are selected from
          * @return the List of Selects
          */ 
        def valDefsToSelect(valDefs:List[ValDef], modification: String):List[Select] = valDefs match {
            case ValDef(a,b,c,d) :: z => {
                val tmp=newTermName(modification); 
                q"$tmp.$b" :: valDefsToSelect(z,modification)} 
            case _                    => Nil
        }
        /** returns a List of ValDefs with an updated Type
          *
          * takes a List of ValDefs, the name of a Type and its Type Parameters as well as a new Type name. returns a list of ValDefs that is equal to the input List,
          * with the exception that ValDefs that were of the type name[types] are now of the type newType
          *
          * @param valDefs the List of ValDefs that need to be updated
          * @param name the name of the Type that needs to be replaced
          * @param types the type Parameters of the Type that needs to be replaced
          * @param newType the name of the new Type 
          * @return the List of updated ValDefs
          */ 
        def updateType(valDefs:List[ValDef], name:Tree, types:List[Tree], newType:String ):List[ValDef] = valDefs match {
            case ValDef(a,b,c,d) :: z => {
                q"class ignoreMe extends $c" match {
                    case q"class ignoreMe extends $name2[..$types2]" if(name.toString==name2.toString && types.toString==types2.toString) => ValDef(a,b,Ident(newTypeName(newType.toString)),d) :: updateType(z,name,types,newType.toString) 
                    case _ => ValDef(a,b,c,d) :: updateType(z,name,types,newType.toString)
                }
            }
            case Nil => Nil
            case _   => valDefs
        }

        val functor = q"""
            trait Functor[F[_]] {
                def fmap[A, B](f: A => B)(fa: F[A]): F[B]
            }
        """
        /*
        val listFunctor = q"""
            implicit def listFunctor: Functor[List] = new Functor[List] {
                def fmap[A, B](f: A => B)(fa: List[A]): List[B] = fa map f
            }
        """
        */
        /** returns a List of Binds, that bind references of the passed in ValDefs 
          *
          * takes a List of ValDefs, and returns a List of Binds, binding references to the values defined
          * by the passed in ValDefs to the WILDCARD, to allow the usage in case expressions
          *
          * @param valDefs the List of ValDefs that need to be Bound
          * @return the List of Binds
          */ 
        def valDefsToBinds(valDefs:List[ValDef]):List[Bind] = valDefs match{
            case ValDef(a,b,c,d) :: tail => Bind(newTermName(b.toString), Ident(nme.WILDCARD)) :: valDefsToBinds(tail)
            case Nil => Nil
        }
        /** returns a Match with a case for all the Variants
          *
          * takes a BusinessView, and creates a Match, that contains a case for all the Variants, to be used in the body of the roll function
          *
          * @param input the Business view containing all the Variants
          * @return the Match
          */ 
        def buildRollBody(input: BusinessInput):Match = {
            val cases = input.variants.map( (x:Variant) => {
                CaseDef(Apply(Ident(newTermName(x.name.toString+"F")), valDefsToBinds(x.valParams)),EmptyTree,Apply(Ident(newTermName(x.name.toString)), valDefsToValRefs(x.valParams)))
            })
            Match(Ident(newTermName("a")), cases)
        }
        /** returns a List of ValDefs with an updated Type
          *
          * takes a List of ValDefs, the name of a Type and a new Type name. returns a list of ValDefs that is equal to the input List,
          * with the exception that ValDefs that were of the type someName[passed-in-name] are now of the new type
          *
          * @param valDefs the List of ValDefs that need to be updated
          * @param oldType the name of the Type that needs to be replaced
          * @param newType the name of the new Type 
          * @return the List of updated ValDefs
          */ 
        def valDefsAdaptGeneralType(valDefs:List[ValDef],oldType:TypeName,newType:TypeName):List[ValDef] = valDefs match{
            //case _ => x
            case q"..$smth val $name: $someType[$typ]" :: tail if(typ.toString.equals(oldType.toString)) => q"val $name: $someType[$newType]" :: valDefsAdaptGeneralType(tail,oldType,newType)
            case y :: tail => y :: valDefsAdaptGeneralType(tail,oldType,newType)
            case Nil => Nil
        }
        /** returns a List of Trees applying map to references of the passed in values if they are of a specific type
          *
          * takes a List of ValDefs, and the name of a type, if a valDef is of the type someType[t] the following constuct is created:
          * sfsomeType.map.value
          * if it is of a different type, a reference to it is created.
          *
          * @param valDefs the List of ValDefs that need to be updated
          * @param t the type of ValDef that is updated 
          * @return the List Trees, containing the References or map calls
          */
        def callSfMapOnGeneral(valDefs:List[ValDef],t:TypeName):List[Tree] = valDefs match {
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => {
                val nameI = Ident(newTermName("sf"+someType.toString))
                q"$nameI.fmap(f)(${Ident(name)})" :: callSfMapOnGeneral(z,t) 
            }
            case ValDef(a,b,c,d) :: z => Ident(b) :: callSfMapOnGeneral(z,t) 
            case _                    => Nil
        }
        /*
        def createFunctors(x:List[ValDef],t:Tree):List[Tree] = x match {    
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => q"""
            implicit def ${newTermName(someType.toString+"Functor")}: Functor[$someType] = new Functor[$someType] {
                def fmap[A, B](f: A => B)(fa: $someType[A]): $someType[B] = fa map f
            }
            """ :: createFunctors(z,t) 
            case ValDef(a,b,c,d) :: z => createFunctors(z,t) 
            case Nil => Nil
        }
        */
        /** returns a List of Trees containing the names of types that need a functor
          *
          * takes a List of ValDefs, and the name of a type, if a valDef is of the type someType[t] the someType is added to the return list
          *
          * @param valDefs the List of ValDefs that need to be analysed
          * @param t the type of ValDef that we are looking for 
          * @return the List of Trees, containing the type names
          */
        def functorTypes(x:List[ValDef],t:Tree):List[Tree] = x match {    
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => someType :: functorTypes(z,t) 
            case ValDef(a,b,c,d) :: z => functorTypes(z,t) 
            case _                    => Nil
        }
        
        /** a Helper function for recursiveParamTypeApply  */
        def recursivePositionType(x:List[ValDef],t:Tree,r:String):List[ValDef] = {
            val mod = q"class Leaf[R](val tag: Int) extends TreeF[R] {}" match {
                    case q"class Leaf[R](..$vals) extends TreeF[R] {}" => vals(0) match {
                        case ValDef(a,b,c,d) => a
                    }
            }
            x match {
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => {
                q" $mod val $name: $someType[${newTypeName(r)}]" :: recursivePositionType(z,t,r)
            }
            case ValDef(a,b,c,d) :: z => ValDef(mod,b,c,d) :: recursivePositionType(z,t,r)
            case Nil => Nil

            }
        }
        
        /*
        def recursiveParamType(x:List[ValDef],t:Tree,r:String):List[ValDef] = {
            val mod = q"def apply(n:Int):Tree = new Leaf[Tree](n) with Tree" match {
                    case q"def apply(..$vals):Tree = new Leaf[Tree](n) with Tree" => vals(0) match {
                        case ValDef(a,b,c,d) => a
                    }
            }
            x match {
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => {
                q" $mod val $name: $someType[${newTypeName(r)}]" :: recursivePositionType(z,t,r)
            }
            case ValDef(a,b,c,d) :: z => ValDef(mod,b,c,d) :: recursivePositionType(z,t,r)
            case Nil => Nil

            }
        }
        */
        /** add stuff here
          */
        def recursiveParamTypeApply(valDefs:List[ValDef],t:Tree,r:String,app:List[Tree]):List[ValDef] = {
            val mod = q"def apply(n:Int):Tree = new Leaf[Tree](n) with Tree" match {
                    case q"def apply(..$vals):Tree = new Leaf[Tree](n) with Tree" => vals(0) match {
                        case ValDef(a,b,c,d) => a
                    }
            }
            valDefs match {
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => {
                q" $mod val $name: $someType[${newTypeName(r)}[..$app]]" :: recursiveParamTypeApply(z,t,r,app)//recursivePositionType(z,t,r)
            }
            case ValDef(a,b,c,d) :: z => ValDef(mod,b,c,d) :: recursiveParamTypeApply(z,t,r,app)//recursivePositionType(z,t,r)
            case _                    => Nil

            }
        }
        /** returns the toString expression for a list of params
          *
          * @param params the values that need to be printed
          * @return the toString expression
          */
        def createPrintTree(params:List[Tree]):Tree = params match{
			case head :: tail => createPrintTreeSub(q"${head}.toString",tail)
			case _ => null//throw new Exception("No params")
		}
        /** helper function for createPrintTree*/
		def createPrintTreeSub(soFar:Tree, params:List[Tree]):Tree = params match{
			case head :: tail => createPrintTreeSub(q"""${soFar} + "," + ${head}.toString""",tail)
			case _ => soFar
		}
        /** returns the equals expression for a list of params
          *
          * @param params the values that need to be equated
          * @return the equals expression
          */
		def createEqualsTree(params:List[Tree]):Tree = params match{
			case head :: tail => createEqualsTreeSub(q"this.${newTermName(head.toString)}.equals(thats.${newTermName(head.toString)})",tail)
			case _ => null
		}
        /** helper function for createEqualsTree*/
		def createEqualsTreeSub(soFar:Tree, params:List[Tree]):Tree = params match{
			case head :: tail => createEqualsTreeSub( q"${soFar} && this.${newTermName(head.toString)}.equals(thats.${newTermName(head.toString)})",tail)
			case _ => soFar
		}
        /** returns the hash expression for a list of params
          *
          * @param params the values that need to be hashed
          * @return the toString expression
          */
		def createHashTree(params:List[Tree]):Tree = params match{
			case head :: tail => createHashTreeSub(q"${newTermName(head.toString)}.hashCode()",tail)
			case _ => null
		}
        /** helper function for createHashTree*/
		def createHashTreeSub(soFar:Tree, params:List[Tree]):Tree = params match{
			case head :: tail => createHashTreeSub(q"${newTermName(head.toString)}.hashCode() + $soFar",tail)
			case _ => soFar
		}
        /*
        def countChilds(x:List[ValDef], name:Tree, types:List[Tree]):Int = x match {
            case ValDef(a,b,c,d) :: z  => {
                q"class ignoreMe extends $c" match {
                    case q"class ignoreMe extends $name2[..$types2]" if(name.toString==name2.toString && types.toString==types2.toString) => 1 + countChilds(z,name,types) 
                    case _ => 0 + countChilds(z,name,types)
                }
            }
            case Nil => 0
            case _ => 0
        }
        */
        /** returns a List of ValDefs that are not of a specific type
          *
          *
          * @param valDefs the List of ValDefs that need to be analysed
          * @param name the name of the type
          * @param types the type parameters of the type
          * @return the List of ValDefs that are not of the specified type
          */
		def nonChilds(valDefs:List[ValDef], name:Tree, types:List[Tree]):List[ValDef] = valDefs match {
            case ValDef(a,b,c,d) :: tail  => {
                q"class ignoreMe extends $c" match {
                    case q"class ignoreMe extends $name2[..$types2]" if(name.toString==name2.toString && types.toString==types2.toString) => nonChilds(tail,name,types) 
                    case _ => ValDef(a,b,c,d) :: nonChilds(tail,name,types)
                }
            }
            case _ => Nil
        }
        /*
		def createWildcards(x:List[Tree]):List[Tree] = x match {
			case TypeDef(a,b,List(),d) :: tail => Bind(tpnme.WILDCARD, EmptyTree) :: createWildcards(tail)
			case AppliedTypeTree(a,b) :: tail => Bind(tpnme.WILDCARD, EmptyTree) :: createWildcards(tail)
			case Nil => Nil
			case _ => throw new Exception("Could not create wildcards")
        }
        */
        /** returns a List of ValDefs that portray a list of implicit parameters for the passed functors
          *
          * takes a List of Trees, that contain the name of the Functor types, and creates a List of ValDefs, that have the implicit modifier,
          * the name sfname and the type name
          *
          * @param fcns the List of Type names, that we need to create a ValDef for
          * @return the List of ValDefs 
          */
        def createValsForFunctors(fcns:List[Tree]):List[ValDef] = fcns match{
            case func :: tail => {
                    val name = func.toString
                    val valName = newTermName("sf"+name)
                    val typName = newTypeName(name) 
                    q"implicit val $valName:Functor[$typName]" :: createValsForFunctors(tail)               
                }
            case _ => Nil
        }
        
        
        /** returns a List of Trees, containing the expanded traits
          *
          * see the above example, to see what a trait is expanded into
          *
          * @param input the BusinessView containing the FixedPoints
          * @return the List of Trees, containing the expanded FixedPoints
          */
        def expandTrait(input: BusinessInput):List[Tree] = {
            input.fixed.map((fix:FixedPoint) => {
                val newtrait     = newTypeName(fix.name.toString+"F")
                val oldtrait     = newTypeName(fix.name.toString)
                val newtraitTerm = newTermName(newtrait.toString)
                val traitTerm    = newTermName(fix.name.toString)
                val rollBody     = buildRollBody(input)
                val theVariants  = input.variants.filter(
                                         (v:Variant)=>v.extend.toString.equals(fix.name.toString))                      
                val theFunctors  = theVariants.map(
                                    (v:Variant) => functorTypes(v.valParams,v.extend)
                                                       ).flatten                                     
                val funcList = createValsForFunctors(theFunctors)
                //val functor = theFunctors.head  
                val fixTypeDef   = q"type R"
                val fixTypeRef   = Ident(newTypeName("R"))
                val traitTypesRef= typeDefsToTypeRefs(fix.typeParams)
                val appTrait = if (fix.typeParams.length>0) AppliedTypeTree(Ident(oldtrait),traitTypesRef)
                               else Ident(oldtrait) 
                val typeApp = fix.typeParams++List(fixTypeDef)    
                //println(showRaw(q"trait Tree extends TreeF[Tree]"))
                val tr = AppliedTypeTree(Ident(newtrait), traitTypesRef++List(appTrait))
                val tx = q"""trait Tree extends TreeF[Tree] {
                            def fold[TTT](f: $newtrait[TTT] => TTT): TTT = f(map (_ fold f))
                         }""" match {
                    case ClassDef(a,b,c,d) =>  d match {
                            case Template(x,y,z) => Template(List(tr),y,z)
                        }    
                }
                
                val typeRefA = traitTypesRef++List(Ident(newTypeName("A")))
                val typeRefB = traitTypesRef++List(Ident(newTypeName("B")))
				val typeRefR = traitTypesRef++List(Ident(newTypeName("R")))
                val lambdaTypeDefR = q"implicit def someFunctor[..${fix.typeParams}]: Functor[({ type lambda[R] = $newtrait[..$typeRefR] })#lambda] = ???" match {
                    case q"implicit def someFunctor[..${fix.typeParams}]: Functor[({ type lambda[$lambdaDefR] = $newtrait[..$typeRefR] })#lambda] = ???" => lambdaDefR match {
                        case TypeDef(a,b,c,d) => TypeDef(a,newTypeName("R"),c,d)
                        case _ => throw new Exception("Could not construct lambda TypeDef")
                    }
                    case _ => throw new Exception("Could not extract lambda TypeDef")
                }
                val func = if(fix.typeParams.length>0) q"""implicit def someFunctor[..${fix.typeParams}]: Functor[({ type lambda[$lambdaTypeDefR] = $newtrait[..$typeRefR] })#lambda] = new Functor[({ type lambda[$lambdaTypeDefR] = $newtrait[..$typeRefR] })#lambda] {
                                                                def fmap[A, B](f: A => B)(fa: $newtrait[..$typeRefA]): $newtrait[..$typeRefB] = fa map f
                                                        } """
                            else q"""implicit def patternFunctor: Functor[$newtrait] = new Functor[$newtrait] {
                                                                def fmap[A, B](f: A => B)(fa: $newtrait[A]): $newtrait[B] = fa map f
                                                        } """                            
                
                //implicit def treeFunctor[T]: Functor[({ type lambda[R] = TreeF[T, R] })#lambda] = ???
                val typeRefS = traitTypesRef++List(Ident(newTypeName("S")))
                val typeRefTTT = traitTypesRef++List(Ident(newTypeName("TTT")))
                val mapFun = if(funcList.length>0) q"def map[S](f: R => S)(implicit ..$funcList): $newtrait[..$typeRefS]"
                             else q"def map[S](f: R => S): $newtrait[..$typeRefS]"
                val erg =
                List(q"""
                    trait $newtrait[..$typeApp] {
                        ..${List(mapFun)}
                    }
                """) ++
                List(q""" 
                    trait ${fix.name}[..${fix.typeParams}] extends $tr {
                        def fold[TTT](f: $newtrait[..$typeRefTTT] => TTT): TTT = f(map (_ fold f))
                    }
                """) ++
                List(q"""
                    object $newtraitTerm {
                        ..${List(func)}
                    }
                """)
                erg
                }
            ).flatten
        }
        /** returns a List of Trees, containing the expanded classes
          *
          * see the above example, to see what a class is expanded into
          *
          * @param input the BusinessView containing the Variants
          * @return the List of Trees, containing the expanded Variants
          */ 
        def expandClasses(input: BusinessInput):List[Tree] = {
            
            input.variants.map( (vari:Variant) => {
                val oldtrait = newTypeName(vari.extend.toString)
                val newtrait = newTypeName(vari.extend.toString+"F")
                val newname  = newTypeName(vari.name.toString+"F")
                val term     = newTermName(vari.name.toString) 
                val newterm  = newTermName(vari.name.toString+"F")    
                val r        = "R"
                val rType    = newTypeName(r)
                val typ      = q"type ${newTypeName(vari.name.toString+r)}"
                val typName  = newTypeName(vari.name.toString+r)
                val parRefs  = valDefsToValRefs(vari.valParams)
                val valSel   = valDefsToSelect(vari.valParams,"b")
                //val functor  = functorTypes(vari.valParams,vari.extend).head
                
                val theVariants  = input.variants.filter(
                     (v:Variant)=>v.extend.toString.equals(vari.extend.toString))                      
                val theFunctors  = theVariants.map(
                                    (v:Variant) => functorTypes(v.valParams,v.extend)
                                                       ).flatten                                    
                val funcList = createValsForFunctors(theFunctors)
                //val functor = theFunctors.head    
                //println(functor)
                val updatedParams = recursivePositionType(vari.valParams,vari.extend,r)
                val parRefsT = valDefsToValRefs(recursivePositionType(vari.valParams,vari.extend,oldtrait.toString))
                //val updatedParamsP = recursiveParamType(vari.valParams,vari.extend,oldtrait.toString)
                
                
                val fixTypeDef = q"type R"
                val fixTypeRef = Ident(newTypeName("R"))
                val typesRef   = typeDefsToTypeRefs(vari.typeParams)
                val fixTypesDef= vari.typeParams ++ List(fixTypeDef)
                val fixTypesRef= typesRef ++ List(fixTypeRef)
                
                val updatedParamsP2 = recursiveParamTypeApply(vari.valParams,vari.extend,oldtrait.toString,typesRef)
                
                val appExtend = if (vari.typeParams.length>0) AppliedTypeTree(Ident(oldtrait),typesRef)
                                else Ident(oldtrait) 
                                
                val appExtendTypes = typesRef++List(appExtend)

                val str = createPrintTree(parRefs)
                val toStr = if(str != null)
                    q"""override def toString():String = {
                                    var str:String = ${vari.name.toString}+"("+${str} + ")"
                                    str }
                            """
                else q"""override def toString() = ${vari.name.toString+"()"}"""
                val hh = createHashTree(valDefsToValRefs(nonChilds(vari.valParams,Ident(oldtrait),typeDefsToTypeRefs(vari.typeParams))))
                val hash = if(hh != null)
                    q"""override def hashCode():Int = $hh"""
                else
                    q"""override def hashCode():Int = 0"""
                val eq = createEqualsTree(parRefs)
                val equal = if(eq != null)
                    q"""override def equals(that: Any):Boolean = {
                        if(that == null) false
                        else if (!that.isInstanceOf[${vari.name}[..$fixTypesRef]]) false
                        else {
                            val thiss = this
                            val thats = that.asInstanceOf[${vari.name}[..$fixTypesRef]]
                            $eq
                        }
                    }
                    """
                else
                    q"""override def equals(that: Any):Boolean = {
                        if(that == null) false
                        else if (!that.isInstanceOf[${vari.name}[..$fixTypesRef]]) false
                        else true
                    }
                    """
                val sTypesRef= typesRef ++ List(Ident(newTypeName("S")))
                val mapFun = if (funcList.length>0) if(vari.extendTypes.length>0) q"""def map[S](f: $rType => S)(implicit ..$funcList): $newtrait[..$sTypesRef] = 
                                                                    new ${vari.name} [..$sTypesRef](..${callSfMapOnGeneral(vari.valParams,oldtrait)})"""
                                                    else q"""def map[S](f: $rType => S)(implicit ..$funcList): $newtrait[S] = new ${vari.name} [S](..${callSfMapOnGeneral(vari.valParams,oldtrait)})"""
                             else if(vari.extendTypes.length>0) q"""def map[S](f: $rType => S): $newtrait[..$sTypesRef] = 
                                                                    new ${vari.name} [..$sTypesRef](..${callSfMapOnGeneral(vari.valParams,oldtrait)})"""
                                                    else q"""def map[S](f: $rType => S): $newtrait[S] = new ${vari.name} [S](..${callSfMapOnGeneral(vari.valParams,oldtrait)})"""
                    
                val body = toStr :: equal :: hash :: mapFun :: Nil
                
                //val uType = AppliedTypeTree(Ident(vari.name),List(Ident(rType)))//createWildcards(vari.typeParams))
                val uType = AppliedTypeTree(Ident(vari.name),fixTypesRef)
                var app = q"def apply[..${vari.typeParams}](..${updatedParamsP2}):$oldtrait[..$typesRef] = new ${Ident(vari.name)}[..$appExtendTypes](..$parRefsT) with $oldtrait[..$typesRef]"
                var unapp = q"def unapply[R](u: $newtrait[R]):Boolean"
                val newTypes2 = extractTypes(updatedParams)
                if(vari.valParams.length>1) unapp = q"""def unapply[..$fixTypesDef](u: $newtrait[..$fixTypesRef]):Option[(..${newTypes2})] = u
                match {
                    case b: $uType  => Some((..$valSel))
                    case _ => None
                }
                """
                if(vari.valParams.length==1) unapp = q"""def unapply[..$fixTypesDef](u: $newtrait[..$fixTypesRef]):Option[${newTypes2(0)}] = u match
                {
                    case b: $uType  => Some(${valSel(0)})
                    case _ => None
                }
                """
                val oBody = List(app) ++ List(unapp)
                
                List(q"""
                    class ${vari.name}[..$fixTypesDef](..$updatedParams) extends $newtrait[..$fixTypesRef] {..${body}}
                """) ++
                List(q"""
                    object $term{..${oBody}}
                """)
            }
            ).flatten
        }
        /** returns a List of Trees, containing the expanded FixedPoints and Variants
          *
          * see the above example, to see what the expansion looks like
          *
          * @param input the BusinessView containing the FixedPoints and Variants
          * @return the List of Trees, containing the expanded FixedPoints
          */
        def businessLogic(input: BusinessInput): List[Tree] = {
            expandTrait(input) ++ expandClasses(input)
        }
        /** returns a BusinessInput, containing simplified views of FixedPoints and Variants as well as the unmodified passThrough values
          *
          * see the above example, to see what a trait is expanded into
          *
          * @param raw the list of Trees, that is searched for FixedPoints and Variants
          * @return the List of Trees, containing the expanded FixedPoints
          */
        def createInput(raw: List[Tree]): BusinessInput = {
            BusinessInput(findFixedPoint(raw), findVariants(raw), findOthers(raw))
        }
        /** returns a List of Trees, containing the expanded traits
          *
          * see the above example, to see what the expansion looks like
          *
          * @param original the annotated trait or object
          * @return the List of Trees, containing the expanded FixedPoints
          */
        def createOutputTrait(original: Tree): Tree = 
            original match {
                case mod @ ClassDef(a, objectName, smth, templ) =>
                    templ match {case Template(a,b,c) => 
                        q"""
                         trait $objectName extends ..$a{
                           ..${businessLogic(createInput(extractDefList(original)))}
                        }"""
                }
                case mod @ ModuleDef(a, objectName, templ) =>
                q"""
                 object $objectName {
                   ..${businessLogic(createInput(extractDefList(original)))}
                }"""
            }
        
    
        val inputs = annottees.map(_.tree).toList
        val (annottee, expandees) = inputs match {
            case (param: ValDef) :: (rest @ (_ :: _)) => (param, rest)
            case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
            case _ => (EmptyTree, inputs)
        }      
        
        val res = createOutputTrait(expandees(0))
        //println(res)
        
        println("="*50)
        //println(showRaw(res))

        val outputs = expandees
        
        
        c.Expr[Any](Block(List(res), Literal(Constant(()))))
    }    
}