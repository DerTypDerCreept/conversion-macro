import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class convert extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro convertMacro.impl
}

object convertMacro {
    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
        import c.universe._
        
        
        
        
        //The new interface
        //represents the trait, we wish to treat as the fixed Point
        case class FixedPoint(name: TypeName, typeParams: List[TypeDef])
        //represents a case class
        //case class Variant(name: TypeName, typeParams: List[TypeDef], valParams: List[ValDef])
        case class Variant(name: TypeName, typeParams: List[TypeDef], valParams: List[ValDef], extend: Tree, extendTypes: List[Tree])
        //contains a FixedPoint and a list of variants
        //case class BusinessInput(fixed: FixedPoint, variants: List[Variant], passThrough: List[Tree])
        case class BusinessInput(fixed: List[FixedPoint], variants: List[Variant], passThrough: List[Tree])
    
        //extracts the body of a ModuleDef
        def extractDefList(x:Tree):List[Tree] = x match {
            case ModuleDef(a, b, Template(_, _, list)) => list
            case _ => throw new Exception("Could not extract Def List from"+x)
        }
        def extractDefListTrait(x:Tree):List[Tree] = x match {
            case ClassDef(a, b, c, Template(_, _, list)) => list
            case _ => throw new Exception("Could not extract Def List from"+x)
        }
        //takes a list of Trees and returns the first trait
        //as an instance of FixedPoint
        def findFixedPoint(raw: List[Tree]): List[FixedPoint] = raw match {
            case q"trait $traitname[..$types]" :: tail => FixedPoint(traitname,reconstructTypes(types).asInstanceOf[List[TypeDef]]) :: findFixedPoint(tail)
            case head :: tail => findFixedPoint(tail)
            case _ => Nil//throw new Exception("Could not find Fixed Point (no trait in annotated object)")
        }
        //takes a list of Trees  and the name of a Type
        //and filters the list for case classes extending the given type
        //turning them into Variants
        def findVariants(raw: List[Tree]): List[Variant] = raw match{
            case q"case class $name[..$types](..$fields) extends $fix[..$smth]" :: tail
              => Variant(name,reconstructTypes(types).asInstanceOf[List[TypeDef]],fields,fix,smth) :: findVariants(tail)
            case head :: tail => findVariants(tail)
            case Nil => Nil
            case _ => throw new Exception("Find Variants Malfunctioned")
        }
        def findOthers(raw: List[Tree]):List[Tree] = raw match{
            case q"case class $name[..$types](..$fields) extends $fix[..$smth]" :: tail => findOthers(tail)
            case q"trait $traitname[..$types]" :: tail => findOthers(tail)
            case q"def $init(..$smth) = {..$smth2}" :: tail => findOthers(tail)
            case head :: tail => head :: findOthers(tail)
            case Nil => Nil
        }
        //Helper Functions
        def reconstructTypes(x:List[Tree]):List[Tree] = x match{
            case TypeDef(a,b,c,d) :: tail => TypeDef(a,b,c,reconstructTypesSub(d)) :: reconstructTypes(tail)
            case AppliedTypeTree(a,b) :: tail => AppliedTypeTree(reconstructTypes(List(a)).head,reconstructTypes(b)) :: reconstructTypes(tail)
            case head :: tail => head :: reconstructTypes(tail)
            case Nil => Nil
        }
        def reconstructTypesSub(x:Tree):Tree = x match{
            case TypeBoundsTree(Select(Select(a, b), c), Select(Select(d,e), f)) => TypeBoundsTree(Select(Select(a, newTermName(b.toString)), c), Select(Select(d,newTermName(e.toString)), f))
            case _ => x        
        }

        def extractTypes(x:List[ValDef]):List[Tree] = x match{
            case ValDef(a,b,c,d) :: tail => c :: extractTypes(tail)
            case _ => x
        }
        //val defs to override
        def valDefsToOverride(x:List[ValDef]):List[ValDef] = x match{
            //case _ => x
            case q"val $name: $typ" :: z => q"override val $name: $typ" :: valDefsToOverride(z)
            case q"..$smth val $name: $typ" :: z => q"override val $name: $typ" :: valDefsToOverride(z)
            //case ValDef(a,b,c,d) :: tail => {println(showRaw(a)+"::::"+showRaw(Modifiers(Flag.OVERRIDE | Flag.CASE | Flag.PARAM))); ValDef(Modifiers(Flag.OVERRIDE | Flag.CASE | Flag.PARAM),b,c,d) :: valDefsToOverride(tail)}
            case Nil => Nil
        }
        def valDefsToNoCase(x:List[ValDef]):List[ValDef] = x match{
            //case _ => x
            case q"val $name: $typ" :: z => q"val $name: $typ" :: valDefsToNoCase(z)
            case q"..$smth val $name: $typ" :: z => q"val $name: $typ" :: valDefsToNoCase(z)
            //case ValDef(a,b,c,d) :: tail => {println(showRaw(a)+"::::"+showRaw(Modifiers(Flag.OVERRIDE | Flag.CASE | Flag.PARAM))); ValDef(Modifiers(Flag.OVERRIDE | Flag.CASE | Flag.PARAM),b,c,d) :: valDefsToOverride(tail)}
            case Nil => Nil
        }
        def valDefsToNoCasePlusVal(x:List[ValDef]):List[ValDef] = //x match{
            q"class Num[FFunctor](val n:Int) extends ExpF[FFunctor]" match{
                case q"class Num[FFunctor](..$vals) extends ExpF[FFunctor]" => vals match{
                    case ValDef(a,b,c,d) :: ignore => x match { //Modifiers(scala.reflect.internal.Flags.ACCESSOR.toLong.asInstanceOf[FlagSet] | scala.reflect.internal.Flags.PARAMACCESSOR.toLong.asInstanceOf[FlagSet])
                        case ValDef(w,x,y,z) :: tail => {/*println("!!"*38);println(a);println(showRaw(a)); */ ValDef(a,x,y,z) :: valDefsToNoCasePlusVal(tail)}
                        case Nil => Nil
                        case _ => throw new Exception("Could not convert params3")
                    }
                    case _ => throw  new Exception("Could not convert params2")
                } 
                case _ => throw  new Exception("Could not convert params1")
            }
        def valDefWithPrivate(x:List[ValDef]):List[ValDef] = {
            val privMod = q"private class ConsF[T](head: T, tail: Lists[T]) extends Cons[T, Lists[T]](head, tail) with Lists[T]" match {
                case q"private class ConsF[T]($a, tail: Lists[T]) extends Cons[T, Lists[T]](head, tail) with Lists[T]" => a match {
                        case ValDef(mods,_,_,_) => mods
                        case _ => throw new Exception("Could not Construct Private Modifier")
                }
                case _ => throw new Exception("Could not Construct Private Modifier")
            }
            x match {
                case ValDef(a,b,c,d) :: z => ValDef(privMod,b,c,d) :: valDefWithPrivate(z)
                case Nil => Nil
                case _ => throw new Exception("Could not Construct Private Modifier"+x)
            }
        }
        //clean Type takes a list of TypeDefs or AppliedTrees and
        //returns a list of references to these types
        def typeDefsToTypeRefs(x:List[Tree]):List[Tree] = x match {
            case TypeDef(a,b,List(),d) :: rest => Ident(b) :: typeDefsToTypeRefs(rest)
            case TypeDef(a,b,c,d) :: rest => AppliedTypeTree(Ident(b),typeDefsToTypeRefs(c)) :: typeDefsToTypeRefs(rest)
            case AppliedTypeTree(a,b) :: rest => AppliedTypeTree(a,typeDefsToTypeRefs(b)) :: typeDefsToTypeRefs(rest)
            case _ => x
            }
        def applyDefinedValsOfTypeTo(x:List[ValDef], typ:Tree, funName:Ident ):List[Tree] = x match {
            case ValDef(a,b,c,d) :: z => {
                if (c.canEqual(typ)) Apply(funName,List(Ident(b))) :: applyDefinedValsOfTypeTo(z,typ,funName)
                else Ident(b) :: applyDefinedValsOfTypeTo(z,typ,funName) 
            }
            case Nil => Nil
        }
        //valDefsToValRefs takes a list of valDefs and returns a list of references to the parameters
        //defined by them (wrapping their names in Idents)
        def valDefsToValRefs(x:List[ValDef]):List[Ident] = x match {
            case ValDef(a,b,c,d) :: z => Ident(b) :: valDefsToValRefs(z) 
            case Nil => Nil
        }
        //valDefsToSelect
        //takes a list of ValDefs and a String and returns a list of selects, selecting the parameters
        //defined by the ValDefs from a term with the given String as its name
        def valDefsToSelect(x:List[ValDef], modification: String):List[Select] = x match {
            case ValDef(a,b,c,d) :: z => {
                val tmp=newTermName(modification); 
                q"$tmp.$b" :: valDefsToSelect(z,modification)} 
            case Nil => Nil
        }
        //updateType
        def updateType(x:List[ValDef], name:Tree, types:List[Tree], newType:String ):List[ValDef] = x match {
            case ValDef(a,b,c,d) :: z => {
                q"class ignoreMe extends $c" match {
                    case q"class ignoreMe extends $name2[..$types2]" if(name.toString==name2.toString && types.toString==types2.toString) => ValDef(a,b,Ident(newTypeName(newType.toString)),d) :: updateType(z,name,types,newType.toString) 
                    case _ => ValDef(a,b,c,d) :: updateType(z,name,types,newType.toString)
                }
            }
            case Nil => Nil
            case _ => x
        }

        val functor = q"""
            trait Functor[F[_]] {
                def fmap[A, B](f: A => B)(fa: F[A]): F[B]
            }
        """
        val listFunctor = q"""
            implicit def listFunctor: Functor[List] = new Functor[List] {
                def fmap[A, B](f: A => B)(fa: List[A]): List[B] = fa map f
            }
        """
        
        def valDefsToBinds(x:List[ValDef]):List[Tree] = x match{
            case ValDef(a,b,c,d) :: tail => Bind(newTermName(b.toString), Ident(nme.WILDCARD)) :: valDefsToBinds(tail)
            case Nil => Nil
        }
        
        def buildRollBody(input: BusinessInput):Tree = {
            val cases = input.variants.map( (x:Variant) => {
                CaseDef(Apply(Ident(newTermName(x.name.toString+"F")), valDefsToBinds(x.valParams)),EmptyTree,Apply(Ident(newTermName(x.name.toString)), valDefsToValRefs(x.valParams)))
            })
            Match(Ident(newTermName("a")), cases)
        }
        
        def valDefsAdaptListType(x:List[ValDef],t:TypeName,newN:TypeName):List[ValDef] = x match{
            //case _ => x
            case q"..$smth val $name: List[$typ]" :: z if(typ.toString.equals(t.toString)) => q"val $name: List[$newN]" :: valDefsAdaptListType(z,t,newN)
            case y :: z => y :: valDefsAdaptListType(z,t,newN)
            case Nil => Nil
        }
        def callSfMapOnLists(x:List[ValDef],t:TypeName):List[Tree] = x match {
            case q"..$smth val $name: List[$typ]" :: z if(typ.toString.equals(t.toString)) => q"sf.fmap(f)(${Ident(name)})" :: callSfMapOnLists(z,t) 
            case ValDef(a,b,c,d) :: z => Ident(b) :: callSfMapOnLists(z,t) 
            case Nil => Nil
        }
        
        def valDefsAdaptGeneralType(x:List[ValDef],t:TypeName,newN:TypeName):List[ValDef] = x match{
            //case _ => x
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => q"val $name: $someType[$newN]" :: valDefsAdaptGeneralType(z,t,newN)
            case y :: z => y :: valDefsAdaptGeneralType(z,t,newN)
            case Nil => Nil
        }
        def callSfMapOnGeneral(x:List[ValDef],t:TypeName):List[Tree] = x match {
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => q"sf.fmap(f)(${Ident(name)})" :: callSfMapOnGeneral(z,t) 
            case ValDef(a,b,c,d) :: z => Ident(b) :: callSfMapOnGeneral(z,t) 
            case Nil => Nil
        }
        def createFunctors(x:List[ValDef],t:Tree):List[Tree] = x match {    
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => q"""
            implicit def ${newTermName(someType.toString+"Functor")}: Functor[$someType] = new Functor[$someType] {
                def fmap[A, B](f: A => B)(fa: $someType[A]): $someType[B] = fa map f
            }
            """ :: createFunctors(z,t) 
            case ValDef(a,b,c,d) :: z => createFunctors(z,t) 
            case Nil => Nil
        }
        def functorTypes(x:List[ValDef],t:Tree):List[Tree] = x match {    
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => someType :: functorTypes(z,t) 
            case ValDef(a,b,c,d) :: z => functorTypes(z,t) 
            case Nil => Nil
        }
        
        def expandTrait(input: BusinessInput):List[Tree] = {
            input.fixed.map( (fix:FixedPoint) => {
                val newtrait     = newTypeName(fix.name.toString+"F")
                val newtraitTerm = newTermName(newtrait.toString)
                val traitTerm    = newTermName(fix.name.toString)
                val rollBody     = buildRollBody(input)
				
                val theVariants  = input.variants.filter(
                                         (v:Variant)=>v.extend.toString.equals(fix.name.toString))
                //println(fix.name.toString + ":" + theVariants)                         
                val theFunctors  = theVariants.map(
                                    (v:Variant) => functorTypes(v.valParams,v.extend)
                                                       ).flatten
                //println(fix.name.toString + ":" + theFunctors)                                       
                val functor = theFunctors.head                                     
                                                       
                List(q"""
                    sealed trait ${fix.name} {
                        def unroll: $newtrait[${fix.name}]
                        def fold[T](f: $newtrait[T] => T): T = f(unroll map (_ fold f))
                    }
                """) ++ List(q"""
                    sealed trait $newtrait[R] {
                        def map[S](f: R => S)(implicit sf: Functor[$functor]): 
                            $newtrait[S]
                    }
                """) ++ List(q"""
                    object $newtraitTerm {
                        implicit def patternFunctor: Functor[$newtrait] = new Functor[$newtrait] {
                          def fmap[A, B](f: A => B)(fa: $newtrait[A]): $newtrait[B] = fa map f
                        }
                    }
                """) ++ List(q"""
                    object $traitTerm {
                        def roll(a: $newtrait[${fix.name}]): ${fix.name} = $rollBody
                    }
                """) 
                }
            ).flatten
        }


        def expandClasses(input: BusinessInput):List[Tree] = {
            
            input.variants.map( (vari:Variant) => {
                val oldtrait = newTypeName(vari.extend.toString)
                val newtrait = newTypeName(vari.extend.toString+"F")
                val newname  = newTypeName(vari.name.toString+"F")
                val term     = newTermName(vari.name.toString) 
                val newterm  = newTermName(vari.name.toString+"F")    
                val r = "R"
                val typ      = q"type ${newTypeName(vari.name.toString+r)}"
                val typName  = newTypeName(vari.name.toString+r)
                //val functor  = functorTypes(vari.valParams,vari.extend).head
                
                val theVariants  = input.variants.filter(
                     (v:Variant)=>v.extend.toString.equals(vari.extend.toString))                      
                val theFunctors  = theVariants.map(
                                    (v:Variant) => functorTypes(v.valParams,v.extend)
                                                       ).flatten                                    
                val functor = theFunctors.head                         
                
                List(q"""
                    case class ${vari.name}(..${vari.valParams}) extends ${vari.extend} { 
                        def unroll = $newterm(..${valDefsToValRefs(vari.valParams)}) 
                    }
                """) ++ List(q"""
                    case class ${newTypeName(vari.name.toString+"F")}[$typ](..${valDefsAdaptGeneralType(vari.valParams,oldtrait,typName)}) extends $newtrait[${typName}] {
                        def map[S](f: $typName => S)(implicit sf: Functor[$functor]): $newtrait[S] = $newterm(..${callSfMapOnGeneral(vari.valParams,oldtrait)})
                    }
                """)
            }
            ).flatten
            /*
            case class Variant(name: TypeName, typeParams: List[TypeDef], valParams: List[ValDef], extend: Tree, extendTypes: List[Tree])
        
            case class Leaf(tag: Int) extends Tree { 
                def unroll = LeafF(tag) 
            }
            case class Branch(children: List[Tree]) extends Tree { 
                def unroll = BranchF(children) 
            }
            case class LeafF[R](tag: Int) extends TreeF[R] {
                def map[S](f: R => S)(implicit sf: Functor[List]): TreeF[S] = LeafF(tag)
            }
            case class BranchF[R](children: List[R]) extends TreeF[R] {
                def map[S](f: R => S)(implicit sf: Functor[List]): TreeF[S] = BranchF(sf.fmap(f)(children))
            }
            */
        }
        /*
        def expand(input: BusinessInput): List[Tree] = {
            List(functor) ++ List(listFunctor) ++ expandTrait(input) ++ expandClasses(input)
        }
        */
        /**/
        def businessLogic(input: BusinessInput): List[Tree] = {
            //List(functor) ++ List(listFunctor) ++ expandTrait(input) ++ expandClasses(input)
            /*var functors:List[Tree] = Nil
            for (v <- input.variants)
                    functors = functors ++ createFunctors(v.valParams,v.extend).filter((x:Tree) => ! functors.contains(x))       
            */
			/*List(functor) ++*/ /*functors ++*/ expandTrait(input) ++ expandClasses(input)
        }
        def createInput(raw: List[Tree]): BusinessInput = {
            //println(raw)
            BusinessInput(findFixedPoint(raw), findVariants(raw), findOthers(raw))
        }
        def createOutputTrait(original: Tree): Tree = 
            original match {
                case mod @ ClassDef(a, objectName, smth, templ) =>
                //println(ClassDef(a, objectName, smth, templ)); templ match {case Template(a,b,c) => {println("a:"+a);println("b:"+b);println("c:"+c)}}
                    templ match {case Template(a,b,c) => 
                        q"""
                         trait $objectName extends ..$a{
                           ..${businessLogic(createInput(extractDefListTrait(original)))}
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
        println(res)
        println("="*50)
        //println(showRaw(res))
        val outputs = expandees
        
        c.Expr[Any](Block(List(res), Literal(Constant(()))))
    }    
}