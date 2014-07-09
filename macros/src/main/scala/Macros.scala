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
            case Nil => Nil
        }
        def valDefsToNoCase(x:List[ValDef]):List[ValDef] = x match{
            //case _ => x
            case q"val $name: $typ" :: z => q"val $name: $typ" :: valDefsToNoCase(z)
            case q"..$smth val $name: $typ" :: z => q"val $name: $typ" :: valDefsToNoCase(z)
            case Nil => Nil
        }
        def valDefsToNoCasePlusVal(x:List[ValDef]):List[ValDef] = //x match{
            q"class Num[FFunctor](val n:Int) extends ExpF[FFunctor]" match{
                case q"class Num[FFunctor](..$vals) extends ExpF[FFunctor]" => vals match{
                    case ValDef(a,b,c,d) :: ignore => x match { //Modifiers(scala.reflect.internal.Flags.ACCESSOR.toLong.asInstanceOf[FlagSet] | scala.reflect.internal.Flags.PARAMACCESSOR.toLong.asInstanceOf[FlagSet])
                        case ValDef(w,x,y,z) :: tail => {ValDef(a,x,y,z) :: valDefsToNoCasePlusVal(tail)}
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
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => {
                val nameI = Ident(newTermName("sf"+someType.toString))
                q"$nameI.fmap(f)(${Ident(name)})" :: callSfMapOnGeneral(z,t) 
            }
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
        
        def recursiveParamTypeApply(x:List[ValDef],t:Tree,r:String,app:List[Tree]):List[ValDef] = {
            val mod = q"def apply(n:Int):Tree = new Leaf[Tree](n) with Tree" match {
                    case q"def apply(..$vals):Tree = new Leaf[Tree](n) with Tree" => vals(0) match {
                        case ValDef(a,b,c,d) => a
                    }
            }
            x match {
            case q"..$smth val $name: $someType[$typ]" :: z if(typ.toString.equals(t.toString)) => {
                q" $mod val $name: $someType[${newTypeName(r)}[..$app]]" :: recursivePositionType(z,t,r)
            }
            case ValDef(a,b,c,d) :: z => ValDef(mod,b,c,d) :: recursivePositionType(z,t,r)
            case Nil => Nil

            }
        }
        
        def createPrintTree(params:List[Tree]):Tree = params match{
			case head :: tail => createPrintTreeSub(q"${head}.toString",tail)
			case Nil => null//throw new Exception("No params")
		}
		def createPrintTreeSub(soFar:Tree, params:List[Tree]):Tree = params match{
			case one :: tail => createPrintTreeSub(q"""${soFar} + "," + ${one}.toString""",tail)
			case Nil => soFar
		}
		def createEqualsTree(params:List[Tree]):Tree = params match{
			case head :: tail => createEqualsTreeSub(q"this.${newTermName(head.toString)}.equals(thats.${newTermName(head.toString)})",tail)
			case Nil => null
		}
		def createEqualsTreeSub(soFar:Tree, params:List[Tree]):Tree = params match{
			case head :: tail => createEqualsTreeSub( q"${soFar} && this.${newTermName(head.toString)}.equals(thats.${newTermName(head.toString)})",tail)
			case Nil => soFar
		}
		def createHashTree(params:List[Tree]):Tree = params match{
			case head :: tail => createHashTreeSub(q"${newTermName(head.toString)}.hashCode()",tail)
			case Nil => null
		}
		def createHashTreeSub(soFar:Tree, params:List[Tree]):Tree = params match{
			case head :: tail => createHashTreeSub(q"${newTermName(head.toString)}.hashCode() + soFar",tail)
			case Nil => soFar
		}
        
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
		def nonChilds(x:List[ValDef], name:Tree, types:List[Tree]):List[ValDef] = x match {
            case ValDef(a,b,c,d) :: z  => {
                q"class ignoreMe extends $c" match {
                    case q"class ignoreMe extends $name2[..$types2]" if(name.toString==name2.toString && types.toString==types2.toString) => nonChilds(z,name,types) 
                    case _ => ValDef(a,b,c,d) :: nonChilds(z,name,types)
                }
            }
            case _ => Nil
        }
		def createWildcards(x:List[Tree]):List[Tree] = x match {
			case TypeDef(a,b,List(),d) :: tail => Bind(tpnme.WILDCARD, EmptyTree) :: createWildcards(tail)
			case AppliedTypeTree(a,b) :: tail => Bind(tpnme.WILDCARD, EmptyTree) :: createWildcards(tail)
			case Nil => Nil
			case _ => throw new Exception("Could not create wildcards")
        }
        def createValsForFunctors(fcns:List[Tree]):List[ValDef] = fcns match{
            case func :: tail => {
                    val name = func.toString
                    val valName = newTermName("sf"+name)
                    val typName = newTypeName(name) 
                    q"implicit val $valName:Functor[$typName]" :: createValsForFunctors(tail)               
                }
            case _ => Nil
        }
        
        
        //case class FixedPoint(name: TypeName, typeParams: List[TypeDef])
        //case class Variant(name: TypeName, typeParams: List[TypeDef], valParams: List[ValDef], extend: Tree, extendTypes: List[Tree])
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
                val func = if(fix.typeParams.length>0) q"""implicit def someFunctor[..${fix.typeParams}]: Functor[$newtrait] = new Functor[$newtrait] {
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
                val updatedParamsP = recursiveParamType(vari.valParams,vari.extend,oldtrait.toString)
                
                
                val fixTypeDef = q"type R"
                val fixTypeRef = Ident(newTypeName("R"))
                val typesRef   = typeDefsToTypeRefs(vari.typeParams)
                val fixTypesDef= vari.typeParams ++ List(fixTypeDef)
                val fixTypesRef= typesRef ++ List(fixTypeRef)
                
                val updatedParamsP2 = recursiveParamTypeApply(vari.valParams,vari.extend,oldtrait.toString,typesRef)
                
                val appExtend = if (vari.typeParams.length>0) AppliedTypeTree(Ident(oldtrait),typesRef)
                                else Ident(oldtrait) 
                                
                val appExtendTypes = typesRef++List(appExtend)
                println("#"*50)
                println(fixTypesDef)
                println("#"*50)
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
                                                    else q"""def map[S](f: $rType => S)(implicit sf: Functor[$functor]): $newtrait[S] = new ${vari.name} [S](..${callSfMapOnGeneral(vari.valParams,oldtrait)})"""
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

        def businessLogic(input: BusinessInput): List[Tree] = {
            expandTrait(input) ++ expandClasses(input)
        }
        def createInput(raw: List[Tree]): BusinessInput = {
            BusinessInput(findFixedPoint(raw), findVariants(raw), findOthers(raw))
        }
        def createOutputTrait(original: Tree): Tree = 
            original match {
                case mod @ ClassDef(a, objectName, smth, templ) =>
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
        
        println("="*50)
        /*
        println(showRaw(q"""trait ConvertMe {
  trait TreeF[R] {
    def map[S](f: R => S)
      (implicit sf: Functor[List]):
        TreeF[S]
  }
  trait Tree extends TreeF[Tree]{
    def fold[T](f: TreeF[T] => T): T = f(map (_ fold f))
  }
  object TreeF {
    implicit def patternFunctor: Functor[TreeF] = new Functor[TreeF] {
      def fmap[A, B](f: A => B)(fa: TreeF[A]): TreeF[B] = fa map f
    }
  }
  class Leaf[R](val tag: Int) extends TreeF[R] {
      def map[S](f: R => S)(implicit sf: Functor[List]): TreeF[S] = new Leaf[S](tag)
  }
  object Leaf{
    def apply(tag:Int):Tree = new Leaf[Tree](tag) with Tree
    def unapply[R](u:TreeF[R]): Option[Int] = u match{
        case b:Leaf[R] => Some(b.tag)
        case _ => None
    }
  }
  class Branch[R](val children: List[R]) extends TreeF[R] {
    def map[S](f: R => S)(implicit sf: Functor[List]): TreeF[S] = new Branch[S](sf.fmap(f)(children))
  }
  object Branch{
     def apply(children:List[Tree]):Tree = new Branch[Tree](children) with Tree//R?
     def unapply[R](u:TreeF[R]): Option[List[R]] = u match{
            case b:Branch[R] => Some(b.children)
            case _ => None
    }
  }
  } """)) */
        println("="*50)
        println("="*50)
        println("="*50)
        //println(showRaw(res))
        val outputs = expandees
        
        
        c.Expr[Any](Block(List(res), Literal(Constant(()))))
    }    
}