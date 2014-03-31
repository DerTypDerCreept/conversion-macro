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
		def createWildcards(x:List[Tree]):List[Tree] =
				x match {
					case TypeDef(a,b,List(),d) :: tail => Bind(tpnme.WILDCARD, EmptyTree) :: createWildcards(tail)
					case AppliedTypeTree(a,b) :: tail => Bind(tpnme.WILDCARD, EmptyTree) :: createWildcards(tail)
					case Nil => Nil
					case _ => throw new Exception("Could not create wildcards")
            }
        def expandFixedPoint(fixed: FixedPoint):List[Tree] = {
            //the additional type parameter
            val fixedType1 = q"type FFunctor"
            //the additional type parameter for the abstract map definition
            val fixedType2 = q"type FFunctor2"
            //the additional type parameter for the fold definition 
            val fixedType3 = q"type FFunctor3"
            //the name for the Fixed Point trait (original name with appended F)
            val newTraitName = newTypeName(fixed.name.toString+"F")
            //the type parameters for the Fixed Point trait
            val newTypeList = fixed.typeParams ++ List(fixedType1)
            //the type parameters of the map definition as references
            val newTypeReferences2 = typeDefsToTypeRefs(fixed.typeParams ++ List(fixedType2))
            //the type parameters of the fold function as references
            val newTypeReferences3 = typeDefsToTypeRefs(fixed.typeParams ++ List(fixedType3))
            //the map function definition
            val maps = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newTraitName[..$newTypeReferences2]"
            val newBodyFixed = List(maps)
            //the new type params of the the original trait
            val extendTypes = typeDefsToTypeRefs(fixed.typeParams ++ List(q"type ${fixed.name}[..${fixed.typeParams}]"))
            //the fold definition 
            val folds = q"def fold[$fixedType3](phi: $newTraitName[..$newTypeReferences3] => ${newTypeReferences3.last}): ${newTypeReferences3.last} = phi(this map (_ fold phi))"
            val newBody = List(folds)
            
            //the goal of all this, the fixed point trait and the original trait that now extends it
            val traitFixed = List(q"trait $newTraitName[..$newTypeList]{..$newBodyFixed}")
            val traitNormal = List(q"trait ${fixed.name}[..${fixed.typeParams}] extends $newTraitName[..$extendTypes]{..$newBody}")
            
            traitFixed ++ traitNormal
            
        }
		def expandVariant3(variant:Variant) = {
			List(createNormalClass(variant)) ++ List(createObject(variant))
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
		
		def createPrivateClass(variant:Variant):Tree = {
            //the name of the original trait (which will now extend the new trait)
            val oldtrait = newTypeName(variant.extend.toString+"")
            //the name of the case class
            val newName = newTypeName(variant.name.toString+"F")
            //the name of the normal class (and needed for the unapply fuction)
            val oldName = newTypeName(variant.name.toString+"")
            //the name of the parameters. devoid of their type, used on multiple occasions
            val paramReferences = valDefsToValRefs(variant.valParams)
            //the valParams as overrides
            val noCaseParams = valDefWithPrivate(variant.valParams)
			//original type references
            val originalTypes = typeDefsToTypeRefs(variant.typeParams)
            
			val typeRef = typeDefsToTypeRefs(List(q"type $oldtrait[..${variant.typeParams}]"))
			val typeRefs = typeDefsToTypeRefs(variant.typeParams)
			val extendTypeParams = typeRefs ++ typeRef

			val str = createPrintTree(paramReferences)
			val toStr = if(str != null)
				q"""override def toString():String = {
								var str:String = ${variant.name.toString}+"("+${str} + ")"
								str }
						"""
			else q"""override def toString() = ${variant.name.toString+"()"}"""
			
			val hh = createHashTree(valDefsToValRefs(nonChilds(variant.valParams,Ident(oldtrait),originalTypes)))
			val hash = if(hh != null)
				q"""override def hashCode():Int = $hh"""
			else
				q"""override def hashCode():Int = 0"""
			
			val eq = createEqualsTree(paramReferences)
			val equal = if(eq != null)
				q"""override def equals(that: Any):Boolean = {
					if(that == null) false
					else if (!that.isInstanceOf[$newName[..$typeRefs]]) false
					else {
						val thiss = this
						val thats = that.asInstanceOf[$newName[..$typeRefs]]
						$eq
					}
				}
				"""
			else
				q"""override def equals(that: Any):Boolean = {
					if(that == null) false
					else if (!that.isInstanceOf[$newName[..$typeRefs]]) false
					else true
				}
				"""
			val body = toStr :: equal :: hash :: Nil	
			val temp02 = q"${Ident(newTypeName(variant.name.toString))}[..$extendTypeParams]"
            val extendType = Apply(temp02,paramReferences)
			val privateClass = if(paramReferences.length<1) q"private class $newName[..${variant.typeParams}](..${noCaseParams}) extends $oldName[..$extendTypeParams] with $oldtrait[..${typeDefsToTypeRefs(variant.typeParams)}]{..${body}}"
							   else                         q"private class $newName[..${variant.typeParams}](..${noCaseParams}) extends $extendType with $oldtrait[..${typeDefsToTypeRefs(variant.typeParams)}]{..${body}}"
			
			privateClass
		}
		
		def createObject (variant:Variant):Tree = {
			//the name for the fixed point (traitnameF)
            val newtrait = newTypeName(variant.extend.toString+"F")
            //the name of the original trait (which will now extend the new trait)
            val oldtrait = newTypeName(variant.extend.toString+"")
            //The additional type parameter
            val fixedType1 = q"type FFunctor"
            //val fixedType3 = q"type FFunctor3"
            //original type references
            val originalTypes = typeDefsToTypeRefs(variant.typeParams)
            //the name of the case class
            val newName = newTypeName(variant.name.toString+"F")
            //the name of the Object
            val nameTerm = newTermName(variant.name.toString+"")
            //the name of the normal class (and needed for the unapply fuction)
            val oldName = newTypeName(variant.name.toString+"")
            //the type params for the case class
            val updatedTypeParams = variant.typeParams ++List(fixedType1)  //List(q"type FFunctor")//
            //the name of the parameters. devoid of their type, used on multiple occasions
            val paramReferences = valDefsToValRefs(variant.valParams)
            //the parameters of the case class need to be updated to the correct type
            val newParams = updateType(variant.valParams,Ident(oldtrait),originalTypes,"FFunctor")
            //the type params of the class as references for the apply and unapply functions
            val typeRefs = typeDefsToTypeRefs(variant.typeParams)
			//for the unapply function, the parameters need to be selected 
            val paramsSelect = valDefsToSelect(variant.valParams,"u")
			val updatedTypeRefs = typeDefsToTypeRefs(updatedTypeParams)
			var app = q"def apply[..${variant.typeParams}](..${variant.valParams}):$oldtrait[..$typeRefs] = new ${Ident(newName)}(..$paramReferences)"
            //if(variant.valParams.length==0) app = q"def apply[..${variant.typeParams}](..${variant.valParams}):$oldtrait[..$typeRefs] = new ${Ident(oldName)}[..$originalTypes]"
            //the unapply function of the object
			var unapp = q"def unapply[..${updatedTypeParams}](u: $newtrait[..$updatedTypeRefs]):Boolean = u.isInstanceOf[$oldName[..${typeDefsToTypeRefs(updatedTypeRefs)}]]"
			//val newTypes2 = extractTypes(variant.valParams)
			val newTypes2 = extractTypes(newParams)
			val uType = AppliedTypeTree(Ident(variant.name),createWildcards(updatedTypeParams))
			if(variant.valParams.length>1) unapp = q"""def unapply[..${updatedTypeParams}](u: $newtrait[..$updatedTypeRefs]):Option[(..${newTypes2})] = u
			match {
				case u: $uType  => Some((..$paramsSelect))
				case _ => None
			}
			"""
			if(variant.valParams.length==1) unapp = q"""def unapply[..${updatedTypeParams}](u: $newtrait[..$updatedTypeRefs]):Option[${newTypes2(0)}] = u match
			{
				case u: $uType  => Some(${paramsSelect(0)})
				case _ => None
			}
			"""

			val objBody = List(createPrivateClass(variant)) ++ List(app) ++ List(unapp)
            val obj = q"object $nameTerm {..$objBody}"
			obj
		}
		
		def createNormalClass(variant:Variant):Tree = {
			//the name for the fixed point (traitnameF)
            val newtrait = newTypeName(variant.extend.toString+"F")
            //the name of the original trait (which will now extend the new trait)
            val oldtrait = newTypeName(variant.extend.toString+"")
            //The additional type parameter
            val fixedType1 = q"type FFunctor"
            //The additional type parameter for the map functions
            val fixedType2 = q"type FFunctor2"
            //original type references
            val originalTypes = typeDefsToTypeRefs(variant.typeParams)
            //The new txpe needed for the case class
            val typeRef = typeDefsToTypeRefs(List(q"type $oldtrait[..${variant.typeParams}]"))
            //the type params for the case class
            val updatedTypeParams = variant.typeParams ++List(fixedType1)  //List(q"type FFunctor")//
            //part of the return type (and result) of the map function
            val mapType = typeDefsToTypeRefs(variant.typeParams ++ List(fixedType2)) //List(q"type FFunctor2") //
            //the name of the parameters. devoid of their type, used on multiple occasions
            val paramReferences = valDefsToValRefs(variant.valParams)
            //for the map function it is necessary to apply the function to parameters of the correct type
            val appliedParams = applyDefinedValsOfTypeTo(variant.valParams,typeRef.head,Ident(newTermName("g")))
            //the parameters of the case class need to be updated to the correct type
            val newParams = updateType(variant.valParams,Ident(oldtrait),originalTypes,"FFunctor")
           
			//count
			val children = countChilds(variant.valParams,Ident(oldtrait),originalTypes)
			
			//the name of the class
            val className = newTypeName(variant.name.toString)
			
            //the valParams as overrides
            val updatedTypeRefs = typeDefsToTypeRefs(updatedTypeParams)
			var mapFun = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newtrait[..$mapType] = new $className()" //new $nameTerm[..$mapType]()
			if(paramReferences.length!=0)
                mapFun = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newtrait[..$mapType] = new $className(..$appliedParams)"   //${Ident(newName)}(..$appliedParams)" //
            if(children==0)
				mapFun = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newtrait[..$mapType] = new $className[..$mapType](..$paramReferences)"   //${Ident(newName)}(..$appliedParams)" //
            
			val mapBody = List(mapFun)
			val normalClass = q"class ${variant.name}[..$updatedTypeParams](..${valDefsToNoCasePlusVal(newParams)}) extends $newtrait[..${updatedTypeRefs}] {..$mapBody}"
			normalClass
		}
		
		def businessLogic(input: BusinessInput): List[Tree] = {
			input.fixed.map(expandFixedPoint(_)).flatten ++ input.variants.map(expandVariant3(_)).flatten ++ input.passThrough
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
        val outputs = expandees

        c.Expr[Any](Block(List(res), Literal(Constant(()))))
    }    
}