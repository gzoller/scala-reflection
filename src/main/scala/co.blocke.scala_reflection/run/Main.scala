package co.blocke.scala_reflection
package run

import neotype.*
import rtypes.*

object Main {


  def main(args: Array[String]): Unit = println("done")

    // This is all you need to parse a NonEmptyString!
    // val t = summon[Newtype.WithType[?,NonEmptyString]]
    // println(t.make("asdf"))

    // // Writing is trivial
    // val n = NonEmptyString("boom")

    // val n: NonEmptyString.Type = ???
    // println(">> "+n.unwrap)

    // val r = RType.of[SampleNeo]
    // println(r.pretty)

    //--------------------



    // println(RType.of[Funny[Int]].pretty)
    // println("\n--------------------\n")
    // println(RType.of[Funny[Int]])

    println(RType.of[PersonZ[Boolean]].pretty)


    // PROBLEM:  When trying to marshal a full RType from a SelfRefRType, all we have is the name
    // and typedName. 

  
}


// Param
// X: TypeDef(Cmd,Template(DefDef(<init>,List(List(TypeDef(T,TypeTree[TypeBounds(TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Nothing),TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any))])), List(ValDef(item,TypeTree[TypeRef(NoPrefix,type T)],EmptyTree))),TypeTree[AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class run)),class Cmd),List(TypeRef(NoPrefix,type T)))],EmptyTree),List(TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class lang)),class Object)], TypeTree[AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class run)),trait Hoovie),List(TypeRef(ThisType(TypeRef(ThisType(TypeRef(NoPrefix,module class run)),class Cmd)),type T)))], TypeTree[TypeRef(TermRef(TermRef(NoPrefix,object _root_),object scala),trait Product)], TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class io)),trait Serializable)]),ValDef(_,EmptyTree,EmptyTree),List(TypeDef(T,TypeTree[TypeBounds(TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Nothing),TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any))]), ValDef(item,TypeTree[TypeRef(ThisType(TypeRef(ThisType(TypeRef(NoPrefix,module class run)),class Cmd)),type T)],EmptyTree), DefDef(copy,List(List(TypeDef(T,TypeTree[TypeBounds(TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Nothing),TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any))])), List(ValDef(item,TypeTree[TypeRef(NoPrefix,type T)],EmptyTree))),TypeTree[AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class run)),class Cmd),List(TypeRef(NoPrefix,type T)))],EmptyTree), DefDef(copy$default$1,List(List(TypeDef(T,TypeTree[TypeBounds(TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Nothing),TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any))]))),TypeTree[TypeRef(ThisType(TypeRef(ThisType(TypeRef(NoPrefix,module class run)),class Cmd)),type T)],EmptyTree), DefDef(_1,List(),TypeTree[TypeRef(ThisType(TypeRef(ThisType(TypeRef(NoPrefix,module class run)),class Cmd)),type T)],EmptyTree), DefDef(hashCode,List(List()),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)],EmptyTree), DefDef(equals,List(List(ValDef(x$0,TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any)],EmptyTree))),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Boolean)],EmptyTree), DefDef(toString,List(List()),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class lang)),class String)],EmptyTree), DefDef(canEqual,List(List(ValDef(that,TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any)],EmptyTree))),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Boolean)],EmptyTree), DefDef(productArity,List(),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)],EmptyTree), DefDef(productPrefix,List(),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Predef),type String)],EmptyTree), DefDef(productElement,List(List(ValDef(n,TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)],EmptyTree))),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any)],EmptyTree), DefDef(productElementName,List(List(ValDef(n,TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)],EmptyTree))),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Predef),type String)],EmptyTree))))
// No-Param
// X: TypeDef(Cmd,Template(DefDef(<init>,List(List(ValDef(item,TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)],EmptyTree))),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class run)),class Cmd)],EmptyTree),List(TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class lang)),class Object)], TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class run)),trait Hoovie)], TypeTree[TypeRef(TermRef(TermRef(NoPrefix,object _root_),object scala),trait Product)], TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class io)),trait Serializable)]),ValDef(_,EmptyTree,EmptyTree),List(ValDef(item,TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)],EmptyTree), DefDef(copy,List(List(ValDef(item,TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)],EmptyTree))),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class run)),class Cmd)],EmptyTree), DefDef(copy$default$1,List(),TypeTree[AnnotatedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int),ConcreteAnnotation(Apply(Select(New(TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class unchecked)),class uncheckedVariance)]),<init>),List())))],EmptyTree), DefDef(_1,List(),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)],EmptyTree), DefDef(hashCode,List(List()),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)],EmptyTree), DefDef(equals,List(List(ValDef(x$0,TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any)],EmptyTree))),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Boolean)],EmptyTree), DefDef(toString,List(List()),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class lang)),class String)],EmptyTree), DefDef(canEqual,List(List(ValDef(that,TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any)],EmptyTree))),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Boolean)],EmptyTree), DefDef(productArity,List(),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)],EmptyTree), DefDef(productPrefix,List(),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Predef),type String)],EmptyTree), DefDef(productElement,List(List(ValDef(n,TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)],EmptyTree))),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Any)],EmptyTree), DefDef(productElementName,List(List(ValDef(n,TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)],EmptyTree))),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Predef),type String)],EmptyTree))))