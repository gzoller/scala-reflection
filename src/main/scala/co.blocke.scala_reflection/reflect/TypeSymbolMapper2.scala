package co.blocke.scala_reflection
package reflect

import scala.quoted.*
import scala.annotation.tailrec
import rtypeRefs.*
import rtypes.*
import javax.management.ReflectionException

/** The goal of TypeSymbolFinder is to deep-dive through a class and find where in the type tree each type parameter is located.
  * For example:
  *
  *    case class Bar[T]( stuff: List[T])
  *    case class Foo[A,B]( thing: A, other: Bar[B] )
  *
  * We'd fiscover that A is found on the first argument of Foo, and that B is found by diving into Bar and ultimately List to find B.
  */
object TypeSymbolMapper2:

  type NavPath = List[Int]

  case class TypeRecord(
      path: List[Int] = Nil,
      isFound: Boolean = false
  ):
    def pushPath(i: Int, setFound: Boolean = false): TypeRecord =
      if !isFound then this.copy(path = path :+ i, isFound = setFound)
      else this
    def popPath(): TypeRecord =
      if !isFound then this.copy(path = path.dropRight(1))
      else this

  def mapTypeSymbolsForClass(
      quotes: Quotes
  )(lookForTypeParams: List[TypeSymbol], inHere: List[quotes.reflect.TypeRepr]): Map[TypeSymbol, TypeRecord] =
    deepDive(quotes)(lookForTypeParams.map(s=>(s,TypeRecord())).toMap, inHere)

  private def deepDive(quotes: Quotes)(soFar: Map[TypeSymbol, TypeRecord], inHere: List[quotes.reflect.TypeRepr]): Map[TypeSymbol, TypeRecord] = 
    import quotes.reflect.*
    inHere.zipWithIndex.foldLeft(soFar){ case(acc,(typeRepr,i)) => 
        typeRepr match
        case AppliedType(_,tob) => 
            val allFound = acc.values.foldLeft(true){ case (acc,r) => acc && r.isFound }
            if allFound then acc
            else deepDive(quotes)(acc.map{ case(k,v) => (k,v.pushPath(i,false)) }.toMap, tob).map{ case(k,v) => (k,v.popPath()) }.toMap
        case TypeRef(_, name) => 
            if acc.contains(name.asInstanceOf[TypeSymbol]) then
                acc.updated(name.asInstanceOf[TypeSymbol], acc(name.asInstanceOf[TypeSymbol]).pushPath(i,true))
            else acc
    }

  def applyPath(quotes: Quotes)( originalSyms: List[quotes.reflect.TypeRef], args: List[quotes.reflect.TypeRepr], path: Map[TypeSymbol, TypeRecord] ): List[quotes.reflect.TypeRepr] = 
    import quotes.reflect.*
    def resolveType(record: TypeRecord): TypeRepr = 
        record.path.foldLeft(args){ case(typeArgs, i) => 
          typeArgs(i) match
            case AppliedType(_,tob) => tob
            case _ => List(typeArgs(i))
          }.head
    originalSyms.map( s =>
      path.get(s.name.asInstanceOf[TypeSymbol]).map{ p => 
        if p.isFound then resolveType(p) else s
      }.getOrElse(s)
    )