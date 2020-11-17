package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class SeqExtractor() extends TypeInfoExtractor[SeqLikeInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = 
    // Try here because non-library symbol won't have a class and will explode.
    val isSeq = scala.util.Try( SeqClazz.isAssignableFrom( Class.forName(symbol.fullName) ) ).toOption.getOrElse(false)
    val isSet = scala.util.Try( SetClazz.isAssignableFrom( Class.forName(symbol.fullName) ) ).toOption.getOrElse(false)
    isSeq || isSet


  def extractInfo(reflect: Reflection)(
    t: reflect.TypeRepr, 
    tob: List[reflect.TypeRepr], 
    symbol: reflect.Symbol): RType =

    val listOfType = tob.head
    val isTypeParam = listOfType.typeSymbol.flags.is(reflect.Flags.Param)
    val listOfRType = 
      if isTypeParam then
        TypeSymbolInfo(tob.head.typeSymbol.name)
      else
        RType.unwindType(reflect)(tob.head)

    SeqLikeInfo(
      t.classSymbol.get.fullName,
      listOfRType
    )
    
