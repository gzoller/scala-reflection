package co.blocke.scala_reflection
package reflect

import scala.quoted.*
import rtypes.*
import javax.management.ReflectionException

/**
  * The goal of TypeSymbolFinder is to deep-dive through a class and find where in the type tree each type parameter is located.
  * For example:
  * 
  *    case class Bar[T]( stuff: List[T])
  *    case class Foo[A,B]( thing: A, other: Bar[B] )
  * 
  * We'd fiscover that A is found on the first argument of Foo, and that B is found by diving into Bar and ultimately List to find B.
  */
object TypeSymbolFinder:

    extension (list: List[TypeRecord])
        def indexOf2(elem: TypeSymbol): Int = {
            list.indexWhere(_.typeSymbol == elem, 0)
        }


    case class TypeRecord( 
        typeSymbol: TypeSymbol,
        path:       List[Int],
        isFound:    Boolean = false
    ):
        def pushPath(i: Int, setFound: Boolean = false): TypeRecord =
            if !isFound then
                this.copy(path = path :+ i, isFound = setFound)
            else
                this
        def popPath(): TypeRecord =
            if !isFound then
                this.copy(path = path.dropRight(1))
            else
                this

                

    def mapTypeSymbolsForClass(using quotes: Quotes)(rt: RType[_]): RType[_] = 
        import quotes.reflect.* 

        rt match {
            case sc: ScalaClassRType[_] =>
                navLevel(rt, -1, Nil) match {
                    case Right(v) => 
                        sc.copy(typeParamPaths = v.map(_.path))
                    case Left(v) => 
                        val msg: String = s"Unable to map all type symbols for class ${sc.name}: "+v.filter(_.isFound ).map(_.typeSymbol.toString).mkString("[",",","]")
                        throw new ReflectException(msg)
                }
            case _ => rt
        }

    private def navLevel(rt: RType[_], index: Int, paramList: List[TypeRecord]): Either[List[TypeRecord], List[TypeRecord]] =  // Left=>More to do, Right=>complete
        rt match {
            case ap: AppliedRType => 
                val initialList = 
                    if paramList == Nil then 
                        ap.typeParamSymbols.map(ts => TypeRecord(ts,Nil)) 
                    else 
                        paramList.map(_.pushPath(index)) // add current path to all un-found symbols
                val afterAppliedList = (0 to ap.selectLimit-1).foldLeft(initialList){ (pList, i) =>
                    navLevel(ap.select(i), i, pList) match {  // not worth the work to detect "finished"--types don't typically have more than 1 or 2 type params
                        case Left(v) => v
                        case Right(v) => v
                    }
                  }
                val cleanedList = afterAppliedList.map(_.popPath())  // revert any paths that weren't found in our deep dive into AppliedType
                var numLeftToFind = cleanedList.foldLeft(cleanedList.size){ (numLeft, rec) => if rec.isFound then numLeft-1 else numLeft }
                if numLeftToFind == 0 then
                    Right( cleanedList )
                else
                    Left( cleanedList )

            case ts: TypeSymbolRType =>
                paramList.indexOf2(ts.name.asInstanceOf[TypeSymbol]) match {
                    case i if(i >= 0 && !paramList(i).isFound) => 
                        Left(paramList.updated(i, paramList(i).pushPath(index, true)))
                    case _ => 
                        Left(paramList) // do nothing--not found, or already found
                }

            case _ => // another RType with no type parameter involvement, e.g. primitive type
                Left(paramList)
        }