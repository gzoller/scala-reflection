package co.blocke.scala_reflection
package impl

import scala.tasty.Reflection

/**

Type-Weaving is the art of extracting and re-applying parameterized types to a generic class, in terms of a
reference--a Trait in our case.

The steps involved are:

1) Walk the class hierarchy (parentage) of a subject (given) class
2) For each AppliedType (parameterized class) parent do:
    - Figure out the type mappings of the subject class in terms of the parent
    - Repeat step 2 (get parents of this parent), but resolve the relative
       mappings: subject's param, in-terms-of parent0, in-terms-of parent1

trait Message[M,N]:
  val msg: M
  val n:   N

trait CommandMessage[C,D] extends Message[List[D],C]:
  val thing: C

case class Command[X,Y,Z](
  stuff: String, 
  one: X, 
  msg: List[Y], 
  n: List[Option[Z]],
  thing: Y) extends CommandMessage[Y, Option[Z]]

  Y and Z inTermsOf CommandMessage:
      MAPPING                DE-REF ACTION
      Y -> C                 C -> Y
      Y -> Param(0)

      Option[Z] -> D         un-apply[D] -> Z
      Option[Z] -> Param(1)  un-apply[Param(1)] -> Z

  Y and Z inTermsOf Message:  
      MAPPING                        DE-REF ACTION
      Y -> N                         N -> Y
      Y -> Param(1)                  Param(1) -> Y

      List[Option[Z]] -> M           un-apply[un-apply[M]] -> Z
      List[Option[Z]] -> Param(0)    un-apply[aun-apply[Param(0)]] -> Z
 */

object TypeLoom:

  def descendParents(reflect: Reflection)( subject: reflect.Type ): Map[String, Map[String, List[Int]]] =
    import reflect._
    val classDef = subject.classSymbol.get.tree.asInstanceOf[ClassDef]
    val lookFor = {
      subject.classSymbol.get.primaryConstructor.paramSymss match {
        case List(paramSyms: List[Symbol], _) => paramSyms.map( s => subject.classSymbol.get.fullName + "." + s.name )
        case _ => Nil
      }
    }
    classDef.parents.collect{
      case t:reflect.TypeTree => t.tpe
    }.collect{
      case a: AppliedType => 
        val dadsSyms = 
          a.classSymbol.get.primaryConstructor.paramSymss match {
            case List(paramSyms: List[Symbol], _) => paramSyms.map(n => a.classSymbol.get.fullName+"."+n.name)
            case _ => Nil
          }
        val dads = descendParents(reflect)(a)
        val mine = Map(a.typeSymbol.fullName -> descendAppliedType(reflect)(a, Nil, Map.empty[String,List[Int]], lookFor.toSet))

        // Ok here we have our mappings and our parents' mappings... In terms of themselves.
        // We now have to resolve the parents' in terms of mine and add to result
        val resolvedPaths: Map[String,Map[String,List[Int]]] = mine ++ dads.map{
          case( dadsClassName, dadsMappings ) =>
            val resolved: Map[String,List[Int]] = mine.head._2.map{
              case (sym, pathHead :: pathRest) =>
                (sym, dadsMappings( dadsSyms(pathHead) ) ++ pathRest)
              case (sym, Nil) =>
                (sym, Nil) // do nothing for unmapped/unfound syms
            }.toMap
            (dadsClassName -> resolved)
        }.toMap
        mine ++ resolvedPaths
    }.foldLeft(Map.empty[String,Map[String,List[Int]]])(_ ++ _)


  def descendAppliedType(reflect: Reflection)( applied: reflect.AppliedType, pathSoFar: List[Int], foundSoFar: Map[String, List[Int]], lookFor: Set[String] ): Map[String, List[Int]] =
    import reflect._
    val AppliedType(t,tob) = applied
    tob.zipWithIndex.foldLeft( (lookFor,foundSoFar) ){ case( (look,fsf), (oneT,i) ) =>
      oneT match {
        case one if look.contains(one.typeSymbol.fullName) => 
          (look - one.typeSymbol.fullName, fsf + (one.typeSymbol.fullName -> (pathSoFar :+ i)))
        case a: AppliedType => 
          val psf = pathSoFar :+ i
          val found = descendAppliedType(reflect)(a, psf, fsf, look)
          (look -- found.keySet, found)
        case _ =>
          (look, foundSoFar) // do nothing
      }
    }._2


  object Recipe:
    // m: Map[Symbol -> List[SelectIndex]]
    def navigate( m: Map[String, List[Int]], rtype: RType ): Map[TypeSymbol, RType] =
      m.map {
        case (sym, path) =>
          ( sym.drop(sym.lastIndexOf('.')+1).asInstanceOf[TypeSymbol], path.foldLeft(rtype){ case(rt, p) => rt.asInstanceOf[AppliedRType].select(p) } )
      }.toMap
