package runnable

import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.*
// import co.blocke.scala_reflection.run_util.*
import java.io.*
import co.blocke.scala_reflection.*

object RunMe extends App:

    val sample = ScalaClassRType("co.blocke.scala_reflection.Thingy2")
    // val two = RType.of[Thingy2[_]]

    // println("\n\n=======================\n"+sample.pretty())
    // println(sample)

    // val result = RType.inTermsOf[Basis[  List[Int|String]  ]](sample)
    // val result = RType.inTermsOf[Basis[  List[Option[String]]  ]](sample)
    
    // val rt = RType.of[Basis[  List[Option[Int|Boolean]]  ]]
    // println(rt)

    // val result = RType.inTermsOf[Basis[  Int|Boolean  ]](sample)
    val result = RType.inTermsOf[Basis[  List[Option[Int|Boolean]]  ]](sample)
    println(result.pretty())

    // val trt = RType.of[Basis[List[Option[Int|Boolean]]]].asInstanceOf[TraitRType[_]]
    // println("\n"+trt.pretty())
    // println((sample >> trt).pretty())



    // RType.of[Int | String]
    //OrType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int),TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Predef),type String))

    // RType.of[List[Int|String]]
    //AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class immutable)),class List),List(OrType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int),TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Predef),type String))))

    println("Done")
