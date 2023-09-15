package runnable

import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.*
import co.blocke.scala_reflection.run_util.*


object RunMe extends App:

    // println(">> "+RType.foo[com.me.Foo[Boolean]])

    val rt = RType.of[HasDefaults]
    // rt.asInstanceOf[ScalaClassRType[_]].fields.map(f => println(s"Field ${f.name} has default: "+f.defaultValue))

    println(rt)
    println(rt.prettyPrint())

//   println(RType.boo[Int])

//   println(RType.of(Class.forName("co.blocke.scala_reflection.Foom")))
//   println(RType.of(Class.forName("co.blocke.scala_reflection.Blather")))