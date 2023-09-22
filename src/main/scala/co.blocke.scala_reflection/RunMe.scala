package runnable

import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.*
// import co.blocke.scala_reflection.run_util.*
import java.io.*

object RunMe extends App:

    // val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    // val rt = RType.of[T10[T11[Int, T5[Double, Char]], String]]
    // val result = RType.inTermsOf( rt, inst.getClass.getName )

    // A: String, B: Boolean C: Int
    // val rt = RType.of[T10[T11[Boolean,T5[Int,String]],Boolean]]

    // val two = Two[Boolean,T5[Int,String]](true, TBar7(5,"foom"))
    // val one = One[String,Boolean,Int](two, false)
    // val clazz = "co.blocke.scala_reflection.One"
    // val result = RType.inTermsOf( rt, clazz )


    val cname = "co.blocke.scala_reflection.One"

    // val rt = RType.of[TLevel0[TLevel1[Int,Boolean],String]]
    // val result = RType.inTermsOf(rt, cname)

    // val rt = RType.inTermsOf(RType.of[Which[String]], "co.blocke.scala_reflection.Arr")
    // val rt = RType.of(Class.forName("co.blocke.scala_reflection.Three"))

    // println(rt.pretty())
    // println(rt)

    println(RType.of(Class.forName("co.blocke.scala_reflection.One")).pretty())
    // println(result.pretty())

    println("----------------------------\n\n")

    def foldLeftBreak2[A, B](as: List[A])(init: B)(op: (A, B) => Either[B, B]): B =
        as match {
            case Nil => 
                println("Init with: "+init)
                init
            case a :: as =>
                println("A: "+a)
                println("Rest: "+as)
                op(a, init) match {
                    case Left(b) => 
                        println("Returning left: "+b)
                        b
                    case Right(b) => 
                        println("Recursing on the right....")
                        foldLeftBreak2(as)(b)(op)
                }
        }  

    val stuff = List(23,75,99,304,-3,0)
    val res = foldLeftBreak2[Int,List[Int]](stuff)(List.empty[Int]){ (a,b) => 
        if a == 99 then
            println("Found 99!")
            Left(b)
        else
            Right(b :+ a)
    }
    println("RESULT: "+res)

    /*

trait TLevel0[X,Y]{ val x: X; val y: Y }
trait TLevel1[A,B]{ val a: A; val b: B }
case class One[Q,R,S](x: TLevel1[Q,S], y: R) extends TLevel0[TLevel1[Q,S],R]


    */