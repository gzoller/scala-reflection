package co.blocke.scala_reflection //.run_util

/*
case class Foo[T](name:T)

case class Item(desc:String)
case class Person(name:String, age:Int, item:Item)

case class HasDefaults( a: String = "wow", b: Int = 5 )

case class Foom( a: BigInt, b: BigDecimal )

trait Thing[T,U]{ val name: T; val item: U }
case class ConcreteThing[A,B](name: A, item: Blather[B]) extends Thing[A,Blather[B]]

case class Blather[T](val name: T) //extends Thing[T]
case class Big(i: Int)

case class DuoTypes[Q,U](x: U, y: Q)  // Note intentional reversal of order to test proper type symbol mapping!
case class DuoHolder( a: DuoTypes[Int,Float] )

case class NestedOption(a: Option[Option[Int]], b: String)

case class Mixed[R,S](a: String, name: R|S) // extends Thing[R]


case class Envelope[T, U](id: String, body: T) {
  type Giraffe = T
  type Foo = Int
}
*/

trait T5[X, Y] { val thing1: X; val thing2: Y }
trait T10[X, Y] { val x: X; val y: Y }
trait T11[W, Z] { val w: W; val z: Z }
// case class TBlah1[A, B](w: A, z: B) extends T11[A, B]
// case class TBar7[A, B](thing1: A, thing2: B) extends T5[A, B]
// case class TFoo6[A, B, C, D](x: T11[C, T5[D, A]], y: B) extends T10[T11[C, T5[D, A]], B]


// case class Two[A,B]( w:A, z: B) extends T11[A,B]
case class One[A,B,C]( x: T11[B,T5[C,A]], y: B ) extends T10[T11[B,T5[C,A]],B]

trait TLevel0[X,Y]{ val x: X; val y: Y }
trait TLevel1[A,B]{ val a: A; val b: B }

// Q: TLevel1[R,S]
// R: Int
// S: String
// case class One[R,S,T](x: TLevel1[R,S], y: T) extends TLevel0[TLevel1[R,S],T]
case class Two[A,B](a: A, b: B) extends TLevel1[A,B]

case class Three[A,B,C]( x:C, y:(Boolean, Float, A, Char), z: B)

trait Which[X]{ val a: Either[X,Int] }
case class Arr[T]( a:Either[T,Int]) extends Which[T]


// trait X[T,U]{val t:T; val u:U}
// case class Ha[A,B,C]( a: A, b: X[B,C]) // extends X[B,C]

/*
A more involved question today...  Suppose I have these classes:
```scala
  trait TLevel0[X,Y]{ val x: X; val y: Y }
  trait TLevel1[A,B]{ val a: A; val b: B }
  case class One[R,S,T](x: TLevel1[R,S], y: T) extends TLevel0[TLevel1[R,S],T]
```

Reflect on a top-level trait of: ```TLevel0[TLevel1[Int,Boolean],String]```
correctly provides:
```
TLevel0 (trait):
   fields ->
      x: [X] TLevel1 (trait):
         fields ->
            a: [A] Int
            b: [B] Boolean
      y: [Y] String
```

I next reflect on class One (from Class.forName()), showing it's unmapped type params:
```
One[R,S,T]:
   fields ->
      x: TLevel1 (trait):
         fields ->
            a: [A] R
            b: [B] S
      y: [T] T
```

I need to express One *in terms of* ```TLevel0[TLevel1[Int,Boolean],String]``` (i.e. apply the types).
I get the TypeRepr for class One, and also a List[TypeRepr] of TLevel0's, which are:
```
> AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class scala_reflection)),class TLevel1),List(TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Int)
> TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Boolean))), TypeRef(ThisType(TypeRef(NoPrefix,module class lang)
```
This is what we'd expect, so I apply the types:
```scala
classTypeRepr.appliedTo( traitsParamReprs )
```

This provides the almost-correct answer of:
```scala
One[R,S,T]:
   fields ->
      x: TLevel1 (trait):
         fields ->
            a: [A] TLevel1 (trait):
               fields ->
                  a: [A] Int
                  b: [B] Boolean
            b: [B] String
      y: [T] T
```
You see the problem:  The application of concrete types is incomlete and wrong.
I'm getting a weird nesting of types, and the T type (String) isn't found for TLevel0/One.
Do I need to be doing something different than ```classTypeRepr.appliedTo( traitsParamReprs )```?


Thinking further---I'm getting type params from TLevel0, of which there are only 2 (for R/Int, and S/Boolean respectively).  There's no 3rd param, so nothing maps to T.  That solves the mystery, but what to do?  The compiler doesn't have a problem with this--it resolves properly in real code, so it is possible.  How can I apply types such that it can extract the R & S params from deeper in the tree, TypeLevel1 in this case?  Do I need to do all the recursion and type symbol mapping myself, or is there a cool API that does this?  I wonder if the missing piece of the puzzle is the "extends TLevel0[TLevel1[R,S],T]" on the class.  That provides the mappings--but what to do with it to make the types apply properly?
*/