package com.me

case class Foo[T](name:T)

case class Item(desc:String)
case class Person(name:String, age:Int, item:Item)