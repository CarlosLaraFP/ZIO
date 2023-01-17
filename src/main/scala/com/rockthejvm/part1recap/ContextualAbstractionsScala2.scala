package com.rockthejvm.part1recap

object ContextualAbstractionsScala2 {

  // implicit classes for extension methods of existing types without modifying them directly

  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet(): String = Person(name).greet()
  }

  val greeting: String = "Peter".greet() // Compiler: new ImpersonableString("Peter").greet()

  import scala.concurrent.duration._
  val oneSecond: FiniteDuration = 1.second

  // implicit arguments and values

  def increment(x: Int)(implicit amount: Int): Int = x + amount
  implicit val defaultAmount: Int = 10
  val twelve: Int = increment(2) // 12
  def multiply(x: Int)(implicit factor: Int): Int = x * factor
  val hundred: Int = multiply(2) // 20

  // more complex example
  trait JSONSerializer[-T] {
    def toJson(value: T): String
  }

  def convertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String = serializer.toJson(value)

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         |{ "name" : ${person.name} }
         |""".stripMargin
  }

  val davidsJson: String = convertToJson(Person("David")) // implicit serializer passed here

  // implicit def(s) to synthesize implicit values automatically
  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    new JSONSerializer[List[T]] {
      override def toJson(list: List[T]): String = s"[${list.map(serializer.toJson).mkString(",")}]".stripMargin.trim
    }

  val peopleJson: String = convertToJson(List(Person("Alice"), Person("Bob"))) // compiler instantiates implicit def

  // implicit conversions (not recommended; abuse guardrails in Scala 3)
  case class Cat(name: String) {
    def meow(): String = s"$name is meowing"
  }

  implicit def stringToCat(name: String): Cat = Cat(name)
  val aCat: Cat = "Garfield" // Compiler calls stringToCat("Garfield")
  val garfieldMeowing: String = "Garfield".meow() // Compiler calls stringToCat("Garfield").meow()


  def main(args: Array[String]): Unit = {
    //
    println(davidsJson)
    println(peopleJson)
  }
}
