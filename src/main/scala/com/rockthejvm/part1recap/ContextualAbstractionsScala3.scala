package com.rockthejvm.part1recap

// Rarely used in ZIO context

object ContextualAbstractionsScala3 {

  // given/using combo

  def increment(x: Int)(using amount: Int): Int = x + amount
  given defaultAmount: Int = 10
  val twelve: Int = increment(2)

  def multiply(x: Int)(using factor: Int): Int = x * factor
  val hundred: Int = multiply(10)

  trait Combiner[A] {
    def combine(x: A, y: A): A
    def empty: A
  }

  // available for Lists of any type A, given a combiner of type A in scope
  def combineAll[A](values: List[A])(using combiner: Combiner[A]): A = values.foldLeft(combiner.empty)(combiner.combine)

  given intCombiner: Combiner[Int] with {
    override def combine(x: Int, y: Int): Int = x + y
    override def empty: Int = 0
  }

  val numbers: List[Int] = (1 to 10).toList
  val sumTen: Int = combineAll(numbers) // intCombiner passed automatically

  // synthesize given instances
  given optionCombiner[T](using combiner: Combiner[T]): Combiner[Option[T]] with {

    override def combine(x: Option[T], y: Option[T]): Option[T] = for {
      a <- x
      b <- y
    } yield combiner.combine(a, b)

    override def empty: Option[T] = Some(combiner.empty)
  }

  val sumOptions: Option[Int] = combineAll(List(Some(1), None, Some(2)))

  // extension methods

  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name"
  }

  // Compiler grants the method greet to Strings through this extension structure
  extension (name: String)
    def greet(): String = Person(name).greet()

  // generic extension
  // Compiler grants the method reduceAll to Lists of any type T, in the presence of a Combiner[T]
  extension [T](list: List[T])
    def reduceAll(using combiner: Combiner[T]): T =
      list.foldLeft(combiner.empty)(combiner.combine)

  // type classes (almost non-existent in ZIO)


  def main(args: Array[String]): Unit = {
    //
    println(twelve)
    println(hundred)
    println(sumTen)
    println(sumOptions)
    println("Charles".greet())
    println(numbers.reduceAll)
  }
}
