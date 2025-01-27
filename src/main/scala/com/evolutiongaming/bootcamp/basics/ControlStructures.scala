package com.evolutiongaming.bootcamp.basics

import java.io.FileNotFoundException
import java.time.Month
import java.time.format.TextStyle
import java.util.Locale
import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object ControlStructures {
  // You can follow your progress using the tests in `ControlStructuresSpec`.
  //   sbt "testOnly com.evolutiongaming.bootcamp.basics.ControlStructuresSpec"

  // The if-else construct is as follows:
  //
  // val result =
  //   if (boolean1) {
  //     result1
  //   } else if (boolean2) {
  //     result2
  //   } else {
  //     otherResult
  //   }
  //
  // Note that it returns a result value.

  // Note that curly braces can be omitted
  //  val result =
  //    if (boolean1) result1
  //    else if (boolean2) result2
  //    else otherResult

  // Exercise. Implement a "Fizz-Buzz" https://en.wikipedia.org/wiki/Fizz_buzz function using the if-else,
  // returning "fizzbuzz" for numbers which divide with 15, "fizz" for those which divide by 3 and "buzz" for
  // those which divide with 5, and returning the input number as a string for other numbers:
  def fizzBuzz1(n: Int): String = {
    if (n % 15 == 0) "fizzbuzz"
    else if (n % 3 == 0) "fizz"
    else if (n % 5 == 0) "buzz"
    else s"$n"
  }

  // Pattern Matching
  //
  // Using the match-case construct we can write constructs equivalent to if-else statements in, often,
  // a more readable and concise form:
  //
  // val result = someValue match {
  //    case pattern1                       => result1
  //    case pattern2 if (guardCondition)   => result2
  //    case _                              => fallbackResult
  // }

  type ErrorMessage = String

  def monthName(x: Int): Either[ErrorMessage, String] = {
    x match {
      case x if x <= 0 => Left(s"Month $x is too small")
      case x if x > 12 => Left(s"Month $x is too large")
      case x if x <= 12 => Right(Month.of(x).getDisplayName(TextStyle.FULL, Locale.getDefault))
    }
  }

  // Question. How would you improve `monthName`?
  // Question. What would you use in its place if you wanted to more properly handle multiple locales?

  sealed trait Shape

  object Shape {
    case object Origin extends Shape

    final case class Circle(radius: Double) extends Shape

    final case class Rectangle(width: Double, height: Double) extends Shape
  }

  import Shape._

  // Typed Pattern
  def matchOnShape1(s: Shape): String = s match {
    case Origin => s"Found the origin."
    case circle: Circle => s"Found a circle $circle."
    case rectangle: Rectangle => s"Found a rectangle $rectangle."
  }

  // Exhaustiveness checking (pay attention to compilation warning)
  def matchOnShape2(s: Shape): String = s match {
    case Origin => s"Found the origin."
    case circle: Circle => s"Found a circle $circle."
    //case rectangle: Rectangle => s"Found a rectangle $rectangle."
  }

  // Unapply the instance of Shape
  def matchOnShape3(s: Shape): String = s match {
    case Origin => s"Found the origin."
    case Circle(radius) => s"Found a circle with radius $radius."
    case Rectangle(width, height) => s"Found a rectangle with width $width and height $height."
  }

  def matchOnShape4(s: Shape): String = s match {
    case Origin => s"Found the origin."
    case circle@Circle(radius) => s"Found a circle $circle with radius $radius."
    case rectangle@Rectangle(width, height) => s"Found a rectangle $rectangle with width $width and height $height."
  }

  // Exercise. Implement a "Fizz-Buzz" function using pattern matching:
  def fizzBuzz2(n: Int): String = n match {
    case x if x % 15 == 0 => "fizzbuzz"
    case x if x % 3 == 0 => "fizz"
    case x if x % 5 == 0 => "buzz"
    case _ => s"$n"
  }

  //  List(1, 2, 3) match {
  //    case head :: tail => s"$head and $next"
  //    case Nil => "Nil"
  //  }

  // Recursion
  //
  // A function which calls itself is called a recursive function. This is a commonly used way how to
  // express looping constructs in Functional Programming languages.

  def sum1(list: List[Int]): Int =
    if (list.isEmpty) 0
    else list.head + sum1(list.tail)

  // Question. What are the risks of List#head and List#tail? How can you refactor `sum1` to avoid these invocations?
  // Better version without possible Exception
  def sum11(list: List[Int]): Int = list match {
    case head :: tail => head + sum11(tail)
    case Nil => 0
  }
  // Question. What are the risks of recursion when applied without sufficient foresight?
  // Stack overflow

  // @tailrec annotation verifies that a method will be compiled with tail call optimisation.
  @tailrec
  def last[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case x :: Nil => Some(x)
    case _ :: xs => last(xs)
  }

  // Recursion isn't used that often as it can be replaced with `foldLeft`, `foldRight`,
  // `reduce` or other larger building blocks.

  def sum2(list: List[Int]): Int =
    list.foldLeft(0)((acc, x) => acc + x)

  def sum3(list: List[Int]): Int =
    list.foldRight(0)((x, acc) => acc + x)

  def sum4(list: List[Int]): Int =
    if (list.isEmpty) 0
    else list.reduce((a, b) => a + b)

  def sum5(list: List[Int]): Int =
    list.sum // only for Numeric lists

  // Exercise: Implement a function `applyNTimes` which takes a function `f` and an integer `n` and
  // returns a function which applies the function `f` `n` times.
  //
  // Thus `applyNTimesForInts(_ + 1, 4)(3)` should return `((((3 + 1) + 1) + 1) + 1)` or `7`.
  def applyNTimesForInts(f: Int => Int, n: Int): Int => Int = { x: Int =>
    if (n > 1) applyNTimesForInts(f, n - 1)(f(x)) else f(x)
  }

  // Exercise: Convert the function `applyNTimesForInts` into a polymorphic function `applyNTimes`:
  def applyNTimes[A](f: A => A, n: Int): A => A = { x: A =>
    if (n > 1) applyNTimes(f, n - 1)(f(x)) else f(x)
  }

  // `map`, `flatMap` and `filter` are not control structures, but methods that various collections (and
  // not only collections) have. We will discuss them now as they are important to understand a key
  // control structure called "for comprehensions".

  // `map` is a higher order function which - in case of collections - transforms each element of the
  // collection into a different element (and returns the resulting collection)

  // For example, for `List` it is defined as
  object list_map_example { // name-spacing to not break other code in this lesson
    class List[A] {
      def map[B](f: A => B): List[B] = ???
    }
  }

  // Question. What is the value of this code?
  val listMapExample = List(1, 2, 3).map(x => x * 2)

  // As we will see in later lessons, `map` is a method that `Functor`-s have, and there are more `Functor`-s
  // than just collections (`IO`, `Future`, `Either`, `Option` are all `Functor`-s too).

  // For now, we will have a utilitarian focus and not go into `Functor`-s and other type classes.

  // `flatMap` is a higher order function which - for collections - transforms each element of the collection
  // into a collection, and then `flatten`-s these collections.

  // For example, for `List` it could be defined as:
  object list_flatmap_example { // name-spacing to not break other code in this lesson
    class List[A] {
      def flatMap[B](f: A => List[B]): List[B] = ???
    }
  }

  // Question. What is the value of this code?
  val listFlatMapExample = List(1, 2, 3).flatMap(x => List(x, x * 2))

  // Question. Do you think only collections can have `flatMap`?

  // `filter` takes a predicate function returning a boolean and - for collections - returns a collection
  // with only these elements which satisfy this predicate.

  // For example, for `List` it is defined as:
  object list_filter_example {
    class List[A] {
      def filter(p: A => Boolean): List[A] = ???
    }
  }

  // Question. What is the value of this code?
  val listFilterExample = List(1, 2, 3).filter(_ % 2 == 0)

  // For Comprehensions

  // A `for-yield` syntax is syntactic sugar for composing multiple `map`, `flatMap` and `filter` operations
  // together in a more readable form.

  // val result = for {
  //   x <- a
  //   y <- b
  // } yield x + y
  //
  // gets translated to
  //
  // val result = a.flatMap(x => b.map(y => x + y))

  private val a = List(1, 2, 3)
  private val b = List(10, 100)

  val c = for {
    x <- a
    y <- b
  } yield x * y

  val d = a.flatMap(x => b.map(y => x * y))

  // Question: What is the value of `c` above?
  // Question: What is the value of `d` above?

  // You can also add `if` guards to `for` comprehensions:
  val e = for {
    x <- a // generator
    z = x % 2 // definition
    if z == 1 // filter expression
    y <- b // generator
  } yield x + y

  // Question. What is the value of `e` above?

  // In idiomatic functional Scala, much of the code ends up written in "for comprehensions".
  // Exercise. Implement `makeTransfer` using `for` comprehensions and the methods provided in `UserService`.

  type UserId = String
  type Amount = BigDecimal

  trait UserService {
    def validateUserName(name: String): Either[ErrorMessage, Unit]

    def findUserId(name: String): Either[ErrorMessage, UserId]

    def validateAmount(amount: Amount): Either[ErrorMessage, Unit]

    def findBalance(userId: UserId): Either[ErrorMessage, Amount]

    /** Upon success, returns the resulting balance */
    def updateAccount(userId: UserId, previousBalance: Amount, delta: Amount): Either[ErrorMessage, Amount]
  }

  // Upon success, should return the remaining amounts on both accounts as a tuple.
  def makeTransfer(
                    service: UserService,
                    fromUserWithName: String,
                    toUserWithName: String,
                    amount: Amount
                  ): Either[ErrorMessage, (Amount, Amount)] = {
    // Replace with a proper implementation that uses validateUserName on each name,
    // findUserId to find UserId, validateAmount on the amount, findBalance to find previous
    // balances, and then updateAccount for both userId-s (with a positive and negative
    // amount, respectively):
    import service._
    for {
      _ <- validateUserName(fromUserWithName)
      _ <- validateUserName(toUserWithName)
      _ <- validateAmount(amount)
      fromId <- findUserId(fromUserWithName)
      toId <- findUserId(toUserWithName)
      fromBalance <- findBalance(fromId)
      toBalance <- findBalance(toId)
      fromUpdate <- updateAccount(fromId, fromBalance, -amount)
      toUpdate <- updateAccount(toId, toBalance, amount)
    } yield (fromUpdate, toUpdate)
  }

  // Question. What are the questions would you ask - especially about requirements - before implementing
  // this function? What issues does this implementation have?

  // Question. Does the implementation of `makeTransfer` depend on the "container" being an `Either`?

  // Let us return to our "intuition about types" exercises from before.

  // Exercise:
  //
  // Given:
  val aa = Set[Int](0, 1, 2)
  val bb = Set[Boolean](true, false)
  //
  // List all the elements in `A * B`.
  //
  // Use a "for comprehension" in your solution.

  val AProductB: Set[(Int, Boolean)] = for {
    x <- aa
    y <- bb
  } yield (x, y)

  // Exercise:
  //
  // Given:
  // A = { 0, 1, 2 }
  // B = { true, false }
  //
  // List all the elements in `A + B`.
  //
  // Use "map" and `++` (`Set` union operation) in your solution.

  val ASumB: Set[Either[Int, Boolean]] = (aa ++ bb).map(v => v match {
    case x: Boolean => Right(x)
    case y: Int => Left(y)
  }).toSet

  // Scala inherits the standard try-catch-finally construct from Java:
  def printFile(fileName: String): Unit = {
    // This code is only here to illustrate try-catch-finally, it shouldn't be considered as good code
    val source = Source.fromFile(fileName)
    try // executed until an exception happens
      source.getLines() foreach println
    catch { // exception handlers
      case e: FileNotFoundException => println(s"Couldn't find the file: $e")
      case e: Exception => println(s"Exception occurred: $e")
    } finally // executed even if an exception happens
      source.close
  }

  // Question. What issues can you find with the above `printFile` method?

  // However, in idiomatic functional Scala, other error handling mechanisms are usually used.
  // Throwing exceptions is an anti-pattern - it introduces another exit path from a function and breaks
  // referential transparency.
  // It can be thought of as a "`goto` to an unknown place up the call stack".

  // One of these other mechanisms is `Try[A]` which can be thought of as an `Either[Throwable, A]`:

  def parseInt1(x: String): Try[Int] = Try(x.toInt)

  parseInt1("asdf") match {
    case Success(value) => println(value)
    case Failure(error) => println(error)
  }

  // Question. What other ways of representing the "parse string to integer" results can you think of?
  // What are the benefits and drawbacks of each?

  // For homework, refer to `ControlStructuresHomework`
}
