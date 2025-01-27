package com.evolutiongaming.bootcamp.basics

import ClassesAndTraits._
import com.evolutiongaming.bootcamp.basics.ClassesAndTraits.Circle
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Gen._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import cats.implicits._

class ClassesAndTraitsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "Circle" should "be correct" in {
    val intervalGen = choose(1.0, 10)

    forAll(intervalGen, intervalGen, intervalGen) { (x, y, r) =>
      val circle = Circle(x, y, r)
      circle.minX shouldEqual x - r
      circle.maxX shouldEqual x + r
      circle.minY shouldEqual y - r
      circle.maxY shouldEqual y + r
    }
  }

  "minimumBoundingRectangle" should "be correct" in {
    val mbr = minimumBoundingRectangle(
      Set(
        Point(-12, -3),
        Point(-3, 7),
        Circle(0, 0, 5),
      )
    )

    mbr.minX shouldEqual -12
    mbr.maxX shouldEqual 5
    mbr.minY shouldEqual -5
    mbr.maxY shouldEqual 7
  }

  val stack = Stack[Int](List(1))
  "push" should "add element to the top of stack" in {
    stack.push(2) shouldEqual Stack[Int](List(2, 1))
  }

  "peep" should "return element at the top" in {
    stack.peek shouldEqual 1
  }

  "pop" should "return and remove element at top" in {
    stack.pop shouldEqual (1, Stack[Int](List()))
  }
}
