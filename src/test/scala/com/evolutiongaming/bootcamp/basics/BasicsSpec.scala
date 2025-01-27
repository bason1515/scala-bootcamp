package com.evolutiongaming.bootcamp.basics

import Basics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BasicsSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "allBooleans" should "contain all possible boolean values" in {
    allBooleans.size shouldEqual 2
    allBooleans.reduce(_ && _) shouldEqual false
    allBooleans.reduce(_ || _) shouldEqual true
  }

  "helloMethod" should "work for all strings" in {
    forAll { x: String =>
      helloMethod(x) shouldEqual s"Hello, $x!"
    }
  }

  "helloFunction" should "work for all strings" in {
    forAll { x: String =>
      helloFunction(x) shouldEqual s"Hello, $x!"
    }
  }

  "stringLength" should "work for all strings" in {
    forAll { x: String =>
      stringLength(x) shouldEqual x.length
    }
  }

  "add" should "add 2 and 3" in {
    add(2, 3) shouldEqual 5
  }

  "addNTimes2" should "add 2 , 4 and multiply by default value" in {
    addNTimes2(x = 2, y = 4) shouldEqual 6
  }

  it should "work for all numbers" in {
    forAll { (a: Int, b: Int) =>
      add(a, b) shouldEqual a + b
    }
  }

  "power" should "be correct" in {
    forAll { n: Int =>
      power(2)(n) shouldEqual Math.pow(n.toDouble, 2)
    }

    forAll { n: Byte =>
      power(3)(n.toInt) shouldEqual Math.pow(n.toDouble, 3)
    }
  }

  "allOptionBooleans" should "be correct" in {
    allOptionBooleans.size shouldEqual 3
  }

  "allEitherUnitBooleans" should "be correct" in {
    allEitherUnitBooleans.size shouldEqual 3
  }

  "allEitherBooleanBooleans" should "be correct" in {
    allEitherBooleanBooleans.size shouldEqual 4
  }

  "allTupleBooleanBooleans" should "be correct" in {
    allTupleBooleanBooleans.size shouldEqual 4
  }

  "gcd" should "be correct" in {
    gcd(54, 24) shouldEqual 6
    gcd(48, 18) shouldEqual 6
    gcd(4, 2) shouldEqual 2
  }

  "lcm" should "be correct" in {
    lcm(4, 6) shouldEqual 12
    lcm(4, 8) shouldEqual 8
    lcm(10, 10) shouldEqual 10
    lcm(3, 7) shouldEqual 21
    lcm(5, 1000001) shouldEqual 5000005
  }
}

