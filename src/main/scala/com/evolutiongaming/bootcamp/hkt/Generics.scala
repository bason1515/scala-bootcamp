package com.evolutiongaming.bootcamp.hkt

object Generics {
  case class Triple[+A](x: A, y: A, z: A) {
    def zip[B](other: Triple[B]): Triple[(A, B)] = Triple((x, other.x), (y, other.y), (z, other.z)) // exercise 1 :  implement

    //exercise 3 (hard) : fix the definition and implement
    def set[B >: A](index: Triple.Index, value: B): Triple[B] = index match {
      case Triple.First => copy(x = value)
      case Triple.Second => copy(y = value)
      case Triple.Third => copy(z = value)
    }

  }

  object Triple {

    def fromList[A](elements: List[A]): Option[Triple[A]] = elements match {
      case List(a, b, c) => Some(Triple(a, b, c))
      case _ => None
    } // exercise 2 : implement


    sealed trait Index

    case object First extends Index

    case object Second extends Index

    case object Third extends Index
  }


  trait Walker[-A, M, +R] { // exercise 4 : fill in correct variance annotations
    def init: M

    def next(element: A, previous: M): M

    def stop(last: M): R

    def contramap[B](f: B => A): Walker[B, M, R] = new Walker[B, M, R] {
      override def init: M = Walker.this.init

      override def next(element: B, previous: M): M = Walker.this.next(f(element), previous)

      override def stop(last: M): R = Walker.this.stop(last)
    } // exercise 5 implement
  }


  trait Collection[+A] {
    def walk[M, R](walker: Walker[A, M, R]): R

    def map[B](f: A => B): Collection[B] = new Collection[B] {
      override def walk[M, R](walker: Walker[B, M, R]): R = Collection.this.walk(walker.contramap(f))
    } // exercise 6 : implement

    def flatMap[B](f: A => Collection[B]): Collection[B] = ??? // HomeWork 2 : implement
  }

  object Collection {
    def apply[A](seq: A*): Collection[A] = ??? // Homework 1: implement
  }

}


object Subkinding {

  trait Animal

  class Dog extends Animal

  class Cat extends Animal

  type >:>[+A, -B] = <:<[B, A]
  type ???[A, B] = DummyImplicit

  // sub or super 1
  implicitly[ {
    type T[+_]
  } ??? {
    type T[_]
  }]

  // sub or super 2
  implicitly[ {
    type T[_]
  } ??? {
    type T[-_]
  }]

  // sub or super 3
  implicitly[ {
    type T[_, _]
  } ??? {
    type T[-_, +_]
  }]


  // sub or super 4
  implicitly[ {
    type T[_[_]]
  } ??? {
    type T[_]
  }]

  // sub or super 5
  implicitly[ {
    type T[_[_]]
  } ??? {
    type T[_[-_]]
  }]

  // sub or super 6
  implicitly[ {
    type T[_[+_]]
  } ??? {
    type T[_[-_]]
  }]

  // sub or super 7
  implicitly[ {
    type T[_[_[+_]]]
  } ??? {
    type T[_[_[_]]]
  }]

  // sub or super 8
  implicitly[ {
    type T[_ >: Dog <: Animal]
  } ??? {
    type T[_]
  }]

  // sub or super 9
  implicitly[ {
    type T[_[_ >: Dog <: Animal]]
  } ??? {
    type T[_[_]]
  }]

  // sub or super 10
  implicitly[ {
    type T[_[x] <: Iterable[_]]
  } ??? {
    type T[_[_]]
  }]

}
