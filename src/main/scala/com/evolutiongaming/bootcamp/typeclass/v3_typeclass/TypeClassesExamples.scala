package com.evolutiongaming.bootcamp.typeclass.v3_typeclass

object TypeClassesExamples extends App {

  // 1. Semigroup
  // 1.1. Implement all parts of the typeclass definition
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    def apply[A](implicit instance: Semigroup[A]): Semigroup[A] = instance
  }

  // 1.2. Implement Semigroup for Int, String
  implicit val intSemigroup: Semigroup[Int] = (x: Int, y: Int) => x + y
  implicit val stringSemigroup: Semigroup[String] = (x: String, y: String) => x + y

  // 1.3. Implement combineAll(list: List[A]) for non-empty lists
  def combineAll[A: Semigroup](list: List[A]): A = list.reduce(Semigroup[A].combine)

  //  combineAll(List(1, 2, 3)) == 6

  // 1.4. Implement combineAll(list: List[A], startingElement: A) for all lists
  def combineAll[A: Semigroup](list: List[A], startingElement: A): A =
    list.foldLeft(startingElement)(Semigroup[A].combine)

  //  println(combineAll(List(1, 2, 3), 0) == 6)
  //  println(combineAll(List(), 1) == 1)

  // 2. Monoid
  // 2.1. Implement Monoid which provides `empty` value (like startingElement in previous example) and extends Semigroup
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit instance: Monoid[A]): Monoid[A] = instance
  }

  // 2.2. Implement Monoid for Int, String
  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(x: Int, y: Int): Int = x + y
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x + y
  }

  // 2.3. Implement combineAll(list: List[A]) for all lists
  def combineAllMonoid[A: Monoid](list: List[A]): A = list.foldLeft(Monoid[A].empty)(Monoid[A].combine)

  //  println(combineAllMonoid(List(1, 2, 3)) == 6)

  // 2.4. Implement Monoid for Option[A]
  implicit def optionMonoid[A: Monoid]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def empty: Option[A] = None

    override def combine(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(x), Some(y)) => Some(Monoid[A].combine(x, y))
      case _                  => x.orElse(y)
    }
  }

  //  println(combineAllMonoid(List(Some(1), None, Some(3))).contains(4))
  //  println(combineAllMonoid(List[Option[Int]](None, None)).isEmpty)
  //  println(combineAllMonoid(List.empty[Option[Int]]).isEmpty)

  // 2.5. Implement Monoid for Function1 (for result of the function)

  implicit def functionMonoid[T, R: Monoid]: Monoid[T => R] = new Monoid[T => R] {
    override def empty: T => R = _ => Monoid[R].empty

    override def combine(x: T => R, y: T => R): T => R = t => Monoid[R].combine(x(t), y(t))
  }

  // combineAll(List((a: String) => a.length, (a: String) => a.toInt))        === (a: String) => (a.length + a.toInt)
  println(combineAll(List(
    (a: String) => a.length,
    (a: String) => a.toInt)
  ).apply("123"))

  // 3. Functor
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit class FunctorOps[F[_] : Functor, A](fa: F[A]) {
    def map[B](f: A => B): F[B] = Functor[F].map(fa)(f)
  }

  object Functor {
    def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  // 4. Semigroupal
  // 4.1. Implement Semigroupal which provides `product` method,
  // so in combination with Functor we'll be able to call for example `plus` on two Options (its content)
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  object Semigroupal {
    def apply[F[_] : Semigroupal]: Semigroupal[F] = implicitly[Semigroupal[F]]
  }

  // 4.2. Implement Semigroupal for Option (EX)
  implicit val optionSemigroupal: Semigroupal[Option] = new Semigroupal[Option] {
    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = for {
      a <- fa
      b <- fb
    } yield (a, b)
  }


  // 4.3. Implement `mapN[R](f: (A, B) => R): F[R]` extension method for Tuple2[F[A], F[B]]
  implicit class TupleOps[F[_], A, B](tuple: (F[A], F[B])) {
    def mapN[R](f: (A, B) => R)(implicit semigroupal: Semigroupal[F], functor: Functor[F]): F[R] =
      functor.map(semigroupal.product(tuple._1, tuple._2)) {
        case (a, b) => f(a, b)
      }
  }

  //   println((Option(1), Option(2)).mapN(_ + _).contains(3))
  //   println((Option(1), Option.empty[Int]).mapN(_ + _).isEmpty)


  // 4.4. Implement Semigroupal for Map

  // (Map(1 -> "a", 2 -> "b"), Map(2 -> "c")).mapN(_ + _) == Map(2 -> "bc")

  // 5. Applicative
  trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
    def pure[A](x: A): F[A]
  }

  // 5.1. Implement Applicative for Option, Either
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

  // 5.2. Implement `traverse` for all Applicatives instead of Option
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldLeft(Option(List.empty[B])) { (result, a) =>
      result match {
        case Some(value) => f(a).map(_ :: value).orElse(Option.empty[List[B]])
        case None        => Option.empty[List[B]]
      }
    }

  // traverse(List(1, 2, 3)) { i =>
  //   Option.when(i % 2 == 1)(i)
  // } == None

  // traverse(List(1, 2, 3)) { i =>
  //   Some(i + 1)
  // } == Some(List(2, 3, 4))
}
