package com.evolutiongaming.bootcamp.typeclass.v3_typeclass

final case class Json(s: String) { // simplified representation of JSON
  override def toString: String = s
}

object OOPJson extends App {

  trait Jsonable {
    def toJson: Json
  }

  def prettyPrint(jsonable: Jsonable): Unit = println(jsonable.toJson)

  final case class Game(id: Int) extends Jsonable {
    def toJson: Json = Json(s"{${'"'}id${'"'}:$id}")
  }

  prettyPrint(Game(123))
}

object FPJson extends App {

  trait Jsonable[A] {
    def toJson(entity: A): Json
  }

  def prettyPrint[A](a: A)(implicit jsonable: Jsonable[A]): Unit = println(jsonable.toJson(a))

  final case class Game(id: Int)

  implicit val gameJsonable: Jsonable[Game] = new Jsonable[Game] {
    def toJson(game: Game): Json = Json(s"{${'"'}id${'"'}:${game.id}}")
  }

  prettyPrint(Game(123))

  object InstancesTask { // exercise

    final case class Player(id: Int, name: String)

    implicit val playerJsonable: Jsonable[Player] = new Jsonable[Player] {
      override def toJson(player: Player): Json =
        Json(
          s"""
             |{
             | "id":${player.id}
             | "name":${player.name}
             |}
             |""".stripMargin
        )
    }

    implicit val intJsonable: Jsonable[Int] = (e: Int) => Json(e.toString)

    implicit val optionIntJsonable: Jsonable[Option[Int]] = (e: Option[Int]) => e match {
      case Some(value) => Json(value.toString)
      case None        => Json("null")
    }
  }

  object GenericImplicitsTask {

    implicit def optionJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[Option[A]] =
      (entity: Option[A]) => entity match {
        case Some(value) => jsonableA.toJson(value)
        case None        => Json("null")
      }

    implicit def listJsonable[A](implicit jsonableA: Jsonable[A]): Jsonable[List[A]] =
      (l: List[A]) => Json(l.map(jsonableA.toJson).mkString("[", ", ", "]"))

    object SingleAbstractMethod {

      implicit val before: Jsonable[Game] = new Jsonable[Game] {
        def toJson(game: Game): Json = Json(s"{${'"'}id${'"'}:${game.id}}")
      }

      implicit val after: Jsonable[Game] = (g: Game) => Json(s"{${'"'}id${'"'}:${g.id}}")
    }

    object ContextBound {

      def prettyPrintBefore[A](a: A)(implicit jsonable: Jsonable[A]): Unit = println(jsonable.toJson(a))

      def prettyPrintAfter[A: Jsonable](a: A): Unit = println(implicitly[Jsonable[A]].toJson(a))
    }

    object Summoner {

      object Jsonable {
        def apply[A](implicit instance: Jsonable[A]): Jsonable[A] = instance
      }

      def prettyPrintBefore[A: Jsonable](a: A): Unit = {
        val jsonable = implicitly[Jsonable[A]]
        println(jsonable.toJson(a))
      }

      def prettyPrintWithSummoner[A: Jsonable](a: A): Unit = println(Jsonable[A].toJson(a))
    }

    object Syntax {

      object Jsonable {
        def apply[A](implicit instance: Jsonable[A]): Jsonable[A] = instance
      }

      def prettyPrintBefore[A: Jsonable](a: A): Unit = println(Jsonable[A].toJson(a))

      object JsonableSyntax {
        implicit class JsonableOps[A](x: A) {
          def toJson(implicit jsonable: Jsonable[A]): Json = jsonable.toJson(x)
        }
      }

      import JsonableSyntax._

      def prettyPrintWithSyntax[A: Jsonable](a: A): Unit = println(a.toJson)
    }
  }
}

object FPJsonSugared extends App {

  trait Jsonable[T] {
    def toJson(entity: T): Json
  }

  object Jsonable {
    def apply[A](implicit instance: Jsonable[A]): Jsonable[A] = instance
  }

  object JsonableSyntax {
    implicit class JsonableOps[A](x: A) {
      def toJson(implicit jsonable: Jsonable[A]): Json = jsonable.toJson(x)
    }
  }

  import JsonableSyntax._

  def prettyPrint[A: Jsonable](a: A): Unit = println(a.toJson)

  final case class Game(id: Int)

  implicit val gameJsonable: Jsonable[Game] = (game: Game) => Json(s"{${'"'}id${'"'}:${game.id}}")

  prettyPrint(Game(123))
}

object FPJsonMacros extends App {

  import simulacrum._

  @typeclass trait Jsonable[T] {
    def toJson(entity: T): Json
  }

  import Jsonable.ops._

  def prettyPrint[A: Jsonable](a: A): Unit = println(a.toJson)

  final case class Game(id: Int)

  implicit val gameJsonable: Jsonable[Game] = (game: Game) => Json(s"{${'"'}id${'"'}:${game.id}}")

  prettyPrint(Game(123))
}

object HashCodeTask extends App {

  trait HashCode[T] { // Turn me into TypeClass
    def hash(entity: T): Int
  }

  object HashCode {
    def apply[A](implicit instance: HashCode[A]): HashCode[A] = instance
  }

  // Implement syntax so I could do "abc".hash
  implicit class HashCodeOps[A](x: A) {
    def hash(implicit h: HashCode[A]): Int = h.hash(x)
  }

  implicit val strHash: HashCode[String] = (entity: String) => entity.hashCode
  println("abc.".hashCode)
}