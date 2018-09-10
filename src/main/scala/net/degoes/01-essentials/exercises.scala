// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.essentials

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.Date

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Try

object types {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // List all values of the type `Boolean`.
  //
  val BoolValues: List[Boolean] = true :: false :: Nil

  //
  // EXERCISE 2
  //
  // List all values of the type `Either[Unit, Boolean]`.
  //
  val EitherUnitBoolValues: List[Either[Unit, Boolean]] = Right(true) :: Right(false) :: Left(()) :: Nil

  //
  // EXERCISE 3
  //
  // List all values of the type `(Boolean, Boolean)`.
  //
  val TupleBoolBoolValues: List[(Boolean, Boolean)] =
    (true, true) :: (true, false) :: (false, true) :: (false, false) :: Nil

  //
  // EXERCISE 4
  //
  // List all values of the type `Either[Either[Unit, Unit], Unit]`.
  //
  val EitherEitherUnitUnitUnitValues: List[Either[Either[Unit, Unit], Unit]] =
    Right(()) :: Left(Right(())) :: Left(Left(())) :: Nil // is isomorphic to EitherUnitBoolValues (same mathematical structure)

  //
  // EXERCISE 5
  //
  // Create a product type of `Int` and `String`, representing the age and
  // name of a person.
  //
  type Person1 = (Int, String)
  final case class Person2(age: Int, name: String)

  //
  // EXERCISE 6
  //
  // Prove that `A * 1` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to1[A](t: (A, Unit)): A   = t._1
  def from1[A](a: A): (A, Unit) = (a, ())

  //
  // EXERCISE 7
  //
  // Create a sum type of `Int` and `String` representing the identifier of
  // a robot (a number) or a person (a name).
  //
  type Identifier = Either[Int, String]

  sealed trait Identifier2
  final case class RobotId(id: Int)       extends Identifier2
  final case class PersonId(name: String) extends Identifier2

  //
  // EXERCISE 8
  //
  // Prove that `A + 0` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to2[A](t: Either[A, Nothing]): A   = t.left.get
  def from2[A](a: A): Either[A, Nothing] = Left(a)

  //
  // EXERCISE 9
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // credit card, which has a number, an expiration date, and a security code.
  //
  type CreditCard = (Int, Date, String)

  //
  // EXERCISE 10
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // payment method, which could be a credit card, bank account, or
  // cryptocurrency.
  //
  sealed trait PaymentMethod
  case object CreditCart     extends PaymentMethod
  case object BankAccount    extends PaymentMethod
  case object Cryptocurrency extends PaymentMethod

  //
  // EXERCISE 11
  //
  // Create either a sum type or a product type (as appropriate) to represent an
  // employee at a company, which has a title, salary, name, employment date.
  //
  type Employee = (String, Int, String, Date)

  //
  // EXERCISE 12
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // piece on a chess board, which could be a pawn, rook, bishop, knight,
  // queen, or king.
  //
  sealed trait ChessPiece
  case object Pawn   extends ChessPiece
  case object Rook   extends ChessPiece
  case object Bishop extends ChessPiece
  case object Knight extends ChessPiece
  case object Queen  extends ChessPiece
  case object King   extends ChessPiece

  //
  // EXERCISE 13
  //
  // Create an ADT model of a game world, including a map, a player, non-player
  // characters, different classes of items, and character stats.
  //
  type Map = Array[Array[(Int, Int)]]

  sealed trait Character2
  case class Player2()    extends Character2
  case class NonPlayer2() extends Character2

  sealed trait Item

  type CharacterStats = (Character, List[Int])

  type GameWorld = (Map, Player2, List[NonPlayer2], List[Item], List[CharacterStats])

  // JDG implementation

  case class JDGGameWorld(
      map: GameMap,
  )

  case class GameMap(realms: List[Realm], paths: List[(RealmId, RealmId)])

  class RealmId /* private */ (val value: Int) extends AnyVal // Private is commented because with it, it doesn't compile anymore.
  object RealmId {
    def apply(value: Int): Option[RealmId] = ???
  }

  case class Realm(id: RealmId, realmType: RealmType, inv: List[Item], chars: List[Character])

  sealed trait RealmType
  case object Plains    extends RealmType
  case object Dungeons  extends RealmType
  case object Highlands extends RealmType

  case class Character(inv: List[Item], charType: CharType)

  sealed trait CharType
  case class Player(id: String)          extends CharType
  case class NonPlayer(npcType: NPCType) extends CharType

  sealed trait NPCType
  case object Ogre   extends NPCType
  case object Troll  extends NPCType
  case object Wizard extends NPCType

}

object functions {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // Convert the following non-function into a function.
  //
  def parseInt1(s: String): Int         = s.toInt               // impure. Lying function.
  def parseInt2(s: String): Option[Int] = Try(s.toInt).toOption // Pure

  //
  // EXERCISE 2
  //
  // Convert the following non-function into a function.
  //
  def arrayUpdate1[A](arr: Array[A], i: Int, f: A => A): Unit = arr.updated(i, f(arr(i)))
  def arrayUpdate2[A: ClassTag](arr: Array[A], i: Int, f: A => A): Array[A] = // TODO: possible without shitty `ClassTag` thing ?
    arr.zipWithIndex
      .foldLeft(ArrayBuffer.empty[A]) {
        case (acc, (a, index)) => acc += (if (index == i) f(a) else a)
      }
      .toArray
  def arrayUpdate3[A](arr: Array[A], i: Int, f: A => A): Option[Array[A]] = ???

  //
  // EXERCISE 3
  //
  // Convert the following non-function into a function.
  //
  def divide1(a: Int, b: Int): Int         = a / b
  def divide2(a: Int, b: Int): Option[Int] = if (b == 0) None else Some(a / b)

  //
  // EXERCISE 4
  //
  // Convert the following non-function into a function.
  //
  var id = 0
  def freshId1(): Int = {
    val newId = id
    id += 1
    newId
  }
  def freshId2(id: Int): (Int, Int) = (id, id + 1)

  //
  // EXERCISE 5
  //
  // Convert the following non-function into a function.
  //
  import java.time.LocalDateTime
  def afterOneHour1: LocalDateTime                     = LocalDateTime.now.plusHours(1)
  def afterOneHour2(now: LocalDateTime): LocalDateTime = now.plusHours(1)

  //
  // EXERCISE 6
  //
  // Convert the following non-function into function.
  //
  def head1[A](as: List[A]): A = {
    if (as.length == 0) println("Oh no, it's impossible!!!")
    as.head
  }
  def head2[A](as: List[A]): Option[A] = as.headOption

  //
  // EXERCISE 7
  //
  // Convert the following non-function into a function.
  //
  trait Account
  trait Processor {
    def charge(account: Account, amount: Double): Unit
  }
  case class Coffee() {
    val price = 3.14
  }
  def buyCoffee1(processor: Processor, account: Account): Coffee = {
    val coffee = Coffee()
    processor.charge(account, coffee.price)
    coffee
  }

  final case class Charge(account: Account, amount: Double)
  def buyCoffee2(account: Account): (Coffee, Charge) = {
    val coffee = Coffee()
    (coffee, Charge(account, coffee.price))
  }

  /**
    * ### Scalazzi
    * 1. Functions        (only use functions, if it's not a function, don't use it)
    * 2. No poly methods  (don't use methods from AnyRef / Java "Object" etc, don't use methods on polymorphic objects)
    * 3. No null          (never use null)
    * 4. No RTTI          (no Runtime Type information/identification, TypeTags etc...)
    */
  //
  // EXERCISE 8
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def printLine(line: String): Unit = ()

  //
  // EXERCISE 9
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  val readLine: String = "any string"

  //
  // EXERCISE 10
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def systemExit(code: Int): Unit = ()

  //
  // EXERCISE 11
  //
  // Rewrite the following non-function `printer1` into a pure function, which
  // could be used by pure or impure code.
  //
  def printer1(): Unit = {
    println("Welcome to the help page!")
    println("To list commands, type `commands`.")
    println("For help on a command, type `help <command>`")
    println("To exit the help page, type `exit`.")
  }
  def printer2[A](println: String => A, combine: (A, A) => A): A =
    List(
      "Welcome to the help page!",
      "To list commands, type `commands`.",
      "For help on a command, type `help <command>`",
      "To exit the help page, type `exit`."
    ).map(println).reduce(combine)

  //
  // EXERCISE 12
  //
  // Create a purely-functional drawing library that is equivalent in
  // expressive power to the following procedural library.
  //
  trait Draw {
    def goLeft(): Unit
    def goRight(): Unit
    def goUp(): Unit
    def goDown(): Unit
    def draw(): Unit
    def finish(): List[List[Boolean]]
  }
  def draw1(size: Int): Draw =
    new Draw {
      val canvas: Array[Array[Boolean]] = Array.fill(size, size)(false)
      var x                             = 0
      var y                             = 0

      def goLeft(): Unit  = x -= 1
      def goRight(): Unit = x += 1
      def goUp(): Unit    = y += 1
      def goDown(): Unit  = y -= 1
      def draw(): Unit = {
        def wrap(x: Int): Int = if (x < 0) (size - 1) + ((x + 1) % size) else x % size

        val x2 = wrap(x)
        val y2 = wrap(y)

        canvas.updated(x2, canvas(x2).updated(y2, true))
      }
      def finish(): List[List[Boolean]] = canvas.map(_.toList).toList
    }

  /** Initial JDG impl
    *
    * type Bitmap    = List[List[Boolean]]
    * type Cursor    = (Int, Int)
    * type Operation = (Cursor, Bitmap) => (Cursor, Bitmap)
    *
    * val draw: Operation    = (c, b) => (c, ???)
    * val goRight: Operation = (c, b) => ((c._1 + 1, c._2), b)
    * val goLeft: Operation  = (c, b) => ((c._1 - 1, c._2), b)
    * val goUp: Operation    = (c, b) => ((c._1, c._2 + 1), b)
    * val goDown: Operation  = (c, b) => ((c._1, c._2 - 1), b)
    *
    * goLeft.andThen(goRight).andThen(draw)
    *
    * def draw2(size: Int, op: Operation): Bitmap = op((0, 0), List.fill(size, size)(false))
    */
  type Bitmap = List[List[Boolean]]
  type Cursor = (Int, Int)

  sealed trait Operation
  case object GoLeft  extends Operation
  case object GoRight extends Operation
  case object GoUp    extends Operation
  case object GoDown  extends Operation
  case object Draw    extends Operation

  // TODO: Is this correct ?
  def draw2(size: Int, op: List[Operation]): Bitmap = {
    def wrap(x: Int): Int = if (x < 0) (size - 1) + ((x + 1) % size) else x % size

    op.foldLeft((Array.fill[Boolean](size, size)(false), (0, 0))) {
        case ((bitmapAcc: Array[Array[Boolean]], (x, y)), operation) =>
          operation match {
            case GoLeft  => (bitmapAcc, (x + 1, y))
            case GoRight => (bitmapAcc, (x - 1, y))
            case GoUp    => (bitmapAcc, (x, y + 1))
            case GoDown  => (bitmapAcc, (x, y - 1))
            case Draw    =>
              val newX = wrap(x)
              val newY = wrap(y)

              bitmapAcc(newX)(newY) = true
              (bitmapAcc, (newX, newY))
          }
      }
      ._1
      .map(_.toList)
      .toList
  }

}

object higher_order {
  case class Parser[+E, +A](run: String => Either[E, (String, A)])

  def fail[E](e: E): Parser[E, Nothing]     = Parser(_ => Left(e))
  def point[A](a: => A): Parser[Nothing, A] = Parser(input => Right((input, a)))

  def char[E](e: E): Parser[E, Char] =
    Parser(
      input =>
        input.headOption match {
          case None       => Left(e)
          case Some(char) => Right((input.drop(1), char))
        }
    )

  //
  // EXERCISE 1
  //
  // Implement the following higher-order function.
  //
  def alt[E1, E2, A, B](l: Parser[E1, A], r: Parser[E2, B]): Parser[E2, Either[A, B]] =
    Parser(
      input =>
        l.run(input) match {
          case Right((string, a)) => Right(string -> Left(a))
          case Left(_) =>
            r.run(input) match {
              case Left(e2)           => Left(e2)
              case Right((string, b)) => Right(string -> Right(b))
            }
        }
    )

  //
  // EXERCISE 2
  //
  // Implement the following higher-order function.
  //
  def fanout[A, B, C](f: A => B, g: A => C): A => (B, C) = a => f(a) -> g(a)

  //
  // EXERCISE 3
  //
  // Implement the following higher-order function.
  //
  def cross[A, B, C, D](f: A => B, g: C => D): (A, C) => (B, D) = (a, c) => f(a) -> g(c)

  //
  // EXERCISE 4
  //
  // Implement the following higher-order function.
  //
  def either[A, B, C](f: A => B, g: C => B): Either[A, C] => B = {
    case Right(c) => g(c)
    case Left(a)  => f(a)
  }

  //
  // EXERCISE 5
  //
  // Implement the following higher-order function.
  //
  def choice[A, B, C, D](f: A => B, g: C => D): Either[A, C] => Either[B, D] = {
    case Right(c) => Right(g(c))
    case Left(a)  => Left(f(a))
  }

  //
  // EXERCISE 6
  //
  // Implement the following higher-order function.
  //
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}

object poly_functions {
  //
  // EXERCISE 1
  //
  // Create a polymorphic function of two type parameters `A` and `B` called
  // `snd` that returns the second element out of any pair of `A` and `B`.
  //
  object snd {
    def apply[A, B](a: A, b: B): B = b
  }
  // snd(1, "foo") // "foo"

  //
  // EXERCISE 2
  //
  // Create a polymorphic function called `repeat` that can take any
  // function `A => A`, and apply it repeatedly to a starting value
  // `A` the specified number of times.
  //
  object repeat {
    @tailrec
    def apply[A](time: Int)(a: A, f: A => A): A = if (time == 0) a else repeat(time - 1)(f(a), f)
  }
  // repeat[Int](100)(0, _ + 1) // 100
  // repeat[String](10)("", _ + "*") // "**********"

  //
  // EXERCISE 3
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample1[A, B](a: A, b: B): Either[A, B] = Left(a)
  def countExample2[A, B](a: A, b: B): Either[A, B] = Right(b)
  val countExample1Answer                           = ???

  //
  // EXERCISE 4
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample2[A, B](f: A => B, g: A => B, a: A): B = f(a)
  def countExample3[A, B](f: A => B, g: A => B, a: A): B = g(a)
  val countExample2Answer                                = ???

  //
  // EXERCISE 5
  //
  // Implement the function `groupBy`.
  //
  val Data: List[String] =
    "poweroutage;2018-09-20;level=20" :: Nil
  val By: String => String =
    (data: String) => data.split(";")(1)
  val Reducer: (String, List[String]) => String =
    (date, events) =>
      "On date " +
        date + ", there were " +
        events.length + " power outages"
  val Expected =
    Map(
      "2018-09-20" ->
        "On date 2018-09-20, there were 1 power outages"
    )
  def groupBy1(l: List[String], by: String => String)(reducer: (String, List[String]) => String): Map[String, String] =
    l.groupBy(by).map { case (date, events) => date -> reducer(date, events) }
  // groupBy1(Data, By)(Reducer) == Expected

  //
  // EXERCISE 6
  //
  // Make the function `groupBy1` as polymorphic as possible and implement
  // the polymorphic function. Compare to the original.
  //
  object groupBy2 {
    def apply[A, B, C](l: List[A], by: A => B)(reducer: (B, List[A]) => C): Map[B, C] =
      l.groupBy(by).map { case (date, events) => date -> reducer(date, events) }
  }

  // TODO: Rewrite it with Scalaz groupBy.
}

object higher_kinded {
  type ??          = Nothing
  type ???[A]      = Nothing
  type ????[A, B]  = Nothing
  type ?????[F[_]] = Nothing

  trait `* => *`[F[_]]
  trait `[*, *] => *`[F[_, _]]
  trait `(* => *) => *`[T[_[_]]]

  /**
    * Int   : *
    * List  : * => *
    * Map   : [*, *] => *
    * Tuple3: [*, *, *] => *
    *
    * def foo[A[_[_, _[_], _], _]: A[_, Int] // [[*, * => *, *] => *, *] => *
    *
    */
  //
  // EXERCISE 1
  //
  // Identify a type constructor that takes one type parameter (i.e. has kind
  // `* => *`), and place your answer inside the square brackets.
  //
  type Answer1 = `* => *`[List]

  //
  // EXERCISE 2
  //
  // Identify a type constructor that takes two type parameters (i.e. has kind
  // `[*, *] => *`), and place your answer inside the square brackets.
  //
  type Answer2 = `[*, *] => *`[Map]

  //
  // EXERCISE 3
  //
  // Create a new type that has kind `(* -> *) -> *`.
  //
  type NewType1[F[_]]
  type Answer3 = `(* => *) => *`[NewType1]

  //
  // EXERCISE 4
  //
  // Create a trait with kind `*`.
  //
  trait Answer4 /*[]*/

  //
  // EXERCISE 5
  //
  // Create a trait with kind `[*, *, *] => *`.
  //
  trait Answer5[A, B, C] /*[]*/

  //
  // EXERCISE 6
  //
  // Create a trait with kind `[* => *, (* => *) => *] => *`.
  //
  trait Answer6[F[_], G[_[_]]] /*[]*/

  // TODO !
  //
  // EXERCISE 7
  //
  // Create an implementation of the trait `CollectionLike` for `List`.
  //
  trait CollectionLike[F[_]] {
    def empty[A]: F[A]

    def cons[A](a: A, as: F[A]): F[A]

    def uncons[A](as: F[A]): Option[(A, F[A])]

    final def singleton[A](a: A): F[A] =
      cons(a, empty[A])

    final def append[A](l: F[A], r: F[A]): F[A] =
      uncons(l) match {
        case Some((l, ls)) => append(ls, cons(l, r))
        case None          => r
      }

    final def filter[A](fa: F[A])(f: A => Boolean): F[A] =
      bind(fa)(a => if (f(a)) singleton(a) else empty[A])

    final def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
      uncons(fa) match {
        case Some((a, as)) => append(f(a), bind(as)(f))
        case None          => empty[B]
      }

    final def fmap[A, B](fa: F[A])(f: A => B): F[B] = {
      val single: B => F[B] = singleton[B](_)

      bind(fa)(f andThen single)
    }
  }
  val ListCollectionLike: CollectionLike[List] = ???

  //
  // EXERCISE 8
  //
  // Implement `Sized` for `List`.
  //
  trait Sized[F[_]] {
    // This method will return the number of `A`s inside `fa`.
    def size[A](fa: F[A]): Int
  }
  val ListSized: Sized[List] =
    new Sized[List] {
      override def size[A](fa: List[A]): Int = fa.length
    }

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to `String`.
  //
  val MapSized1: Sized[Map[String, ?]] =
    new Sized[Map[String, ?]] {
      override def size[A](fa: Map[String, A]): Int = fa.size
    }

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to a user-defined type parameter.
  //
  def MapSized2[K]: Sized[Map[K, ?]] =
    new Sized[Map[K, ?]] {
      override def size[A](fa: Map[K, A]): Int = fa.size
    }

  //
  // EXERCISE 10
  //
  // Implement `Sized` for `Tuple3`.
  //
  def Tuple3Sized[A, B]: Sized[(A, B, ?)] =
    new Sized[(A, B, ?)] {
      override def size[C](fa: (A, B, C)): Int = 1
    }
}

object typeclasses {

  /**
    * if we have a1: A, a2: A, a3: A
    * then
    * - lt(a1, a2) && lt(a2, a3) == lt(a1, a3)
    * - lt(a1, a1) == false
    *
    * This is called laws !
    *
    */
  // -

  /**
    * {{
    * Reflexivity:   a ==> equals(a, a)
    *
    * Transitivity:  equals(a, b) && equals(b, c) ==>
    *                equals(a, c)
    *
    * Symmetry:      equals(a, b) ==> equals(b, a)
    * }}
    */
  trait Eq[A] {
    def equals(l: A, r: A): Boolean
  }
  object Eq {
    def apply[A](implicit eq: Eq[A]): Eq[A] = eq

    implicit val EqInt: Eq[Int] = new Eq[Int] {
      def equals(l: Int, r: Int): Boolean = l == r
    }
    implicit def EqList[A](implicit Eq: Eq[A]): Eq[List[A]] =
      new Eq[List[A]] {
        @tailrec
        def equals(l: List[A], r: List[A]): Boolean =
          (l, r) match {
            case (Nil, Nil)         => true
            case (Nil, _)           => false
            case (_, Nil)           => false
            case (l :: ls, r :: rs) => Eq.equals(l, r) && equals(ls, rs)
          }
      }
  }
  implicit class EqSyntax[A](val l: A) extends AnyVal {
    def ===(r: A)(implicit eq: Eq[A]): Boolean = eq.equals(l, r)
  }

  //
  // Scalaz 7 Encoding
  //
  sealed trait Ordering
  case object EQUAL extends Ordering
  case object LT    extends Ordering
  case object GT    extends Ordering
  object Ordering {
    implicit val orderingEq: Eq[Ordering] = new Eq[Ordering] {
      override def equals(l: Ordering, r: Ordering): Boolean =
        (l, r) match {
          case (EQUAL, EQUAL) => true
          case (LT, LT)       => true
          case (GT, GT)       => true
          case _              => false
        }
    }
  }

  trait Ord[A] {
    def compare(l: A, r: A): Ordering
  }
  object Ord {
    def apply[A](implicit A: Ord[A]): Ord[A] = A

    implicit val OrdInt: Ord[Int] = new Ord[Int] {
      def compare(l: Int, r: Int): Ordering =
        if (l < r) LT else if (l > r) GT else EQUAL
    }
  }
  implicit class OrdSyntax[A](l: A) {
    def =?=(r: A)(implicit A: Ord[A]): Ordering =
      A.compare(l, r)

    def <(r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), LT)

    def <=(r: A)(implicit A: Ord[A]): Boolean =
      (l < r) || (this === r)

    def >(r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), GT)

    def >=(r: A)(implicit A: Ord[A]): Boolean =
      (l > r) || (this === r)

    def ===(r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), EQUAL)

    def !==(r: A)(implicit A: Ord[A]): Boolean =
      !Eq[Ordering].equals(A.compare(l, r), EQUAL)
  }
  case class Person(age: Int, name: String)
  object Person {
    implicit val OrdPerson: Ord[Person] = new Ord[Person] {
      def compare(l: Person, r: Person): Ordering =
        if (l.age < r.age) LT
        else if (l.age > r.age) GT
        else if (l.name < r.name) LT
        else if (l.name > r.name) GT
        else EQUAL
    }
    implicit val EqPerson: Eq[Person] = new Eq[Person] {
      def equals(l: Person, r: Person): Boolean =
        l == r
    }
  }

  //
  // EXERCISE 1
  //
  // Write a version of `sort1` called `sort2` that uses the polymorphic `List`
  // type constructor, and which uses the `Ord` type class, including the
  // compare syntax operator `=?=` to compare elements.
  //
  def sort1(l: List[Int]): List[Int] =
    l match {
      case Nil => Nil
      case x :: xs =>
        val (lessThan, notLessThan) = xs.partition(_ < x)

        sort1(lessThan) ++ List(x) ++ sort1(notLessThan)
    }

  def sort2[A: Ord](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)

      sort2(lessThan) ++ List(x) ++ sort2(notLessThan)
  }

  //
  // Scalaz 8 Encoding
  //
  sealed abstract class InstanceOfModule {
    type InstanceOf[T] <: T
    def instanceOf[T](t: T): InstanceOf[T]
  }

  object InstanceOfModule {
    val impl: InstanceOfModule = new InstanceOfModule {
      override type InstanceOf[T] = T
      override def instanceOf[T](t: T) = t
    }
  }
  import InstanceOfModule.impl._

  type ???[A] = Nothing

  /**
    * {{
    * // Associativity:
    * (a <> b) <> c === a <> (b <> c)
    * }}
    */
  trait SemigroupClass[A] {
    def append(l: => A, r: => A): A
  }
  type Semigroup[A] = InstanceOf[SemigroupClass[A]]
  object SemigroupClass {
    def apply[A](implicit A: Semigroup[A]): Semigroup[A] = A

    implicit val SemigroupString: Semigroup[String] =
      instanceOf(new SemigroupClass[String] {
        def append(l: => String, r: => String): String = l + r
      })
    implicit def SemigroupList[A]: Semigroup[List[A]] =
      instanceOf(new SemigroupClass[List[A]] {
        def append(l: => List[A], r: => List[A]): List[A] = l ++ r
      })
  }
  implicit def AnyToSemigroupSyntax[A](a: => A): SemigroupSyntax[A] = new SemigroupSyntax(() => a)
  class SemigroupSyntax[A](l: () => A) {
    def <>(r: => A)(implicit A: Semigroup[A]): A = A.append(l(), r)
  }
  //
  // EXERCISE 2
  //
  // Create an instance of the `Semigroup` type class for `java.time.Instant`.
  //
  implicit val SemigroupInstant: Semigroup[java.time.Instant] =
    instanceOf(new SemigroupClass[java.time.Instant] {
      override def append(l: => Instant, r: => Instant): Instant = l.plus(r.toEpochMilli, ChronoUnit.MILLIS)
    })

  //
  // EXERCISE 3
  //
  // Create an instance of the `Semigroup` type class for `Int`.
  //
  implicit val SemigroupInt: Semigroup[Int] =
    instanceOf(new SemigroupClass[Int] {
      override def append(l: => Int, r: => Int): Int = l + r
    })

  //
  // EXERCISE 4
  //
  // Create an instance of the `Semigroup` type class for `Set[A]`.
  //
  implicit def SemigroupSet[A]: Semigroup[Set[A]] =
    instanceOf(new SemigroupClass[Set[A]] {
      override def append(l: => Set[A], r: => Set[A]): Set[A] = l ++ r
    })

  //
  // EXERCISE 5
  //
  // Create an instance of the `Semigroup` type class for `Map[K, ?]`. Hint:
  // you will need some constraint applied to the values.
  //
  implicit def SemigroupMap[K, V: Semigroup]: Semigroup[Map[K, V]] =
    instanceOf(new SemigroupClass[Map[K, V]] {
      override def append(l: => Map[K, V], r: => Map[K, V]): Map[K, V] = l ++ r
    })

  //
  // EXERCISE 6
  //
  // Create a type class `Monoid[A]` that implies `Semigroup[A]` (that is, every
  // `Monoid[A]` must be a `Semigroup[A]`), which adds a single operation called
  // `zero`, which satisfies additional laws.
  //
  /**
    * {{
    * append(zero, a) === a
    * append(a, zero) === a
    * }}
    */
  trait MonoidClass[A] extends SemigroupClass[A] {
    def zero: A
  }
  object MonoidClass {
    def apply[A](implicit A: Monoid[A]): Monoid[A] = A
  }
  type Monoid[A] = InstanceOf[MonoidClass[A]]
  implicit def MonoidSemigroup[A](implicit M: Monoid[A]): Semigroup[A] = instanceOf(M)

  def empty[A: Monoid]: A = implicitly[Monoid[A]].zero

  //
  // EXERCISE 7
  //
  // Create an instance of the `Monoid` type class for `java.time.Instant`.
  //
  implicit val MonoidInstant: Monoid[java.time.Instant] =
    instanceOf(new MonoidClass[java.time.Instant] {
      override def zero: Instant                                 = Instant.MIN
      override def append(l: => Instant, r: => Instant): Instant = implicitly[Semigroup[java.time.Instant]].append(l, r)
    })

  //
  // EXERCISE 8
  //
  // Create an instance of the `Monoid` type class for `String`.
  //
  implicit val MonoidString: Monoid[String] =
    instanceOf(new MonoidClass[String] {
      override def zero: String                               = ""
      override def append(l: => String, r: => String): String = implicitly[Semigroup[String]].append(l, r)
    })

  //
  // EXERCISE 9
  //
  // Create an instance of the `Monoid` type class for `List[A]`.
  //
  implicit def MonoidList[A]: Monoid[List[A]] =
    instanceOf(new MonoidClass[List[A]] {
      override def zero: List[A]                                 = Nil
      override def append(l: => List[A], r: => List[A]): List[A] = implicitly[Semigroup[List[A]]].append(l, r)
    })

  //
  // EXERCISE 10
  //
  // Create an instance of the `Monoid` type class for `Int`.
  //
  implicit val MonoidInt: Monoid[Int] =
    instanceOf(new MonoidClass[Int] {
      override def zero: Int                         = 0
      override def append(l: => Int, r: => Int): Int = implicitly[Semigroup[Int]].append(l, r)
    })

  //
  // EXERCISE 11
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the additive monoid, with addition as `append`, and 0 as
  // `zero`.
  //
  final case class Sum(run: Int) extends AnyVal
  implicit val MonoidSum: Monoid[Sum] =
    instanceOf(new MonoidClass[Sum] {
      override def zero: Sum                         = Sum(0)
      override def append(l: => Sum, r: => Sum): Sum = Sum(l.run + r.run)
    })

  //
  // EXERCISE 12
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the multiplicative monoid, with multiplication as `append`,
  // and 1 as `zero`.
  //
  final case class Product(run: Int) extends AnyVal
  implicit val MonoidProduct: Monoid[Product] =
    instanceOf(new MonoidClass[Product] {
      override def zero: Product                                 = Product(1)
      override def append(l: => Product, r: => Product): Product = Product(l.run * r.run)
    })

  //
  // EXERCISE 13
  //
  // Create an instance of the `Collection` type class for `List`.
  //
  trait CollectionClass[F[_]] {
    def empty[A]: F[A]
    def cons[A](a: A, as: F[A]): F[A]
    def uncons[A](fa: F[A]): Option[(A, F[A])]
  }
  object CollectionClass {
    def apply[F[_]](implicit F: Collection[F]): Collection[F] = F
  }
  type Collection[F[_]] = InstanceOf[CollectionClass[F]]
  implicit val ListCollection: Collection[List] = instanceOf(new CollectionClass[List] {
    override def empty[A]: List[A]                   = Nil
    override def cons[A](a: A, as: List[A]): List[A] = a +: as
    override def uncons[A](fa: List[A]): Option[(A, List[A])] =
      fa match {
        case Nil     => None
        case x :: xs => Some(x -> xs)
      }
  })
}
