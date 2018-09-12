// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.abstractions

import scalaz.Scalaz._
import scalaz._

import scala.language.higherKinds

object algebra {
  //
  // EXERCISE 1
  //
  // Define a semigroup for `NotEmpty` below.
  //
  case class NotEmpty[+A](head: A, tail: Option[NotEmpty[A]])
  implicit def NotEmptySemigroup[A]: Semigroup[NotEmpty[A]] =
    new Semigroup[NotEmpty[A]] {
      // TODO: this is false. Fix it !
      override def append(f1: NotEmpty[A], f2: => NotEmpty[A]): NotEmpty[A] =
        (f1, f2) match {
          case (NotEmpty(a, None), NotEmpty(b, None)) => NotEmpty(a, Some(NotEmpty(b, None)))
          case (NotEmpty(a, Some(nonEmptyA)), NotEmpty(b, Some(nonEmptyB))) =>
            NotEmpty(a, None) |+| nonEmptyA |+| NotEmpty(b, None) |+| nonEmptyB
        }
    }
  val example1 = NotEmpty(1, None) |+| NotEmpty(2, None)

  //
  // EXERCISE 2
  //
  // Design a permission system for securing some resource, together with a
  // monoid for the permission data structure.
  //
  sealed trait Rights
  case object Read  extends Rights
  case object Write extends Rights

  final case class Email(value: String) extends AnyVal
  sealed trait Resource
  final case class File(name: String)  extends Resource
  final case class Video(name: String) extends Resource

  case class Permission(value: Map[Email, (Resource, Set[Rights])]) extends AnyVal

  /* TODO: Finish !
  implicit val MonoidPermission: Monoid[Permission] =
    new Monoid[Permission] {
      override def zero: Permission = Permission(Map.empty)
      override def append(f1: Permission, f2: => Permission): Permission =
        Permission(f1.value |+| f2.value) // TODO: Where is the Semigroup of Map in Scalaz ??
    }

  val example2 = mzero[Permission] |+| Permission(Map((Email("Jules"), (File("FP to the Max"), Set(Read)))))
   */

  //
  // EXERCISE 3
  //
  // Define an instance of `Semigroup` for `(A, B)` when both `A` and
  // `B` form semigroups.
  //
  implicit def SemigroupTuple2[A: Semigroup, B: Semigroup]: Semigroup[(A, B)] =
    new Semigroup[(A, B)] {
      override def append(l: (A, B), r: => (A, B)): (A, B) = (l._1 |+| r._1, l._2 |+| r._2)
    }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Monoid` for `NotEmpty` for any type `A`.
  //
  implicit def MonoidNotEmpty[A]: Monoid[NotEmpty[A]] = ??? // Impossible !
}

object functor {

  object Teaching {

    /**
      * Laws:
      * -----
      *
      *  Identity:    fmap(identity) === identity
      *  Composition: fmap(f.compose(g)) === fmap(f).compose(fmap(g))
      *
      */
    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]

      def fmap[A, B](f: A => B): F[A] => F[B] = (fa: F[A]) => map(fa)(f)
    }

    implicit def FunctorMap[K]: Functor[Map[K, ?]] =
      new Functor[Map[K, ?]] {
        override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = ???
      }

    implicit val ListFunctor: Functor[List] =
      new Functor[List] {
        override def map[A, B](fa: List[A])(f: A => B): List[B] =
          fa match {
            case Nil     => Nil
            case a :: as => f(a) :: map(as)(f)
          }
      }

    implicit val OptionFunctor: Functor[Option] =
      new Functor[Option] {
        override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
          fa match {
            case None        => None
            case Some(value) => Some(f(value))
          }
      }

    implicit val VectorFunctor: Functor[Vector] =
      new Functor[Vector] {
        override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f) // Yes, we cheat here :D
      }

    implicit def MapFunctor[K]: Functor[Map[K, ?]] =
      new Functor[Map[K, ?]] {
        override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.mapValues(f)
      }
  }

  //
  // EXERCISE 1
  //
  // Define an instance of `Functor` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A)                            extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]

  // TODO: tail recursive version ?
  implicit val BTreeFunctor: Functor[BTree] =
    new Functor[BTree] {
      def map[A, B](fa: BTree[A])(f: A => B): BTree[B] =
        fa match {
          case Leaf(a)           => Leaf(f(a))
          case Fork(left, right) => Fork(map(left)(f), map(right)(f))
        }
    }

  //
  // EXERCISE 2
  //
  // Define an instance of `Functor` for `Nothing`.
  //
  // Jules Note:
  // -----------
  // Useful to define because for things like that for example: Either[Nothing, A]
  // So it's possible to implement this (and it's useful) but, because Nothing doesn't have any value,
  // it's impossible to use this functor instance.
  //
  implicit val NothingFunctor: Functor[Nothing] =
    new Functor[Nothing] {
      override def map[A, B](fa: Nothing)(f: A => B): Nothing = fa
    }

  //
  // EXERCISE 3
  //
  // Define an instance of `Functor` for Parser[E, ?].
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def ParserFunctor[E]: Functor[Parser[E, ?]] =
    new Functor[Parser[E, ?]] {
      override def map[A, B](fa: Parser[E, A])(f: A => B): Parser[E, B] =
        Parser(
          input =>
            fa.run(input) match {
              case Left(e)            => Left(e)
              case Right((string, a)) => Right(string -> f(a))
            }
        )
    }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Functor` for the following data type.
  //
  case class DataType[A](f: A => A)
  implicit val DataTypeFunctor: Functor[DataType] = ??? // Impossible to implement !

  //
  // EXERCISE 5
  //
  // Define an instance of `Functor` for `FunctorProduct`.
  //
  case class FunctorProduct[F[_], G[_], A](l: F[A], r: F[A])
  implicit def FunctorProductFunctor[F[_]: Functor, G[_]: Functor]: Functor[FunctorProduct[F, G, ?]] = ???

  //
  // EXERCISE 6
  //
  // Define an instance of `Functor` for `FunctorSum`.
  //
  case class FunctorSum[F[_], G[_], A](run: Either[F[A], G[A]])
  implicit def FunctorSumFunctor[F[_]: Functor, G[_]: Functor]: Functor[FunctorSum[F, G, ?]] = ???

  //
  // EXERCISE 7
  //
  // Define an instance of `Functor` for `FunctorNest`.
  //
  case class FunctorNest[F[_], G[_], A](run: F[G[A]])
  implicit def FunctorNestFunctor[F[_]: Functor, G[_]: Functor]: Functor[FunctorNest[F, G, ?]] = ???

  object Teaching2 {

    // Jules note:
    // -----------
    // We're arrived late after lunching.
    // So, I don't have the context of this code.

    case class Identity[A](value: A)

    case class USD(value: BigDecimal)

    case class Amount[A](value: A)
    case class Account[Ident](id: Ident)
    case class Client[F[_], Ident](account: F[Account[Ident]])

    sealed trait Operation[A] {
      def map[B](f: A => B): Operation[B] = ???

      def zip[B](that: Operation[B]): Operation[(A, B)] = ???
    }

    case class Deposit[Ident, A](to: Account[Ident], amount: Amount[A])    extends Operation[A]
    case class Wighdraw[Ident, A](from: Account[Ident], amount: Amount[A]) extends Operation[A]
    case class Both[A, B](l: Operation[A], r: Operation[B])                extends Operation[A]
    case class Map[A, B](op: Operation[A], f: A => B)                      extends Operation[A]

    // Jules notes:
    // ------------
    // for the following code, I have the context.

    trait Apply[F[_]] extends Functor[F] {
      def zip[A, B](l: F[A], r: F[B]): F[(A, B)]
    }
    implicit class ApplySyntax[F[_], A](l: F[A]) {
      def *>[B](r: F[B])(implicit F: Apply[F]): F[B] = F.zip(l, r).map(_._2)

      def <*[B](r: F[B])(implicit F: Apply[F]): F[A] = F.zip(l, r).map(_._1)
    }

    def zipOption[A, B](l: Option[A], r: Option[B]): Option[(A, B)] =
      (l, r) match {
        case (Some(a), Some(b)) => Some(a -> b)
        case _                  => None
      }

    def zipWith[A, B, C](l: Option[A], r: Option[B])(f: ((A, B)) => C): Option[C] = zipOption(l, r).map(f)

    def zipList1[A, B](l: List[A], r: List[B]): List[(A, B)] =
      (l, r) match {
        case (Nil, _)      => Nil
        case (a :: as, bs) => zipList1(as, bs) ++ bs.map(a -> _)
      }

    def zipList2[A, B](l: List[A], r: List[B]): List[(A, B)] =
      (l, r) match {
        case (a :: as, b :: bs) => (a -> b) :: zipList2(as, bs)
        case _                  => Nil
      }

    implicit val ApplyOption: Apply[Option] =
      new Apply[Option] {
        override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
          fa match {
            case None    => None
            case Some(a) => Some(f(a))
          }

        /**
          * `ap` defined in terms of `zip`
          *
          * Commented because compiler is complaining about its non usage in the code...
          */
        //def ap[A, B](fa: => Option[A])(fab: => Option[A => B]): Option[B] = zip(fa, fab).map { case (a, ab) => ab(a) }

        override def zip[A, B](l: Option[A], r: Option[B]): Option[(A, B)] =
          (l, r) match {
            case (Some(a), Some(b)) => Some(a -> b)
            case _                  => None
          }
      }

    trait Applicative[F[_]] extends Apply[F] {
      // fa <* fb !== fa
      // fa *> fb !== fb
      // fa <* point(b) === fa
      // point(b) *> fa === fa
      def point[A](a: => A): F[A]
    }

    val l   = List(1, 2, 3)
    val r   = List(9, 2)
    val lr1 = List((1, 9), (1, 2), (2, 9), (2, 2), (3, 9), (3, 2))
    val lr2 = List((1, 9), (2, 2))

    // (l <* r) : List(1, 1, 2, 2, 3, 3)
    // (l <* r) !== l
    val lr1_mapped1 = lr1.map(_._1)

    // (l *> r) : List(9, 2, 9, 2, 9, 2)
    // (l *> r) !== l
    val lr1_mapped2 = lr1.map(_._2)

    // (l <* r) = List(1, 2)
    val lr2_mapped1 = lr2.map(_._1)

    // (l *> r) = List(9, 2) // actual coinscidence that the result match the original list.
    val lr2_mapped2 = lr2.map(_._2)

  }

  //
  // EXERCISE 8
  //
  // Define `Applicative` for `Option`.
  //
  implicit val OptionApplicative: Applicative[Option] =
    new Applicative[Option] {
      def point[A](a: => A): Option[A] = Some(a)

      def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] =
        (fa, f) match {
          case (Some(a), Some(fab)) => point(fab(a))
          case _                    => None
        }
    }

  //
  // EXERCISE 9
  //
  // Implement `zip` in terms of the applicative composition using `|@|`.
  //
  val example1                                                     = (Option(3) |@| Option(5))((_, _))
  val example2                                                     = zip(Option(3), Option("foo")): Option[(Int, String)]
  def zip[F[_]: Applicative, A, B](l: F[A], r: F[B]): F[(A, B)]    = (l |@| r)(_ -> _)
  def ap2[F[_]: Applicative, A, B](fa: F[A], fab: F[A => B]): F[B] = (fab |@| fa)(_(_))
  //
  // EXERCISE 10
  //
  // Define an instance of `Applicative` for `Parser[E, ?]`.
  //
  implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
    new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E, A] = Parser(input => Right((input, a)))

      def ap[A, B](fa: => Parser[E, A])(f: => Parser[E, A => B]): Parser[E, B] =
        Parser(
          input =>
            f.run(input) match {
              case Left(e) => Left(e)
              case Right((string0, fab)) =>
                fa.run(string0) match {
                  case Left(e)             => Left(e)
                  case Right((string1, a)) => point(fab(a)).run(string1)
                }
            }
        )
    }

  object Teaching3 {
    import Teaching2._

    trait Monad[F[_]] extends Applicative[F] {
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
    }

  }

  implicit val MonadOption: Monad[Option] =
    new Monad[Option] {
      override def point[A](a: => A): Option[A] = Some(a)
      override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
        fa match {
          case None    => None
          case Some(a) => f(a)
        }
    }

  implicit val MonadList: Monad[List] =
    new Monad[List] {
      override def point[A](a: => A): List[A] = List(a)
      override def bind[A, B](fa: List[A])(f: A => List[B]): List[B] =
        fa match {
          case Nil     => Nil
          case a :: as => f(a) ++ bind(as)(f)
        }
    }

  //
  // EXERCISE 11
  //
  // Define an instance of `Monad` for `BTree`.
  //
  implicit val MonadBTree: Monad[BTree] =
    new Monad[BTree] {
      override def point[A](a: => A): BTree[A] = Leaf(a)
      override def bind[A, B](fa: BTree[A])(f: A => BTree[B]): BTree[B] =
        fa match {
          case Leaf(a)    => f(a)
          case Fork(l, r) => Fork(bind(l)(f), bind(r)(f))
        }
    }

  //
  // EXERCISE 12
  //
  // Define an instance of `Monad` for `Parser[E, ?]`.
  //
  implicit def MonadParser[E]: Monad[Parser[E, ?]] =
    new Monad[Parser[E, ?]] {
      override def point[A](a: => A): Parser[E, A] = Parser(input => Right((input, a)))
      override def bind[A, B](fa: Parser[E, A])(f: A => Parser[E, B]): Parser[E, B] =
        Parser(
          input =>
            fa.run(input) match {
              case Left(e)            => Left(e)
              case Right((string, a)) => f(a).run(string)
            }
        )
    }
}

object foldable {
  //
  // EXERCISE 1
  //
  // Define an instance of `Foldable` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A)                            extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]

  implicit val FoldableBTree: Foldable[BTree] =
    new Foldable[BTree] {
      override def foldMap[A, B](fa: BTree[A])(f: A => B)(implicit F: Monoid[B]): B =
        fa match {
          case Leaf(a)           => f(a)
          case Fork(left, right) => foldMap(left)(f) |+| foldMap(right)(f)
        }

      override def foldRight[A, B](fa: BTree[A], z: => B)(f: (A, => B) => B): B =
        fa match {
          case Leaf(a) => f(a, z)
          case Fork(left, right) =>
            val b = foldRight(right, z)(f)
            foldRight(left, b)(f)
        }
    }

  //
  // EXERCISE 2
  //
  // Try to define an instance of `Foldable` for `A => ?`.
  //
  implicit def FunctionFoldable[A]: Foldable[A => ?] = ??? // Impossible to implement a sensible instance.

  //
  // EXERCISE 3
  //
  // Define an instance of `Traverse` for `BTree`.
  //
  implicit val TraverseBTree: Traverse[BTree] =
    new Traverse[BTree] {
      override def traverseImpl[F[_], A, B](fa: BTree[A])(f: A => F[B])(implicit F: Applicative[F]): F[BTree[B]] =
        fa match {
          case Leaf(a) => f(a).map(Leaf.apply)
          case Fork(left, right) =>
            val lg = traverseImpl(left)(f)
            val rg = traverseImpl(right)(f)
            (lg |@| rg)(Fork(_, _))
        }
    }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Traverse` for `Parser[E, ?]`.
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def TraverseParser[E]: Traverse[Parser[E, ?]] = ??? // Impossible !
}

object optics {
  sealed trait Country
  object Country {
    final val CountryPrism: Prism[Country, Unit] =
      Prism[Country, Unit](
        get = {
          case USA => Some(())
          case _   => None
        },
        set = _ => USA
      )
  }
  case object USA    extends Country
  case object UK     extends Country
  case object Poland extends Country

  case class Org(name: String, address: Address, site: Site)
  object Org {
    val site: Lens[Org, Site] = Lens[Org, Site](_.site, ss => _.copy(site = ss))
  }
  case class Address(number: String, street: String, postalCode: String, country: Country)
  case class Site(manager: Employee, address: Address, employees: Set[Employee])
  object Site {
    val manager: Lens[Site, Employee] = Lens[Site, Employee](_.manager, m => _.copy(manager = m))
  }
  case class Employee(name: String, dob: java.time.Instant, salary: BigDecimal)
  object Employee {
    final val salary: Lens[Employee, BigDecimal] = Lens[Employee, BigDecimal](_.salary, s => _.copy(salary = s))
  }

  lazy val org: Org = ???

  lazy val org2: Org = org.copy(
    site = org.site.copy(
      manager = org.site.manager.copy(
        salary = org.site.manager.salary * 0.95
      )
    )
  )

  /**
    * S - Super structure
    * A - Sub structure
    *
    * An optic S A allows you to focus in a sub structure A inside a super-structure S,
    * for purposes of accessing or modifying the substructure.
    */
  type Optic[S, A]

  final case class Lens[S, A](get: S => A, set: A => S => S) { self =>
    final def >>>[B](that: Lens[A, B]): Lens[S, B] =
      Lens(
        get = self.get.andThen(that.get),
        set = (b: B) => (s: S) => self.set(that.set(b)(self.get(s)))(s)
      )

    final def update(f: A => A): S => S = (s: S) => set(f(self.get(s)))(s)
  }

  lazy val org3: Org =
    (Org.site >>> Site.manager >>> Employee.salary).update(_ * 0.95)(org)

  final case class Prism[S, A](get: S => Option[A], set: A => S) { self =>
    def >>>[B](that: Prism[A, B]): Prism[S, B] =
      Prism[S, B](
        get = (s: S) => self.get(s).flatMap(that.get),
        set = that.set.andThen(self.set)
      )

    final def select(implicit ev: Unit =:= A): S = set(ev(()))
  }

  def _Left[A, B]: Prism[Either[A, B], A] =
    Prism[Either[A, B], A](
      get = {
        case Left(a) => Some(a)
        case _       => None
      },
      set = Left(_)
    )

  def _Right[A, B]: Prism[Either[A, B], B] =
    Prism[Either[A, B], B](
      get = {
        case Right(b) => Some(b)
        case _        => None
      },
      set = Right(_)
    )

}
