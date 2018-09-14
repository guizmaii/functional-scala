// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.arts

import scalaz._
import Scalaz._
import com.github.ghik.silencer.silent
import scalaz.zio.IO

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds
import scala.reflect.ClassTag

object exercises {

  /**
    * Example of how to use Monad Transformer to do effects without requiring the Effect in the method signature.
    *
    * We call this "eliminating the effect". (We don't propagate it to callers).
    *
    * Here we logs things, without adding `F[_]: Logging` in the method signature.
    *
    * The only constraint to achieve is for you to be able to express the capability of your Monad Transformer with
    * the Effects present in your method.
    *
    * For example, here we're able to log because we have to Console effect, which will finally make the logging effect.
    *
    * The problem with the Monad Transformer approach is that it add a lot of mess in your code.
    * And it becomes even worse when we're adding more effects and more Monad Transformers.
    *
    * There're alternative approaches, like MTL !
    *
    * Look at Cats-mtl !
    */
  object Teaching0 {

    case class LoggingT[F[_], A](run: List[String] => F[(List[String], A)]) { self =>
      def map[B](f: A => B)(implicit F: Functor[F]): LoggingT[F, B] =
        LoggingT(log => self.run(log).map(t => (t._1, f(t._2))))

      def flatMap[B](f: A => LoggingT[F, B])(implicit F: Monad[F]): LoggingT[F, B] =
        LoggingT(log => self.run(log).flatMap(t => f(t._2).run(t._1)))

      /**
        * `eval` elimintates the effect: LoggingT[F[_], ...] => F[_]
        */
      def eval(implicit F: Functor[F]): F[(List[String], A)] = run(Nil).map(t => (t._1.reverse, t._2))
    }
    object LoggingT {
      def point[F[_]: Applicative, A](a: => A): LoggingT[F, A] = LoggingT(log => Applicative[F].point((log, a)))

      def lift[F[_], A](fa: F[A])(implicit F: Functor[F]) = LoggingT(log => fa.map(a => (log, a)))

      /**
        * This add the `log` capability to F[_]
        */
      def log[F[_]: Applicative](line: String): LoggingT[F, Unit] =
        LoggingT(log => Applicative[F].point((line :: log, ())))

      implicit def LoggingTMonad[F[_]: Monad]: Monad[LoggingT[F, ?]] =
        new Monad[LoggingT[F, ?]] {
          override def point[A](a: => A): LoggingT[F, A] = LoggingT(log => Monad[F].point((log, a)))

          override def bind[A, B](fa: LoggingT[F, A])(f: A => LoggingT[F, B]): LoggingT[F, B] =
            LoggingT(log => fa.run(log).flatMap((a: (List[String], A)) => f(a._2).run(a._1)))
        }
    }

    trait Console[F[_]] {
      def putStrLn(s: String): F[Unit]
      def getStrLn: F[String]
    }
    object Console {
      def apply[F[_]](implicit F: Console[F]): Console[F] = F

      def putStrLn[F[_]](s: String): F[Unit] = ???
      def getStrLn[F[_]]: F[String]          = ???
    }
    import Console._

    /**
      * We can observe here all the mess the Monad Transformer adds with the `lift` thing.
      */
    def getName[F[_]: Console: Monad]: F[String] =
      (for {
        _ <- LoggingT.lift[F, Unit](putStrLn("Hello world"))
        _ <- LoggingT.log[F]("My log line")
        n <- LoggingT.lift[F, String](getStrLn)
        _ <- LoggingT.lift[F, Unit](putStrLn(s"tada ! Your name is $n"))
      } yield n).eval.flatMap {
        case (log, name) =>
          val lines: String = log.mkString("\n")

          putStrLn[F](s"Logging: \n$lines") *> Monad[F].point(name)
      }

  }

  object Teaching1 {

    /**
    *
    *
    *
    * 1. Each type class represents some business subdomain, e.g. account credits/debits.
    * 2. All methods in the type class relate to each other through algebraic or operational laws.
    *    transact(debit(account, amount) *> credit(account, amount)) === balance(account)
    * 3. Delete all methods that don't relate to others through algebraic or operational laws
    * 4. All operations should be able to be expressed in terms of a small set of orthogonal operations
    * 5. It's a good sign if you can express one type class in terms of another, lower-level one
    *
    * TODO: Any point I missed ?
    */

  }

  object Teaching2 {
    type Geocode
    type Response
    type Value

    /**
      * `map` && `flatMap` could also be implemented thanks to an Monad[Effect[E, ?]\] instance in the companion object.
      * Performances could be a little less.
      *
      */
    case class Effect[E, A](run: Future[Either[E, Option[A]]]) {
      def map[B](f: A => B)(implicit ec: ExecutionContext): Effect[E, B] = Effect(run.map(_.map(_.map(f))))
      def flatMap[B](f: A => Effect[E, B])(implicit ec: ExecutionContext): Effect[E, B] =
        Effect(run.flatMap {
          case Left(e)        => Future.successful(Left(e))
          case Right(None)    => Future.successful(Right(None))
          case Right(Some(a)) => f(a).run
        })
    }
    object Effect {
      def point[E, A](a: => A): Effect[E, A] = Effect(Future.successful(Right(Some(a))))
    }

    def geoAPI[E](url: String): Effect[E, Geocode]           = ???
    def cacheAPI[E](key: Array[Byte]): Effect[E, Value]      = ???
    def queryDatabase[E](query: String): Effect[E, Response] = ???

    /**
      * Second possibility
      *
      * (
      *   doesn't compile:
      *     [error] type Λ$ takes type parameters
      *     [error]     type MonadTransformerBasedEffect[E, A] = OptionT[EitherT[Future, E, ?], ?]
      * )
      *
      */
    //type MonadTransformerBasedEffect[E, A] = OptionT[EitherT[Future, E, ?], ?]

    /**
      * Third possibility
      */
    trait PerformantEffect[F[_, _]] {
      def monad[E]: Monad[F[E, ?]]

      def fail[E, A](e: E): F[E, A]

      def attempt[E, A](fea: F[E, A]): F[Nothing, Either[E, A]]

      def none[E, A]: F[E, A]

      def some[E, A](a: A): F[E, A]

      def fromFuture[E, A](f: Future[A]): F[E, A]
    }
    object PerformantEffect {
      type MyIO[E, A] = IO[Option[E], A]

      implicit val EffectMyIO: PerformantEffect[MyIO] =
        new PerformantEffect[MyIO] {
          def monad[E]: Monad[MyIO[E, ?]] =
            new Monad[MyIO[E, ?]] {
              def point[A](a: => A): MyIO[E, A]                              = IO.point(a)
              def bind[A, B](fa: MyIO[E, A])(f: A => MyIO[E, B]): MyIO[E, B] = fa.flatMap(f)
            }

          def fail[E, A](e: E): MyIO[E, A] = IO.fail(Some(e))

          def attempt[E, A](fea: MyIO[E, A]): MyIO[Nothing, Either[E, A]] = fea.attempt.flatMap {
            case Left(None)    => IO.fail(None)
            case Left(Some(e)) => IO.now(Left(e))
            case Right(a)      => IO.now(Right(a))
          }

          def none[E, A]: MyIO[E, A] = IO.fail[Option[E]](None)

          def some[E, A](a: A): MyIO[E, A] = IO.now(a)

          def fromFuture[E, A](f: Future[A]): MyIO[E, A] = ???
        }
    }

    type Task[A]
    type MyTask[E, A] = EitherT[Task, E, A]
    implicit val EffectTask: PerformantEffect[MyTask] =
      new PerformantEffect[MyTask] {
        override def monad[E]: Monad[MyTask[E, ?]]                                   = ???
        override def fail[E, A](e: E): MyTask[E, A]                                  = ???
        override def attempt[E, A](fea: MyTask[E, A]): MyTask[Nothing, Either[E, A]] = ???
        override def none[E, A]: MyTask[E, A]                                        = ???
        override def some[E, A](a: A): MyTask[E, A]                                  = ???
        override def fromFuture[E, A](f: Future[A]): MyTask[E, A]                    = ???
      }

    type MyTerribleTask[E, A] = Task[A]
    case class MyTerribleError[E: ClassTag](error: E) extends Throwable
    implicit def EffectMyTerribleError[E0: ClassTag]: PerformantEffect[MyTerribleTask] =
      new PerformantEffect[MyTerribleTask] {
        override def monad[E]: Monad[MyTerribleTask[E, ?]]                                           = ???
        override def fail[E, A](e: E): MyTerribleTask[E, A]                                          = ???
        override def attempt[E, A](fea: MyTerribleTask[E, A]): MyTerribleTask[Nothing, Either[E, A]] = ???
        override def none[E, A]: MyTerribleTask[E, A]                                                = ???
        override def some[E, A](a: A): MyTerribleTask[E, A]                                          = ???
        override def fromFuture[E, A](f: Future[A]): MyTerribleTask[E, A]                            = ???
      }

    def polyGeoAPI[F[_, _], E](url: String): F[E, Geocode]           = ???
    def polyCacheAPI[F[_, _], E](key: Array[Byte]): F[E, Value]      = ???
    def polyQueryDatabase[F[_, _], E](query: String): F[E, Response] = ???

  }

  /**
    * Higher Order Abstract Syntax
    */
  object TeachingHigherOrderAsbtractSyntax {

    /**
      * TO ACHIEVE:
      * -----------
      *
      * let i = 0
      * in while(1 < 10) { i = i + i }
      */
    /**
      * v1
      */
    sealed trait Expr0[A]
    final case class IntLit(value: Int)                                       extends Expr0[Int]
    final case class Add(l: Expr0[Int], r: Expr0[Int])                        extends Expr0[Int]
    final case class Let[A, B](name: String, value: Expr0[A], body: Expr0[B]) extends Expr0[B]
    final case class Value[A](name: String)                                   extends Expr0[A]
    final case class UpdateVar[A](name: String, value: Expr0[A])              extends Expr0[A]
    final case class LessThan(left: Expr0[Int], right: Expr0[Int])            extends Expr0[Boolean]
    final case class While[A](condition: Expr0[Boolean], body: Expr0[A])      extends Expr0[Unit]

    case class IState(value: Map[String, Any]) {
      def addVariable(name: String, v: Any): IState = copy(value = value + (name -> v))
      def removeVariable(name: String): IState      = copy(value = value - name)
    }

    import scalaz.zio._

    def interpret[A0](expr: Expr0[A0], ref: Ref[IState]): IO[String, A0] = {
      def interpret0[A](expr: Expr0[A]): IO[String, A] =
        expr match {
          case IntLit(value)    => IO.now(value)
          case Add(left, right) => interpret0(left).seqWith(interpret0(right))(_ + _)

          case Let(name, value, body) =>
            for {
              v <- interpret0(value)
              _ <- ref.update(_.addVariable(name, v))
              b <- interpret0(body)
              _ <- ref.update(_.removeVariable(name))
            } yield b

          case Value(name) =>
            for {
              s <- ref.get
              v <- IO.fromOption(s.value.get(name)).leftMap(_ => s"Unreferenced variable: $name")
            } yield v.asInstanceOf[A]

          case UpdateVar(name, value) =>
            for {
              v <- interpret0(value)
              _ <- ref.update(_.addVariable(name, v))
            } yield v

          case LessThan(left, right) =>
            interpret0(left).seqWith(interpret0(right))(_ < _)

          case While(condition, body) =>
            (for {
              b <- interpret0(condition)
              _ <- if (b) interpret0(body) else IO.unit
            } yield b).repeat(Schedule.doWhile[Boolean](identity).void)
        }

      interpret0[A0](expr)
    }

    /**
      * v2
      */
    trait Expr[F[_]] {
      def intLit(value: Int): F[Int]
      def add(l: F[Int], r: F[Int]): F[Int]
      def let[A, B](name: String, value: F[A], body: F[A] => F[B]): F[B]
      def updateVar[A](name: String, value: F[A]): F[A]
      def lessThan(left: F[Int], right: F[Int]): F[Boolean]
      def while0[A](condition: F[Boolean], body: F[A]): F[Unit]
    }
    object Expr {
      def apply[F[_]](implicit F: Expr[F]): Expr[F] = F
    }
    implicit class IntExprSunctax[F[_]](left: F[Int]) {
      def +(right: F[Int])(implicit F: Expr[F]): F[Int]     = F.add(left, right)
      def <(right: F[Int])(implicit F: Expr[F]): F[Boolean] = F.lessThan(left, right)
    }
    def int[F[_]: Expr](int: Int): F[Int]                                          = Expr[F].intLit(int)
    def let[F[_]: Expr, A, B](name: String, value: F[A])(body: F[A] => F[B]): F[B] = Expr[F].let(name, value, body)
    def while0[F[_]: Expr, A](condition: F[Boolean])(body: F[A]): F[Unit]          = Expr[F].while0(condition, body)

    def interpreter(ref: Ref[IState]): Expr[IO[String, ?]] =
      new Expr[IO[String, ?]] {
        override def intLit(value: Int): IO[String, Int] = IO.now(value)

        override def add(l: IO[String, Int], r: IO[String, Int]): IO[String, Int] =
          l.seqWith(r)(_ + _)

        override def let[A, B](
            name: String,
            value: IO[String, A],
            body: IO[String, A] => IO[String, B]
        ): IO[String, B] =
          for {
            v <- value
            _ <- ref.update(_.addVariable(name, v))
            b <- body(value)
            _ <- ref.update(_.removeVariable(name))
          } yield b

        override def updateVar[A](name: String, value: IO[String, A]): IO[String, A] =
          for {
            v <- value
            _ <- ref.update(_.addVariable(name, v))
          } yield v

        override def lessThan(left: IO[String, Int], right: IO[String, Int]): IO[String, Boolean] =
          left.seqWith(right)(_ < _)

        override def while0[A](condition: IO[String, Boolean], body: IO[String, A]): IO[String, Unit] =
          (for {
            b <- condition
            _ <- if (b) body else IO.unit
          } yield b).repeat(Schedule.doWhile[Boolean](identity).void)
      }

    def program[F[_]: Expr]: F[Unit] =
      let("i", int(0)) { i =>
        while0(i < int(10)) {
          Expr[F].updateVar("i", i + int(1))
        }
      }

  }

  object TeachingFunctors {

    /**
      * Monadic Parser
      */
    case class Parser0[+E, +A](run: String => Either[E, (String, A)])

    /**
      * Applicative Parser
      */
    sealed trait Parser[+E, +A] { self =>
      def map[B](f: A => B): Parser[E, B] = Parser.Map[E, A, B](self, f)

      def ||[E1 >: E, B](that: Parser[E1, B]): Parser[E1, Either[A, B]] = Parser.Alternative(self, that)

      def * : Parser[E, List[A]] = Parser.Repeat(self)

      def ~[E1 >: E, B](that: Parser[E1, B]): Parser[E1, (A, B)] = Parser.Zip(self, that)

      def <*[E1 >: E, B](that: Parser[E1, B]): Parser[E1, A] = (self ~ that).map(_._1)

      def *>[E1 >: E, B](that: Parser[E1, B]): Parser[E1, B] = (self ~ that).map(_._2)
    }
    object Parser {
      def fail[E](e: E): Parser[E, Nothing] = Fail(e)

      def char[E](e: E): Parser[E, Char] = Character(e)

      def select[E, A](cond: Parser[E, Boolean])(ifTrue: Parser[E, A], ifFalse: Parser[E, A]) =
        Select(cond, ifTrue, ifFalse)

      case class Fail[E](error: E)                                             extends Parser[E, Nothing]
      case class Succceed[A](value: A)                                         extends Parser[Nothing, A]
      case class Character[E](error: E)                                        extends Parser[E, Char]
      case class Repeat[E, A](value: Parser[E, A])                             extends Parser[E, List[A]]
      case class Alternative[E, A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, Either[A, B]]
      case class Zip[E, A, B](left: Parser[E, A], right: Parser[E, B])         extends Parser[E, (A, B)]
      case class Map[E, A0, A](value: Parser[E, A0], f: A0 => A)               extends Parser[E, A]

      /**
        * Selectable Functor
        */
      case class Select[E, A](condition: Parser[E, Boolean], ifTrue: Parser[E, A], ifFalse: Parser[E, A])
          extends Parser[E, A]

      implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
        new Applicative[Parser[E, ?]] {
          override def point[A](a: => A): Parser[E, A] = Succceed(a)
          override def ap[A, B](fa: => Parser[E, A])(f: => Parser[E, A => B]): Parser[E, B] =
            Map[E, (A => B, A), B](Zip(f, fa), { case (f, a) => f(a) })
        }
    }

    def compiler[E, A](parser: Parser[E, A]): String => Either[E, A] =
      input => {
        @silent var index    = 0
        @silent var error: E = null.asInstanceOf[E]
        @silent var value: A = null.asInstanceOf[A]
        type Repr = () => Unit

        def compile0(parser: Parser[E, A]): Repr = ???

        compile0(parser)
        if (error == null) Right(value) else Left(error)
      }

    sealed trait JSON
    sealed trait JSONError

    def ParserJSON: Parser[JSONError, JSON] = ???

    val parseJson: String => Either[JSONError, JSON] = compiler(ParserJSON)
  }

  /**
    * Final tagless composition
    */
  object Teaching4 {

    trait MonadReader[R, F[_]] {
      def read: F[R]
    }
    trait HasEnv1[R] {
      def env1: Lens[R, LowDslEnv1]
    }
    case class LowDslEnv1()
    def myLowDsl1[R: HasEnv1, F[_]](implicit F: MonadReader[R, F]): F[Unit] = ???

    trait HasEnv2[R] {
      def env2: Lens[R, LowDslEnv2]
    }
    case class LowDslEnv2()
    def myLowDsl2[R: HasEnv2, F[_]](implicit F: MonadReader[R, F]): F[Unit] = ???

    case class GlobalEnv(env1: LowDslEnv1, env2: LowDslEnv2)
    object GlobalEnv {
      implicit val GlobalEnvHasEnv1: HasEnv1[GlobalEnv] = ???
      implicit val GlobalEnvHasEnv2: HasEnv2[GlobalEnv] = ???
    }

    def myProgram[F[_]](implicit F: MonadReader[GlobalEnv, F]): F[Unit] = ???

  }

  object TeachingFixPointDatatype {

    // Waiting for JDG mail ?

  }

}
