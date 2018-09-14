// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.arts

import scalaz._
import Scalaz._

import scala.language.higherKinds

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

}
