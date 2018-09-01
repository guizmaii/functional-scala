// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.essentials

object EffectsAsValues {

  sealed trait Program[A] { self =>
    final def map[B](f: A => B): Program[B] = flatMap(a => Return(f(a)))

    final def flatMap[B](f: A => Program[B]): Program[B] =
      self match {
        case Return(a) => f(a)
        case x         => Chain(x, f)
      }
  }
  object Program {
    final def point[A](a: => A): Program[A] = Return(a)
  }
  final case class Return[A](a: A)                                    extends Program[A]
  final case class GetStrLn[A](next: String => Program[A])            extends Program[A] // next is named a continuation.
  final case class PutStrLn[A](line: String, next: Program[A])        extends Program[A]
  final case class Chain[A0, A](previous: A0, next: A0 => Program[A]) extends Program[A]

  val getStinLn: Program[String] = GetStrLn(Program.point(_))
  def putStrLn(line: String)     = PutStrLn(line, Return(()))

  val myProgram: Program[Unit] =
    for {
      _    <- putStrLn("Hello")
      _    <- println("world")
      name <- getStinLn
      _    <- println(s"your name: $name")
    } yield ()

}

object exercises {}
