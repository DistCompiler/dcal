package com.github.distcompiler.dcal.parsing

import com.github.distcompiler.dcal.transform.Transform
import cats.Comonad

final case class Ps[T](value: T)(using val sourceLocation: SourceLocation) {
  def map[U](fn: T => U): Ps[U] =
    copy(value = fn(value))

  def up[U >: T]: Ps[U] =
    map(v => v: U)
}

object Ps {
  given comonad: Comonad[Ps] with {
    override def extract[A](x: Ps[A]): A = x.value

    override def map[A, B](fa: Ps[A])(f: A => B): Ps[B] =
      fa.map(f)

    override def coflatMap[A, B](fa: Ps[A])(f: Ps[A] => B): Ps[B] =
      Ps(f(fa))(using fa.sourceLocation)
  }
}