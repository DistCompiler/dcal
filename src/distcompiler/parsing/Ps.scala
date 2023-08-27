package distcompiler.parsing

import cats.*
import cats.syntax.all.given

final case class Ps[+T](value: T)(using val sourceLocation: SourceLocation) {
  def map[U](fn: T => U): Ps[U] =
    copy(value = fn(value))

  def toPsK: PsK[T] = PsK(value, sourceLocation)
}

object Ps {
  given comonadTraverse: Comonad[Ps] with Traverse[Ps] with {
    def extract[A](x: Ps[A]): A = x.value

    def coflatMap[A, B](fa: Ps[A])(f: Ps[A] => B): Ps[B] =
      Ps(f(fa))(using fa.sourceLocation)

    def traverse[G[_]: Applicative, A, B](fa: Ps[A])(f: A => G[B]): G[Ps[B]] =
      f(fa.extract).map(fa.as)

    def foldLeft[A, B](fa: Ps[A], b: B)(f: (B, A) => B): B = f(b, fa.extract)

    def foldRight[A, B](fa: Ps[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = f(fa.extract, lb)
  }
}