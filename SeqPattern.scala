// Copyright 2025 DCal Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package distcompiler

import cats.syntax.all.given
import dsl.*
import scala.collection.IndexedSeqView

final class SeqPattern[+T](val manip: Manip[SeqPattern.Result[T]])
    extends AnyVal:
  import SeqPattern.*

  def |[U >: T](other: SeqPattern[U]): SeqPattern[U] =
    SeqPattern(manip.combineK(other.manip))

  def productAtRightSibling[U](other: SeqPattern[U]): SeqPattern[(T, U)] =
    SeqPattern:
      manip.lookahead.flatMap:
        case Result.Top(_, _) => backtrack
        case Result.Look(_, idx, t) =>
          atIdx(idx)(other.map((t, _)).manip)
        case Result.Match(_, idx, t) =>
          atIdx(idx + 1)(other.map((t, _)).manip)

  def restrict[U](fn: PartialFunction[T, U]): SeqPattern[U] =
    SeqPattern:
      manip.restrict:
        case Result.Top(top, fn(u))           => Result.Top(top, u)
        case Result.Look(parent, idx, fn(u))  => Result.Look(parent, idx, u)
        case Result.Match(parent, idx, fn(u)) => Result.Match(parent, idx, u)

  def filter(pred: T => Boolean): SeqPattern[T] =
    SeqPattern(manip.filter(res => pred(res.value)))

  def asManip: Manip[T] =
    manip.map(_.value)

  @scala.annotation.targetName("fieldsConcat")
  def ~[Tpl1 <: Tuple, Tpl2 <: Tuple](using
      T <:< Fields[Tpl1]
  )(using DebugInfo)(
      other: SeqPattern[Fields[Tpl2]]
  ): SeqPattern[Fields[Tuple.Concat[Tpl1, Tpl2]]] =
    this
      .productAtRightSibling(other)
      .map: (flds1, flds2) =>
        Fields(flds1.fields ++ flds2.fields)

  @scala.annotation.targetName("fieldsEnd")
  def ~[Tpl <: Tuple, T2](using T <:< Fields[Tpl])(using
      maybeStrip: Fields.MaybeStripTuple1[Tpl, T2]
  )(using DebugInfo)(other: eof.type): SeqPattern[T2] =
    this.productAtRightSibling(atEnd).map(flds => maybeStrip(flds._1.fields))

  @scala.annotation.targetName("fieldsTrailing")
  def ~[Tpl <: Tuple, T2](using
      T <:< Fields[Tpl]
  )(using maybeStrip: Fields.MaybeStripTuple1[Tpl, T2])(
      other: trailing.type
  ): SeqPattern[T2] =
    this.map(flds => maybeStrip(flds.fields))

  def stripFields[TT](using T <:< Fields[TT]): SeqPattern[TT] =
    this.map(t => t.fields)

  @scala.annotation.targetName("logicalAnd")
  def &&(other: SeqPattern[?]): SeqPattern[Unit] =
    this *> other.void

object SeqPattern:
  export applicative.{pure, unit}

  given applicative: cats.Applicative[SeqPattern] with
    override val unit: SeqPattern[Unit] = pure(())
    def pure[A](x: A): SeqPattern[A] = SeqPattern:
      getHandle.map:
        case Manip.Handle.AtTop(top) =>
          Result.Top(top, x)
        case handle: (Manip.Handle.Sentinel | Manip.Handle.AtChild) =>
          Result.Look(handle.parent, handle.idx, x)
    override def map[A, B](fa: SeqPattern[A])(f: A => B): SeqPattern[B] =
      SeqPattern:
        fa.manip.map: result =>
          result.withValue(f(result.value))
    def ap[A, B](ff: SeqPattern[A => B])(fa: SeqPattern[A]): SeqPattern[B] =
      SeqPattern:
        (ff.manip, fa.manip).mapN: (result1, result2) =>
          result1.combine(result2)(_.apply(_))

  given semigroupk: cats.SemigroupK[SeqPattern] with
    def combineK[A](x: SeqPattern[A], y: SeqPattern[A]): SeqPattern[A] =
      SeqPattern(x.manip | y.manip)

  enum Result[+T]:
    val value: T

    case Top(top: Node.Top, value: T)
    case Look(parent: Node.Parent, idx: Int, value: T)
    case Match(parent: Node.Parent, idx: Int, value: T)

    def isLook: Boolean =
      this match
        case _: Look[?] => true
        case _          => false

    def isMatch: Boolean =
      this match
        case _: Match[?] => true
        case _           => false

    def withValue[U](value: U): Result[U] =
      if this.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]
      then this.asInstanceOf
      else
        this match
          case Top(top, _)           => Top(top, value)
          case Look(parent, idx, _)  => Look(parent, idx, value)
          case Match(parent, idx, _) => Match(parent, idx, value)

    def combine[U, V](other: Result[U])(fn: (T, U) => V): Result[V] =
      def result = fn(value, other.value)
      (this, other) match
        case (Top(top1, _), Top(top2, _)) =>
          assert(top1 eq top2)
          Top(top1, result)
        case (Top(_, _), _) | (_, Top(_, _)) =>
          throw NodeError(
            "one part of the same pattern matched top while the other did not"
          )
        case (lhs: (Look[T] | Match[T]), rhs: (Look[U] | Match[U])) =>
          assert(rhs.parent eq rhs.parent)
          val parent = lhs.parent
          def mkLook(idx: Int): Result[V] =
            Look(parent, idx, result)
          def mkMatch(idx: Int): Result[V] =
            Match(parent, idx, result)

          (lhs, rhs) match
            case (Look(_, idx1, _), Look(_, idx2, _)) =>
              mkLook(idx1.max(idx2))
            case (Look(_, idx1, _), Match(_, idx2, _)) if idx1 <= idx2 =>
              mkMatch(idx2)
            case (Look(_, idx1, _), Match(_, idx2, _)) =>
              assert(idx1 > idx2)
              mkLook(idx1)
            case (Match(_, idx1, _), Look(_, idx2, _)) if idx1 < idx2 =>
              mkLook(idx2)
            case (Match(_, idx1, _), Look(_, idx2, _)) =>
              assert(idx1 >= idx2)
              mkMatch(idx1)
            case (Match(_, idx1, _), Match(_, idx2, _)) =>
              mkMatch(idx1.max(idx2))

  object Result:
    extension [T](result: Result.Look[T] | Result.Match[T])
      def parent: Node.Parent =
        result match
          case Result.Look(parent, _, _)  => parent
          case Result.Match(parent, _, _) => parent
      def idx: Int =
        result match
          case Result.Look(_, idx, _)  => idx
          case Result.Match(_, idx, _) => idx

  final case class NodeTokensRestriction(tokens: Set[Token])
      extends Manip.Restriction[Node.All, Result[Node]]:
    protected val impl = {
      case node: Node if tokens(node.token) =>
        Result.Match(node.parent.get, node.idxInParent, node)
    }

  import scala.language.implicitConversions
  implicit def tokenAsTok(token: Token): SeqPattern[Node] =
    tok(token)

  final class Fields[T](val fields: T) extends AnyVal
  object Fields:
    sealed trait MaybeStripTuple1[Tpl <: Tuple, T]:
      def apply(tpl: Tpl): T

    given stripTuple1[T]: MaybeStripTuple1[Tuple1[T], T] with
      def apply(tpl: Tuple1[T]): T = tpl._1

    given notTuple1[Tpl <: Tuple](using
        scala.util.NotGiven[Tpl <:< Tuple1[?]]
    ): MaybeStripTuple1[Tpl, Tpl] with
      def apply(tpl: Tpl): Tpl = tpl

  case object FieldsEndMarker
  case object FieldsTrailingMarker

  object ops:
    def refine[T](manip: Manip[T]): SeqPattern[T] =
      SeqPattern:
        (SeqPattern.unit.manip, manip).mapN(_.withValue(_))

    @scala.annotation.targetName("deferSeqPattern")
    def defer[T](pattern: => SeqPattern[T]): SeqPattern[T] =
      SeqPattern(Manip.ops.defer(pattern.manip))

    def field[T](pattern: SeqPattern[T]): SeqPattern[Fields[Tuple1[T]]] =
      pattern.map(t => Fields(Tuple1(t)))

    def fields[Tpl <: Tuple](
        pattern: SeqPattern[Tpl]
    ): SeqPattern[Fields[Tpl]] =
      pattern.map(Fields(_))

    def skip[T](pattern: SeqPattern[T]): SeqPattern[Fields[EmptyTuple]] =
      pattern.as(Fields(EmptyTuple))

    def anyNode(using DebugInfo): SeqPattern[Node] =
      SeqPattern:
        getNode.restrict:
          case node: Node =>
            Result.Match(node.parent.get, node.idxInParent, node)

    def anyChild(using DebugInfo): SeqPattern[Node.Child] =
      SeqPattern:
        getNode.restrict:
          case child: Node.Child =>
            Result.Match(child.parent.get, child.idxInParent, child)

    def tok(using DebugInfo)(tokens: Token*): SeqPattern[Node] =
      SeqPattern:
        getNode.restrict(NodeTokensRestriction(tokens.toSet))

    def atEnd(using DebugInfo): SeqPattern[Unit] =
      SeqPattern:
        getHandle.restrict:
          case Manip.Handle.Sentinel(parent, idx) =>
            Result.Look(parent, idx, ())

    def atBegin(using DebugInfo): SeqPattern[Unit] =
      SeqPattern:
        getHandle.restrict:
          case Manip.Handle.Sentinel(parent, 0) =>
            Result.Look(parent, 0, ())
          case Manip.Handle.AtChild(parent, 0, _) =>
            Result.Look(parent, 0, ())

    def nodeSpanMatchedBy(using DebugInfo)(
        pattern: SeqPattern[?]
    ): SeqPattern[IndexedSeqView[Node.Child]] =
      SeqPattern:
        getHandle
          .restrict:
            case handle: (Manip.Handle.Sentinel | Manip.Handle.AtChild) =>
              (handle.parent, handle.idx)
          .product(pattern.void.manip)
          .restrict:
            case (
                  (parent, startIdx),
                  patResult: (Result.Look[Unit] | Result.Match[Unit])
                ) =>
              assert(patResult.parent eq parent)
              patResult.withValue:
                parent.children.view.slice(
                  from = startIdx,
                  until =
                    // Endpoint is exclusive.
                    // If we matched idx, add 1 to include it.
                    // Otherwise, default is fine.
                    if patResult.isMatch
                    then patResult.idx + 1
                    else patResult.idx
                )

    export SeqPattern.{FieldsEndMarker as eof, FieldsTrailingMarker as trailing}

    def not[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[Unit] =
      SeqPattern(Manip.Negated(pattern.manip, summon[DebugInfo]) *> unit.manip)

    def optional[T](using DebugInfo)(
        pattern: SeqPattern[T]
    ): SeqPattern[Option[T]] =
      pattern.map(Some(_)) | pure(None)

    def repeated[T](using DebugInfo)(
        pattern: SeqPattern[T]
    ): SeqPattern[List[T]] =
      lazy val impl: SeqPattern[List[T]] =
        (field(pattern) ~ field(defer(impl)) ~ trailing)
          .map(_ :: _)
          | pure(Nil)

      impl

    def repeated1[T](using DebugInfo)(
        pattern: SeqPattern[T]
    ): SeqPattern[List[T]] =
      (field(pattern) ~ field(repeated(pattern)) ~ trailing).map(_ :: _)

    def repeatedSepBy1[T](using
        DebugInfo
    )(sep: SeqPattern[?])(pattern: SeqPattern[T]): SeqPattern[List[T]] =
      (field(pattern) ~ field(
        repeated(skip(sep) ~ field(pattern) ~ trailing)
      ) ~ trailing)
        .map(_ :: _)

    def repeatedSepBy[T](using DebugInfo)(sep: SeqPattern[?])(
        pattern: SeqPattern[T]
    ): SeqPattern[List[T]] =
      repeatedSepBy1(sep)(pattern)
        | pure(Nil)

    def theTop(using DebugInfo): SeqPattern[Node.Top] =
      SeqPattern:
        getHandle.restrict:
          case Manip.Handle.AtTop(top) =>
            Result.Top(top, top)

    def theFirstChild(using DebugInfo): SeqPattern[Node.Child] =
      SeqPattern:
        getHandle.restrict:
          case Manip.Handle.AtChild(parent, 0, child) =>
            Result.Match(parent, 0, child)

    def children[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
      refine(atFirstChild(pattern.asManip))

    def onlyChild[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
      refine(atFirstChild((field(pattern) ~ eof).asManip))

    def parent[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
      refine(atParent(on(pattern).value))

    def ancestor[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
      refine(atAncestor(on(pattern).value))

    def lastChild[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
      refine(atLastChild(on(pattern).value))

    extension [P <: Node.Parent](parentPattern: SeqPattern[P])
      def withChildren[T](using DebugInfo)(
          pattern: SeqPattern[T]
      ): SeqPattern[T] =
        SeqPattern:
          parentPattern.manip.lookahead.flatMap: result =>
            val parent = result.value
            atNode(parent)(atFirstChild(pattern.asManip))
              .map(result.withValue)

    extension (nodePattern: SeqPattern[Node])
      def src(using DebugInfo)(sourceRange: SourceRange): SeqPattern[Node] =
        nodePattern.filter(_.sourceRange == sourceRange)

      def src(using DebugInfo)(str: String): SeqPattern[Node] =
        src(SourceRange.entire(Source.fromString(str)))

    extension [T](hdPattern: SeqPattern[T])
      def *:[Tpl <: Tuple](tlPattern: SeqPattern[Tpl]): SeqPattern[T *: Tpl] =
        (hdPattern, tlPattern).mapN(_ *: _)
