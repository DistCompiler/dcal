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
import scala.annotation.targetName
import java.nio.charset.{StandardCharsets, Charset}
import java.nio.CharBuffer

trait Reader:
  def wellformed: Wellformed

  protected def tracePathOpt: Option[os.Path] = None
  protected def traceLimit: Int = -1
  protected def rules: Manip[SourceRange]

  final def apply(sourceRange: SourceRange): Node.Top =
    import dsl.*
    val top = Node.Top()

    val manip =
      Manip.ops.atNode(top):
        Reader.srcRef.init(sourceRange):
          Reader.matchedRef.init(sourceRange.take(0)):
            rules.flatMap: _ =>
              if top.hasErrors
              then Manip.unit
              else wellformed.markErrorsPass

    tracePathOpt match
      case None => manip.perform()
      case Some(tracePath) =>
        println(s"!! logging behavior to $tracePath")
        Manip.ops
          .withTracer(manip)(Manip.LogTracer(tracePath, limit = traceLimit))
          .perform()

    top

object Reader:
  import dsl.*

  object srcRef extends Manip.Ref[SourceRange]
  object matchedRef extends Manip.Ref[SourceRange]

  // TODO: other ways to access matched ranges

  def consumeMatch[T](fn: SourceRange => Manip[T]): Manip[T] =
    matchedRef.get.lookahead.flatMap: m =>
      matchedRef.updated(m => m.drop(m.length))(fn(m))

  def dropMatch[T](manip: Manip[T]): Manip[T] =
    matchedRef.updated(m => m.drop(m.length))(manip)

  def extendThisNodeWithMatch[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
    consumeMatch: m =>
      extendThisNode(m)
        *> manip

  def extendThisNode(using DebugInfo)(m: SourceRange): Manip[Unit] =
    getNode.lookahead.flatMap:
      case node: Node =>
        effect(node.extendLocation(m))
      case top: Node.Top =>
        // if top is an extend target, ignore it.
        Manip.pure(())
      case _ => backtrack

  object bytes:
    private def advancingRefs[T](manip: Manip[T]): Manip[T] =
      srcRef.updated(_.tail):
        matchedRef.updated(_.extendRight):
          manip

    final class selecting[T] private (private val cell: selecting.Cell[T])
        extends AnyVal:
      @targetName("onOneOfChars")
      def onOneOf(chars: IterableOnce[Char])(manip: => Manip[T]): selecting[T] =
        onOneOf(chars.iterator.map(_.toByte))(manip)

      def on(char: Char)(manip: => Manip[T]): selecting[T] =
        on(char.toByte)(manip)

      def onOneOf(bytes: IterableOnce[Byte])(manip: => Manip[T]): selecting[T] =
        lazy val impl = manip
        bytes.iterator.foldLeft(this)(_.on(_)(impl))

      def on(byte: Byte)(manip: => Manip[T]): selecting[T] =
        new selecting(cell.add(Iterator.single(byte), defer(manip)))

      def onSeq(
          chars: CharSequence,
          encoding: Charset = StandardCharsets.UTF_8
      )(manip: => Manip[T]): selecting[T] =
        val bytes = SourceRange.entire(
          Source.fromByteBuffer(encoding.encode(CharBuffer.wrap(chars)))
        )
        new selecting(cell.add(bytes.iterator, defer(manip)))

      def fallback(using DebugInfo)(manip: => Manip[T]): Manip[T] =
        val fallbackImpl = defer(manip)
        srcRef.get.lookahead.flatMap: src =>
          val (matched, manip) = cell.lookup(src, fallbackImpl)
          commit:
            srcRef.updated(_.drop(matched.length)):
              matchedRef.updated(_ <+> matched):
                manip

    object selecting:
      def apply[T]: selecting[T] =
        new selecting[T](Cell.Branch(Map.empty, None))

      enum Cell[T]:
        case Branch(map: Map[Byte, Cell[T]], fallbackOpt: Option[Manip[T]])
        case Leaf(manip: Manip[T])

        def lookup(
            src: SourceRange,
            fallback: Manip[T]
        ): (SourceRange, Manip[T]) =
          @scala.annotation.tailrec
          def impl(
              cell: Cell[T],
              src: SourceRange,
              matched: SourceRange,
              otherwise: (SourceRange, Manip[T])
          ): (SourceRange, Manip[T]) =
            cell match
              case Branch(map, fallbackOpt) =>
                def noMatch: (SourceRange, Manip[T]) =
                  fallbackOpt.fold(otherwise)((matched, _))

                if src.isEmpty
                then noMatch
                else
                  map.get(src.head) match
                    case None => noMatch
                    case Some(cell) =>
                      impl(cell, src.tail, matched.extendRight, noMatch)
              case Leaf(manip) => (matched, manip)

          impl(this, src, src.emptyAtOffset, (src.emptyAtOffset, fallback))

        def add(seq: Iterator[Byte], manip: Manip[T]): Cell[T] =
          if seq.hasNext
          then
            val seqHead = seq.next()
            this match
              case Branch(map, fallback) =>
                Branch(
                  map.updatedWith(seqHead)(
                    _.orElse(Some(Branch[T](Map.empty, None)))
                      .map(_.add(seq, manip))
                  ),
                  fallback
                )
              case Leaf(existingManip) =>
                Branch(
                  Map(seqHead -> Branch(Map.empty, None).add(seq, manip)),
                  Some(existingManip)
                )
          else
            this match
              case Branch(map, fallback) =>
                Branch(map, Some(fallback.fold(manip)(_ | manip)))
              case Leaf(manip) => Leaf(manip | manip)

    def selectManyLike[T](using DebugInfo)(bytes: Set[Byte])(
        manip: Manip[T]
    ): Manip[T] =
      lazy val impl: Manip[T] =
        srcRef.get.lookahead.flatMap: src =>
          if src.nonEmpty && bytes(src.head)
          then advancingRefs(impl)
          else Manip.Backtrack(summon[DebugInfo])
        | manip

      impl

    @targetName("selectManyLikeChars")
    def selectManyLike[T](using DebugInfo)(chars: Set[Char])(
        manip: Manip[T]
    ): Manip[T] =
      selectManyLike(chars.map(_.toByte))(manip)

    def selectSeq[T](using DebugInfo)(str: String)(manip: Manip[T]): Manip[T] =
      val bytes = str.getBytes()

      def impl(idx: Int): Manip[T] =
        if idx < bytes.length
        then
          srcRef.get.lookahead.flatMap: src =>
            if src.nonEmpty && src.head == bytes(idx)
            then advancingRefs(impl(idx + 1))
            else backtrack
        else manip

      impl(0)

    def selectCount[T](using DebugInfo)(count: Int)(manip: Manip[T]): Manip[T] =
      srcRef.get.lookahead.flatMap: src =>
        if count <= src.length
        then
          srcRef.updated(_.drop(count)):
            matchedRef.updated(_.extendRightBy(count)):
              manip
        else Manip.Backtrack(summon[DebugInfo])

    def selectOne[T](using DebugInfo)(manip: Manip[T]): Manip[T] =
      srcRef.get.lookahead.flatMap: src =>
        if src.nonEmpty
        then advancingRefs(manip)
        else backtrack

    def getSrc: Manip[SourceRange] =
      srcRef.get
