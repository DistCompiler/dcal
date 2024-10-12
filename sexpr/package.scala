package distcompiler.sexpr

import cats.syntax.all.given

import scala.collection.mutable

import distcompiler.*

object tokens:
  object List extends Token
  object Atom extends Token:
    override def showSource: Boolean = true
  object String extends Token:
    override def showSource: Boolean = true

val wellFormed: Wellformed =
  import distcompiler.wf.*
  Wellformed:
    val listContents = repeated(tok(tokens.Atom, tokens.List))
    Node.Top ::= listContents
    tokens.Atom ::= Atom
    tokens.List ::= listContents

object parse:
  def fromFile(path: os.Path): Node.Top =
    fromSourceRange:
      SourceRange.entire(Source.mapFromFile(path))

  def fromSourceRange(sourceRange: SourceRange): Node.Top =
    val top = reader(sourceRange)
    passes.perform(top)
    top

  import dsl.*

  private lazy val passes: Manip[Unit] =
    pass(once = true)
      .rules:
        on(
          tok(tokens.String).map(_.sourceRange)
        ).rewrite: str =>
          val locBuilder = SourceRange.newBuilder
          val nodeBuffer = mutable.ListBuffer.empty[Node]

          @scala.annotation.tailrec
          def impl(str: SourceRange): Unit =
            str match
              case src"\\\\$rest" =>
                locBuilder.addOne('\\')
                impl(rest)
              case src"\\\"$rest" =>
                locBuilder.addOne('"')
                impl(rest)
              case src"\\'$rest" =>
                locBuilder.addOne('\'')
                impl(rest)
              case src"\\n$rest" =>
                locBuilder.addOne('\n')
                impl(rest)
              case src"\\t$rest" =>
                locBuilder.addOne('\t')
                impl(rest)
              case src"\\r$rest" =>
                locBuilder.addOne('\r')
                impl(rest)
              case src"\\\n\r$rest" => impl(rest)
              case src"\\\r\n$rest" => impl(rest)
              case src"\\\n$rest"   => impl(rest)
              case src"\\\r$rest"   => impl(rest)
              case src"\\$rest" =>
                nodeBuffer += Builtin.Error(
                  "invalid string escape",
                  Builtin.SourceMarker().at(rest.take(1))
                )
                locBuilder.addOne('?')
                impl(rest.drop(1))
              case _ if str.nonEmpty =>
                locBuilder.addOne(str.head)
                impl(str.tail)
              case _ => ()

          impl(str)

          nodeBuffer += tokens.Atom(locBuilder.result())
          splice(nodeBuffer.result())

object serialize:
  // using TailCalls rather than cats.Eval because we are mixing imperative
  // and lazy code, and I ran into a bug where cats.Eval (reasonably for its normal use but not here)
  // silently memoized an effectful computation
  import scala.util.control.TailCalls.*

  extension [T](iter: Iterator[T])
    private def intercalate(value: T): Iterator[T] =
      new Iterator[T]:
        var prependSep = false
        def hasNext: Boolean = iter.hasNext
        def next(): T =
          if prependSep && iter.hasNext
          then
            prependSep = false
            value
          else
            prependSep = true
            iter.next()
    private def traverse(fn: T => TailRec[Unit]): TailRec[Unit] =
      def impl: TailRec[Unit] =
        if !iter.hasNext
        then done(())
        else
          for
            () <- fn(iter.next())
            () <- impl
          yield ()

      impl

  def toPrettyString(top: Node.Top): String =
    val out = java.io.ByteArrayOutputStream()
    toPrettyWritable(top).writeBytesTo(out)
    out.toString()

  def toCompactWritable(top: Node.Top): geny.Writable =
    new geny.Writable:
      override def writeBytesTo(out: java.io.OutputStream): Unit =
        def impl(node: Node.Child): TailRec[Unit] =
          (node: @unchecked) match
            case tokens.Atom(atom) =>
              val sourceRange = atom.sourceRange
              out.write(sourceRange.length.toString().getBytes())
              out.write(':')
              sourceRange.writeBytesTo(out)
              done(())
            case tokens.List(list) =>
              for
                () <- done(out.write('('))
                () <- list.children.iterator
                  .traverse(impl)
                () <- done(out.write(')'))
              yield ()

        top.children.iterator
          .map(impl)
          .traverse(identity)
          .result

  def toPrettyWritable(top: Node.Top): geny.Writable =
    new geny.Writable:
      override def writeBytesTo(out: java.io.OutputStream): Unit =
        var indentLevel = 0

        def lzy[T](fn: => T): TailRec[T] =
          tailcall(done(fn))

        val nl: TailRec[Unit] =
          lzy:
            out.write('\n')
            (0 until indentLevel).foreach(_ => out.write(' '))

        def withIndent(fn: => TailRec[Unit]): TailRec[Unit] =
          indentLevel += 2
          for
            () <- tailcall(fn)
            () <- done(indentLevel -= 2)
          yield ()

        def impl(node: Node.Child): TailRec[Unit] =
          (node: @unchecked) match
            case tokens.Atom(atom) =>
              val sourceRange = atom.sourceRange
              out.write(sourceRange.length.toString().getBytes())
              out.write(':')
              sourceRange.writeBytesTo(out)
              done(())
            case tokens.List(list) =>
              if list.children.isEmpty
              then
                out.write('(')
                out.write(')')
                done(())
              else if list.children.length == 1
              then
                for
                  () <- done(out.write('('))
                  () <- impl(list.children.head)
                  () <- done(out.write(')'))
                yield ()
              else
                def writeChildren(iter: Iterator[Node.Child]): TailRec[Unit] =
                  iter
                    .map(impl)
                    .intercalate(nl)
                    .traverse(identity)

                out.write('(')
                for
                  () <- withIndent:
                    list.children.head match
                      case tokens.Atom(atom) =>
                        for
                          () <- impl(atom)
                          () <- nl
                          () <- writeChildren(list.children.iterator.drop(1))
                        yield ()
                      case _ =>
                        for
                          () <- nl
                          () <- writeChildren(list.children.iterator)
                        yield ()
                  () <- done(out.write(')'))
                yield ()

        top.children.iterator
          .map(impl)
          .intercalate(nl)
          .traverse(identity)
          .result