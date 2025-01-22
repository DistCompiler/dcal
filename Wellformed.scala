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
import scala.collection.mutable
import izumi.reflect.Tag
import scala.util.control.TailCalls.*
import util.TailCallsUtils.*

final class Wellformed private (
    val assigns: Map[Token, Wellformed.Shape],
    val topShape: Wellformed.Shape
):
  import Wellformed.Shape
  import dsl.*
  require(
    assigns.iterator.map(_._1.name).distinct.size == assigns.size,
    s"assigns must be distinct"
  )
  locally:
    val reached = mutable.HashSet.empty[Token]
    def assertReachable(tokenOrEmbed: Token | EmbedMeta[?]): TailRec[Unit] =
      tokenOrEmbed match
        case token: Token =>
          if !reached(token)
          then
            reached += token
            require(
              assigns.contains(token),
              s"token $token must be assigned a shape"
            )
            tailcall(assertReachableFromShapeOrEmbed(assigns(token)))
          else done(())
        case _: EmbedMeta[?] => done(())

    def assertReachableFromShapeOrEmbed(
        shape: Shape | EmbedMeta[?]
    ): TailRec[Unit] =
      shape match
        case _: EmbedMeta[?] => done(())
        case Shape.Atom      => done(())
        case Shape.AnyShape  => done(())
        case Shape.Choice(choices) =>
          choices.iterator.traverse(assertReachable)
        case Shape.Repeat(choice, _) =>
          choice.choices.iterator.traverse(assertReachable)
        case Shape.Fields(fields) =>
          fields.iterator.traverse: field =>
            field.choices.iterator.traverse(assertReachable)

    assertReachableFromShapeOrEmbed(topShape).result

  private lazy val knownMetasByName: Map[SourceRange, EmbedMeta[?]] =
    def implForShape(shape: Shape): Iterator[(SourceRange, EmbedMeta[?])] =
      shape match
        case Shape.Atom | Shape.AnyShape => Iterator.empty
        case Shape.Choice(choices) =>
          choices.iterator.flatMap(implForTokenOrEmbed)
        case Shape.Fields(fields) =>
          fields.iterator.flatMap(implForShape)
        case Shape.Repeat(choice, _) =>
          implForShape(choice)

    def implForTokenOrEmbed(
        tokenOrEmbed: Token | EmbedMeta[?]
    ): Option[(SourceRange, EmbedMeta[?])] =
      tokenOrEmbed match
        case token: Token => None
        case embed: EmbedMeta[?] =>
          val name = SourceRange.entire(Source.fromString(embed.canonicalName))
          Some(name -> embed)

    assigns.iterator
      .map(_._2)
      .flatMap(implForShape)
      .toMap

  private lazy val shortNameByToken: Map[Token, SourceRange] =
    val acc = mutable.HashMap[List[String], mutable.ListBuffer[Token]]()
    acc(Nil) = assigns.iterator.map(_._1).to(mutable.ListBuffer)
    acc(Nil) += Node.EmbedT // we use this for serialization to mark embed sites

    def namesToFix: List[List[String]] =
      acc.iterator
        .collect:
          case (name, buf) if buf.size > 1 => name
        .toList

    @scala.annotation.tailrec
    def impl(): Unit =
      val toFix = namesToFix
      if toFix.nonEmpty
      then
        toFix.foreach: nameTooShort =>
          val toks = acc(nameTooShort)
          acc.remove(nameTooShort)

          var didntGrowCount = 0
          toks.foreach: token =>
            val longerName = token.nameSegments.takeRight(nameTooShort.size + 1)
            if longerName == nameTooShort
            then
              assert(
                didntGrowCount == 0,
                s"duplicate name $nameTooShort in group ${toks.iterator.map(_.name).mkString(", ")}"
              )
              didntGrowCount += 1

            val buf = acc.getOrElseUpdate(longerName, mutable.ListBuffer.empty)
            buf += token

        impl()

    impl()
    acc.iterator
      .filter: (parts, unitList) =>
        if unitList.isEmpty
        then
          assert(parts == Nil)
          false
        else true
      .map: (parts, unitList) =>
        unitList.head -> SourceRange.entire(
          Source.fromString(parts.mkString("."))
        )
      .toMap

  private lazy val tokenByShortName: Map[SourceRange, Token] =
    shortNameByToken.map(_.reverse)

  def shapeOf(tokenOrTop: Token | Node.Top.type): Shape =
    tokenOrTop match
      case token: Token => assigns(token)
      case Node.Top     => topShape

  lazy val markErrorsPass: Manip[Unit] =
    getNode.tapEffect(markErrors).void

  def markErrors(node: Node.All): Unit =
    def implShape(
        desc: String,
        parent: Node.Parent,
        shape: Shape
    ): TailRec[Unit] =
      shape match
        case Shape.AnyShape => done(())
        case Shape.Atom =>
          implShape(desc, parent, Shape.Fields(Nil))
        case choice @ Shape.Choice(_) =>
          implShape(desc, parent, Shape.Fields(List(choice)))
        case Shape.Fields(fields) =>
          if parent.children.size != fields.size
          then
            done:
              val wrongSize = parent.children.size
              parent.children = List(
                Node(Builtin.Error)(
                  Builtin.Error.Message(
                    s"$desc should have exactly ${fields.size} children, but it had $wrongSize instead"
                  ),
                  Builtin.Error.AST(parent.unparentedChildren)
                )
              )
          else
            parent.children.iterator
              .zip(fields.iterator)
              .traverse: (child, choice) =>
                child match
                  case node: Node =>
                    if node.token == Builtin.Error
                    then done(())
                    else if choice.choices(node.token)
                    then tailcall(implForNode(child))
                    else
                      node.replaceThis:
                        Builtin.Error(
                          s"in $desc, found token ${node.token}, but expected ${choice.choices.mkString(" or ")}",
                          child.unparent()
                        )
                      done(())
                  case embed: Node.Embed[?] =>
                    if choice.choices(embed.meta)
                    then done(())
                    else
                      embed.replaceThis:
                        Builtin.Error(
                          s"in $desc, found embed ${embed.meta.canonicalName}, but expected ${choice.choices.mkString(" or ")}",
                          embed.unparent()
                        )
                      done(())
        case Shape.Repeat(choice, minCount) =>
          if parent.children.size < minCount
          then
            done:
              val wrongSize = parent.children.size
              parent.children = List(
                Node(Builtin.Error)(
                  Builtin.Error.Message(
                    s"$desc should have at least $minCount children, but it had $wrongSize instead"
                  ),
                  Builtin.Error.AST(parent.unparentedChildren)
                )
              )
          else
            parent.children.iterator
              .traverse:
                case node: Node =>
                  if node.token == Builtin.Error
                  then done(())
                  else if choice.choices(node.token)
                  then tailcall(implForNode(node))
                  else
                    node.replaceThis:
                      Builtin.Error(
                        s"in $desc, found token ${node.token}, but expected ${choice.choices.mkString(" or ")}",
                        node.unparent()
                      )
                    done(())
                case _: Node.Embed[?] => ???

    def implForNode(node: Node.All): TailRec[Unit] =
      node match
        case top: Node.Top =>
          tailcall(implShape("top", top, topShape))
        case node: Node =>
          assert(assigns.contains(node.token))
          tailcall(implShape(node.token.name, node, assigns(node.token)))
        case embed: Node.Embed[?] => ???

    implForNode(node).result

  def makeDerived(fn: Wellformed.Builder ?=> Unit): Wellformed =
    val builder =
      Wellformed.Builder(mutable.HashMap.from(assigns), Some(topShape))
    fn(using builder)
    builder.build()

  def serializeTree(tree: Node.All): tree.This =
    import sexpr.tokens.{Atom, List as SList}

    val src = SourceRange.entire(Source.fromString(":src"))
    val txt = SourceRange.entire(Source.fromString(":txt"))

    extension [P <: Node.Parent](parent: P)
      def addSerializedChildren(
          children: IterableOnce[Node.Child]
      ): TailRec[P] =
        children.iterator
          .traverse: child =>
            tailcall(impl(child)).map(parent.children.addOne)
          .map(_ => parent)

    extension (token: Token)
      def shortName =
        shortNameByToken.getOrElse(
          token,
          SourceRange.entire(Source.fromString(token.name))
        )

    def impl(tree: Node.All): TailRec[tree.This] =
      val result: TailRec[Node.All] =
        tree match
          case top: Node.Top =>
            val result = Node.Top()
            result.addSerializedChildren(top.children)
          case nd @ Node(token) if !token.showSource =>
            done(Atom(token.shortName))
          case node: Node =>
            val result = SList(Atom(node.token.shortName))
            if node.token.showSource
            then
              val srcRange = node.sourceRange
              result.children.addOne:
                SList(
                  Atom(txt),
                  Atom(srcRange)
                )
              srcRange.source.origin match
                case None =>
                case Some(origin) =>
                  result.children.addOne:
                    SList(
                      Atom(src),
                      Atom(origin.toString),
                      Atom(srcRange.offset.toString()),
                      Atom(srcRange.length.toString())
                    )

            result.addSerializedChildren(node.children)
          case embed: Node.Embed[?] =>
            val bytes =
              Source.fromWritable(embed.meta.serialize(embed.value))
            done:
              SList(
                Atom(shortNameByToken(Node.EmbedT)),
                Atom(embed.meta.canonicalName),
                Atom(SourceRange.entire(bytes))
              )

      result.asInstanceOf[TailRec[tree.This]]

    impl(tree).result
  end serializeTree

  def deserializeTree(tree: Node.All): Node.All =
    import sexpr.tokens.{Atom, List as SList}

    val src = SourceRange.entire(Source.fromString(":src"))
    val txt = SourceRange.entire(Source.fromString(":txt"))
    val srcMap = mutable.HashMap.empty[os.Path, Source]

    lazy val nodeManip: Manip[TailRec[Node.All]] =
      on(
        tok(Atom)
          .map(_.sourceRange)
          .restrict(tokenByShortName)
          <* refine(atFirstChild(on(atEnd).check))
      ).value.map: token =>
        done(token())
      | on(
        tok(SList).withChildren:
          skip(tok(Atom).src(shortNameByToken(Node.EmbedT)))
            ~ field(tok(Atom).map(_.sourceRange).restrict(knownMetasByName))
            ~ field(tok(Atom).map(_.sourceRange))
            ~ eof
      ).value.map: (meta, src) =>
        done(Node.Embed(meta.deserialize(src))(using meta))
      | on(
        anyNode
        *: tok(SList).withChildren:
          field:
            tok(Atom)
              .map(_.sourceRange)
              .restrict(tokenByShortName)
          ~ field:
            optional:
              tok(SList).withChildren:
                skip(tok(Atom).src(txt))
                  ~ field(tok(Atom).map(_.sourceRange))
                  ~ eof
          ~ field:
            optional:
              tok(SList).withChildren:
                skip(tok(Atom).src(src))
                  ~ field(tok(Atom))
                  ~ field(tok(Atom))
                  ~ field(tok(Atom))
                  ~ eof
          ~ trailing
      ).value.map: (node, token, txtOpt, srcOpt) =>
        val result = token()
        srcOpt match
          case None =>
          case Some((path, offset, len)) =>
            val pathVal = os.Path(path.sourceRange.decodeString())
            val offsetVal = offset.sourceRange.decodeString().toInt
            val lenVal = len.sourceRange.decodeString().toInt
            val loc =
              srcMap.getOrElseUpdate(pathVal, Source.mapFromFile(pathVal))
            result.at(SourceRange(loc, offsetVal, lenVal))
        txtOpt match
          case None =>
          case Some(text) if srcOpt.nonEmpty =>
            assert(text == result.sourceRange)
          case Some(text) =>
            result.at(text)

        val skipCount =
          1 + (if srcOpt.nonEmpty then 1 else 0) + (if txtOpt.nonEmpty then 1
                                                    else 0)
        tailcall:
          result
            .addDeserializedChildren(node.children.iterator.drop(skipCount))
            .map(_ => result)
      | on(
        anyNode
      ).value.map: badNode =>
        done(
          Builtin.Error(
            "could not parse node",
            badNode.clone()
          )
        )

    extension [P <: Node.Parent](parent: P)
      def addDeserializedChildren(
          children: IterableOnce[Node.Child]
      ): TailRec[P] =
        children.iterator
          .traverse: child =>
            tailcall(impl(child)).map(parent.children.addOne)
          .map(_ => parent)

    def impl[N <: Node.All](tree: N): TailRec[N] =
      (tree: @unchecked) match
        case top: Node.Top =>
          val result: Node.Top = Node.Top()
          result
            .addDeserializedChildren(top.children)
            .map(_ => result.asInstanceOf[N])
        case node: Node =>
          atNode(node)(nodeManip)
            .perform()
            .asInstanceOf[TailRec[N]]

    impl(tree).result
  end deserializeTree
end Wellformed

object Wellformed:
  val empty: Wellformed = Wellformed:
    Node.Top ::= Wellformed.Shape.AnyShape

  def apply(fn: Builder ?=> Unit): Wellformed =
    val builder = Builder(mutable.HashMap.empty, None)
    fn(using builder)
    builder.build()

  final class Builder private[Wellformed] (
      assigns: mutable.HashMap[Token, Wellformed.Shape],
      private var topShapeOpt: Option[Wellformed.Shape]
  ):
    extension (token: Token | Node.Top.type)
      def ::=(shape: Shape): Unit =
        token match
          case token: Token =>
            require(!assigns.contains(token), s"$token already has a shape")
            assigns(token) = shape
          case Node.Top =>
            require(topShapeOpt.isEmpty, s"top already has a shape")
            topShapeOpt = Some(shape)

      @scala.annotation.targetName("resetTopShape")
      def ::=!(shape: Shape): Unit =
        token match
          case token: Token =>
            require(assigns.contains(token), s"$token has no shape to replace")
            assigns(token) = shape
          case Node.Top =>
            require(topShapeOpt.nonEmpty, s"top has no shape to replace")
            topShapeOpt = Some(shape)

      private def getExistingShape: Shape =
        token match
          case token: Token =>
            require(assigns.contains(token))
            assigns(token)
          case Node.Top =>
            require(topShapeOpt.nonEmpty)
            topShapeOpt.get

      def existingCases: Set[Token | EmbedMeta[?]] =
        getExistingShape match
          case Shape.Choice(choices)    => choices
          case Shape.Repeat(choices, _) => choices.choices
          case shape: (Shape.Fields | Shape.Atom.type | Shape.AnyShape.type) =>
            throw IllegalArgumentException(
              s"$token's shape doesn't have cases ($shape)"
            )

      def removeCases(cases: (Token | EmbedMeta[?])*): Unit =
        getExistingShape match
          case Shape.Choice(choices) =>
            token ::=! Shape.Choice(choices -- cases)
          case Shape.Repeat(choices, minCount) =>
            token ::=! Shape.Repeat(
              Shape.Choice(choices.choices -- cases),
              minCount
            )
          case Shape.Fields(fields) =>
            token ::=! Shape.Fields(
              fields.map(choice => Shape.Choice(choice.choices -- cases))
            )
          case Shape.AnyShape | Shape.Atom =>
          // maybe it helps to assert something here, but it is technically correct to just do nothing

      def addCases(cases: Token*): Unit =
        getExistingShape match
          case Shape.Choice(choices) =>
            token ::=! Shape.Choice(choices ++ cases)
          case Shape.Repeat(choice, minCount) =>
            token ::=! Shape.Repeat(
              Shape.Choice(choice.choices ++ cases),
              minCount
            )
          case shape: (Shape.Fields | Shape.Atom.type | Shape.AnyShape.type) =>
            throw IllegalArgumentException(
              s"$token's shape is not appropriate for adding cases ($shape)"
            )

      def importFrom(wf2: Wellformed): Unit =
        def fillFromShape(shape: Shape): Unit =
          shape match
            case Shape.Atom     =>
            case Shape.AnyShape =>
            case Shape.Choice(choices) =>
              choices.foreach(fillFromTokenOrEmbed)
            case Shape.Repeat(choice, _) =>
              choice.choices.foreach(fillFromTokenOrEmbed)
            case Shape.Fields(fields) =>
              fields.foreach(_.choices.foreach(fillFromTokenOrEmbed))

        def fillFromTokenOrEmbed(tokenOrEmbed: Token | EmbedMeta[?]): Unit =
          tokenOrEmbed match
            case token: Token =>
              if !assigns.contains(token)
              then
                assigns(token) = wf2.assigns(token)
                fillFromShape(wf2.assigns(token))
            case _: EmbedMeta[?] =>

        token match
          case Node.Top =>
            topShapeOpt = Some(wf2.topShape)
            fillFromShape(wf2.topShape)
          case token: Token => fillFromTokenOrEmbed(token)

    private[Wellformed] def build(): Wellformed =
      require(topShapeOpt.nonEmpty)
      new Wellformed(assigns.toMap, topShape = topShapeOpt.get)
  end Builder

  enum Shape:
    case Atom, AnyShape
    case Choice(choices: Set[Token | EmbedMeta[?]])
    case Repeat(choice: Shape.Choice, minCount: Int)
    case Fields(fields: List[Shape.Choice])

  object Shape:
    extension (choice: Shape.Choice)
      @scala.annotation.targetName("combineChoices")
      def |(other: Shape.Choice): Shape.Choice =
        Shape.Choice(choice.choices ++ other.choices)

    def choice(tokens: (Token | EmbedMeta[?])*): Shape.Choice =
      Shape.Choice(tokens.toSet)

    def choice(tokens: Set[Token | EmbedMeta[?]]): Shape.Choice =
      Shape.Choice(tokens)

    def repeated(choice: Shape.Choice, minCount: Int = 0): Shape.Repeat =
      Shape.Repeat(choice, minCount)

    inline def embedded[T: EmbedMeta]: Shape.Choice =
      Shape.Choice(Set(summon[EmbedMeta[T]]))

    def fields(fields: Shape.Choice*): Shape.Fields =
      Shape.Fields(fields.toList)

    import scala.language.implicitConversions
    // TODO: once `into` keyword works, use that
    implicit def tokenAsChoice(token: Token): Shape.Choice =
      Shape.Choice(Set(token))
end Wellformed
