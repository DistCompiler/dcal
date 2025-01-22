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

import cats.Eval
import cats.data.Chain
import cats.syntax.all.given
import scala.collection.mutable
import scala.annotation.constructorOnly
import distcompiler.util.toShortString

final case class NodeError(msg: String) extends RuntimeException(msg)

final class Node(val token: Token)(
    childrenInit: IterableOnce[Node.Child] @constructorOnly = Nil
) extends Node.Child,
      Node.Parent,
      Node.Traversable:
  thisNode =>

  def this(token: Token)(childrenInit: Node.Child*) =
    this(token)(childrenInit)

  // !! important !!
  // these vars must init _before_ we make Children, otherwise
  // Children's constructor will see the default 0 value even if it
  // was supposed to start at 1

  // maybe refactor this logic into some kind of Node.Counted interface?
  private var _scopeRelevance: Int =
    if token.canBeLookedUp then 1 else 0

  private var _errorRefCount: Int =
    if token == Builtin.Error
    then 1
    else 0

  val children: Node.Children = Node.Children(this, childrenInit)

  assertErrorRefCounts()

  override def asNode: Node = this

  override type This = Node
  override def cloneEval(): Eval[Node] =
    Chain
      .traverseViaChain(children.toIndexedSeq)(_.cloneEval())
      .map(clonedChildren => Node(token)(clonedChildren.toIterable).like(this))

  private var _sourceRange: SourceRange | Null = null

  def extendLocation(sourceRange: SourceRange): this.type =
    if _sourceRange eq null
    then _sourceRange = sourceRange
    else _sourceRange = _sourceRange.nn <+> sourceRange
    this

  def at(string: String): this.type =
    at(Source.fromString(string))

  def at(source: Source): this.type =
    at(SourceRange.entire(source))

  def at(sourceRange: SourceRange): this.type =
    if _sourceRange eq null
    then
      _sourceRange = sourceRange
      this
    else throw NodeError("node source range already set")

  def like(other: Node): this.type =
    if other._sourceRange eq null
    then this
    else at(other._sourceRange.nn)

  def sourceRange: SourceRange =
    var rangeAcc: SourceRange | Null = null
    traverse:
      case node: Node =>
        if node._sourceRange ne null
        then
          if rangeAcc ne null
          then rangeAcc = rangeAcc.nn <+> node._sourceRange.nn
          else rangeAcc = node._sourceRange
          Node.TraversalAction.SkipChildren
        else Node.TraversalAction.Continue
      case _: Node.Embed[?] =>
        Node.TraversalAction.SkipChildren

    if rangeAcc ne null
    then rangeAcc.nn
    else SourceRange.entire(Source.empty)

  override def unparent(unsafe: Boolean = false): this.type =
    require(parent.nonEmpty)
    if _scopeRelevance > 0
    then parent.get.decScopeRelevance()
    if _errorRefCount > 0
    then parent.get.decErrorRefCount()

    val prevParent = parent.get

    super.unparent(unsafe)
    this

  override def ensureParent(parent: Node.Parent, idxInParent: Int): this.type =
    val prevParent = this.parent

    // We can ensureParent without unparent() if only the idx changes.
    // In that case, don't inc twice.
    val prevParentPtr = prevParent.getOrElse(null)
    if _scopeRelevance > 0 && (prevParentPtr ne parent)
    then parent.incScopeRelevance()
    if _errorRefCount > 0 && (prevParentPtr ne parent)
    then parent.incErrorRefCount()

    super.ensureParent(parent, idxInParent)

    prevParent.foreach(_.assertErrorRefCounts())
    parent.assertErrorRefCounts()

    this

  override def assertErrorRefCounts(): Unit =
    // check only if configured; can be very very slow otherwise
    if Node.assertErrorRefCorrectness
    then
      // may be called during object construction
      if children ne null
      then
        val countErrors =
          children.iterator.filter(_.hasErrors).size
            + (if token == Builtin.Error then 1 else 0)
        assert(
          countErrors == _errorRefCount,
          s"mismatched error counts, $countErrors != $_errorRefCount"
        )

  override def hasErrors: Boolean =
    assertErrorRefCounts()
    _errorRefCount > 0

  override def errors: List[Node] =
    val errorsAcc = mutable.ListBuffer.empty[Node]
    traverse:
      case thisNode: Node if thisNode.token == Builtin.Error =>
        errorsAcc += thisNode
        Node.TraversalAction.SkipChildren

      case _: (Node | Node.Embed[?]) => Node.TraversalAction.Continue

    errorsAcc.result()

  def lookup: List[Node] =
    assert(token.canBeLookedUp)
    parent.map(_.findNodeByKey(thisNode)).getOrElse(Nil)

  def lookupRelativeTo(referencePoint: Node): List[Node] =
    assert(token.canBeLookedUp)
    referencePoint.findNodeByKey(thisNode)

  override def lookupKeys: Set[Node] =
    this.inspect(token.lookedUpBy).getOrElse(Set.empty)
end Node

object Node:
  def unapplySeq(node: Node): Some[(Token, childrenAccessor)] =
    Some((node.token, childrenAccessor((node.children))))

  final class childrenAccessor(val children: Node.Children) extends AnyVal:
    def length: Int = children.length
    def apply(idx: Int): Node.Child = children(idx)
    def drop(n: Int): Seq[Node.Child] = toSeq.drop(n)
    def toSeq: Seq[Node.Child] = children.toSeq

  private val assertErrorRefCorrectness: Boolean =
    val prop = System.getProperty("distcompiler.Node.assertErrorRefCorrectness")
    (prop ne null) && prop.nn.toLowerCase() == "yes"

  enum TraversalAction:
    case SkipChildren, Continue
  end TraversalAction

  sealed trait Traversable:
    thisTraversable =>

    final def traverse(fn: Node.Child => Node.TraversalAction): Unit =
      extension (self: Node.Child)
        def findNextChild: Option[Node.Child] =
          var curr = self
          def extraConditions: Boolean =
            curr.parent.nonEmpty
            // .rightSibling looks at the parent node. If we're looking at thisTraversable,
            // then that means we should stop or we'll be traversing our parent's siblings.
              && (curr ne thisTraversable)
              && !curr.parent.get.isInstanceOf[Node.Top]

          while curr.rightSibling.isEmpty
            && extraConditions
          do curr = curr.parent.get.asNode

          if !extraConditions
          then None
          else curr.rightSibling

      @scala.annotation.tailrec
      def impl(traversable: Node.Child): Unit =
        traversable match
          case node: Node =>
            import TraversalAction.*
            fn(node) match
              case SkipChildren =>
                node.findNextChild match
                  case None        =>
                  case Some(child) => impl(child)
              case Continue =>
                node.firstChild match
                  case None =>
                    node.findNextChild match
                      case None        =>
                      case Some(child) => impl(child)
                  case Some(child) => impl(child)
          case embed: Embed[?] =>
            fn(embed) // result doesn't matter as it has no children
            embed.findNextChild match
              case None        =>
              case Some(child) => impl(child)

      this match
        case thisChild: Node.Child => impl(thisChild)
        case thisTop: Node.Top =>
          thisTop.children.iterator
            .foreach(impl)
  end Traversable

  object Hole extends Token

  sealed trait All extends Cloneable:
    type This <: All
    def cloneEval(): Eval[This]

    def assertErrorRefCounts(): Unit = ()

    final override def clone(): This =
      cloneEval().value

    final def inspect[T](manip: Manip[T]): Option[T] =
      import dsl.*
      atNode(this)(manip.map(Some(_)) | Manip.pure(None))
        .perform()

    def asNode: Node =
      throw NodeError("not a node")

    def asTop: Top =
      throw NodeError("not a top")

    def asParent: Parent =
      throw NodeError("not a parent")

    def asChild: Child =
      throw NodeError("not a child")

    def isChild: Boolean = false

    def hasErrors: Boolean

    def errors: List[Node]

    def parent: Option[Node.Parent] = None

    def idxInParent: Int =
      throw NodeError("tried to get index in node that cannot have a parent")

    final override def hashCode(): Int =
      this match
        case node: Node =>
          (Node, node.token, node.children).hashCode()
        case top: Top =>
          (Top, top.children).hashCode()
        case Embed(value) =>
          (Embed, value).hashCode()

    final override def equals(that: Any): Boolean =
      if this eq that.asInstanceOf[AnyRef]
      then return true
      (this, that) match
        case (thisNode: Node, thatNode: Node) =>
          thisNode.token == thatNode.token
          && (if thisNode.token.showSource
              then thisNode.sourceRange == thatNode.sourceRange
              else true)
          && thisNode.children == thatNode.children
        case (thisTop: Top, thatTop: Top) =>
          thisTop.children == thatTop.children
        case (thisEmbed: Embed[?], thatEmbed: Embed[?]) =>
          thisEmbed.value == thatEmbed.value
        case _ => false

    final override def toString(): String =
      val str =
        sexpr.serialize.toPrettyString(Wellformed.empty.serializeTree(this))

      if this.isInstanceOf[Top]
      then s"[top] $str"
      else str

    final def presentErrors(debug: Boolean = false): String =
      require(hasErrors)
      errors
        .map: err =>
          val msg = err(Builtin.Error.Message)
          val ast = err(Builtin.Error.AST)

          s"${msg.sourceRange.decodeString()} at ${ast.sourceRange.presentationStringLong}\n${if debug then ast.toShortString() else ""}"
        .mkString("\n")

    final def serializedBy(wf: Wellformed): This =
      wf.serializeTree(this).asInstanceOf[This]

    final def toPrettyWritable(wf: Wellformed): geny.Writable =
      sexpr.serialize.toPrettyWritable(serializedBy(wf))

    final def toCompactWritable(wf: Wellformed): geny.Writable =
      sexpr.serialize.toCompactWritable(serializedBy(wf))

    final def toPrettyString(wf: Wellformed): String =
      sexpr.serialize.toPrettyString(serializedBy(wf))

  final class Top(childrenInit: IterableOnce[Node.Child] @constructorOnly)
      extends Parent,
        Traversable:
    def this(childrenInit: Node.Child*) = this(childrenInit)

    val children: Children = Node.Children(this, childrenInit)

    override def asTop: Top = this

    override type This = Top
    override def cloneEval(): Eval[Top] =
      Chain
        .traverseViaChain(children.toIndexedSeq)(_.cloneEval())
        .map(_.toIterable)
        .map(Top(_))

    override def hasErrors: Boolean = children.exists(_.hasErrors)
    override def errors: List[Node] =
      children.iterator
        .filter(_.hasErrors)
        .flatMap(_.errors)
        .toList
  end Top

  object Top

  sealed trait Parent extends All:
    thisParent =>

    override type This <: Parent

    override def asParent: Parent = this

    val children: Node.Children

    final def apply(tok: Token, toks: Token*): Node =
      val results = children.iterator
        .collect:
          case node: Node if node.token == tok || toks.contains(node.token) =>
            node
        .toList

      require(
        results.size == 1,
        s"token(s) not found ${(tok +: toks).map(_.name).mkString(", ")}"
      )
      results.head

    final def firstChild: Option[Node.Child] =
      children.headOption

    @scala.annotation.tailrec
    private[Node] final def incScopeRelevance(): Unit =
      this match
        case root: Node.Top => // nothing to do here
        case thisNode: Node =>
          thisNode._scopeRelevance += 1
          if thisNode._scopeRelevance == 1
          then
            thisNode.parent match
              case None         =>
              case Some(parent) => parent.incScopeRelevance()

    @scala.annotation.tailrec
    private[Node] final def decScopeRelevance(): Unit =
      this match
        case root: Node.Top => // nothing to do here
        case thisNode: Node =>
          assert(thisNode._scopeRelevance > 0)
          thisNode._scopeRelevance -= 1
          if thisNode._scopeRelevance == 0
          then
            thisNode.parent match
              case None         =>
              case Some(parent) => parent.decScopeRelevance()

    @scala.annotation.tailrec
    private[Node] final def incErrorRefCount(): Unit =
      this match
        case root: Node.Top => // nothing to do here
        case thisNode: Node =>
          thisNode._errorRefCount += 1
          if thisNode._errorRefCount == 1
          then
            thisNode.parent match
              case None         =>
              case Some(parent) => parent.incErrorRefCount()

    @scala.annotation.tailrec
    private[Node] final def decErrorRefCount(): Unit =
      this match
        case root: Node.Top => // nothing to do here
        case thisNode: Node =>
          thisNode._errorRefCount -= 1
          if thisNode._errorRefCount == 0
          then
            thisNode.parent match
              case None         =>
              case Some(parent) => parent.decErrorRefCount()

    @scala.annotation.tailrec
    private[Node] final def findNodeByKey(key: Node): List[Node] =
      this match
        case root: Node.Top => Nil
        case thisNode: Node
            if thisNode.token.symbolTableFor.contains(key.token) =>
          import Node.TraversalAction.*
          val resultsList = mutable.ListBuffer.empty[Node]
          thisNode.traverseChildren:
            case irrelevantNode: Node if irrelevantNode._scopeRelevance == 0 =>
              SkipChildren
            case descendantNode: Node =>
              if !descendantNode.token.canBeLookedUp
              then TraversalAction.Continue
              else
                descendantNode.inspect(descendantNode.token.lookedUpBy) match
                  case None => TraversalAction.Continue
                  case Some(descendantKey) =>
                    if key == descendantKey
                    then resultsList.addOne(descendantNode)
                    TraversalAction.Continue
            case _: Node.Embed[?] => SkipChildren

          resultsList.result()
        case thisNode: Node =>
          thisNode.parent match
            case None         => Nil
            case Some(parent) => parent.findNodeByKey(key)

    final def traverseChildren(fn: Node.Child => TraversalAction): Unit =
      children.iterator.foreach(_.traverse(fn))

    final def children_=(childrenInit: IterableOnce[Node.Child]): Unit =
      children.clear()
      children.addAll(childrenInit)

    final def unparentedChildren
        : scala.collection.mutable.ArraySeq[Node.Child] =
      // .unparent() is done by Children .clear
      val result = children.toArray
      children.clear()
      result
  end Parent

  sealed trait Child extends Traversable, All:
    thisChild =>

    override type This <: Child

    override def isChild: Boolean = true

    override def asChild: Child = this

    private var _parent: Parent | Null = null
    private var _idxInParent: Int = -1

    def ensureParent(parent: Parent, idxInParent: Int): this.type =
      val oldParent = _parent
      if _parent eq parent
      then
        // reparenting within the same parent shouldn't really do anything,
        // so don't make a fuss if it happens. The seq ops on Children might do this.
        _idxInParent = idxInParent
        this
      else if _parent eq null
      then
        _parent = parent
        _idxInParent = idxInParent
        this
      else throw NodeError("node already has a parent")

      if oldParent ne null
      then oldParent.nn.assertErrorRefCounts()
      if parent ne null
      then parent.nn.assertErrorRefCounts()

      this

    def unparent(unsafe: Boolean = false): this.type =
      assert(_parent ne null, "tried to unparent floating node")
      val safe = !unsafe
      val oldParent = _parent

      if safe
      then _parent.nn.children.makeHole(_idxInParent)

      _parent = null
      _idxInParent = -1

      if safe && (oldParent ne null)
      then oldParent.nn.assertErrorRefCounts()

      this

    override def parent: Option[Parent] =
      _parent match
        case null            => None
        case _parent: Parent => Some(_parent)

    override def idxInParent: Int =
      if _parent eq null
      then throw NodeError("tried to get index in parent of floating node")
      assert(_idxInParent >= 0)
      _idxInParent

    def lookupKeys: Set[Node] = Set.empty

    final def replaceThis[R <: Node.Child](replacement: => R): R =
      require(parent.nonEmpty)
      val parentTmp = parent.get
      val idxInParentTmp = idxInParent
      val computedReplacement = replacement
      parentTmp.children(idxInParentTmp) = computedReplacement
      computedReplacement

    final def removeThis(): this.type =
      require(parent.nonEmpty)
      val parentTmp = parent.get
      val idxInParentTmp = idxInParent
      parentTmp.children.remove(idxInParentTmp).asInstanceOf[this.type]

    final def rightSibling: Option[Node.Child] =
      parent.flatMap: parent =>
        parent.children.lift(idxInParent + 1)
  end Child

  final case class Embed[T](value: T)(using val meta: EmbedMeta[T])
      extends Child:
    override type This = Embed[T]
    override def cloneEval(): Eval[Embed[T]] =
      Eval.now(Embed(meta.doClone(value)))

    override def hasErrors: Boolean = false
    override def errors: List[Node] = Nil
  end Embed

  object EmbedT extends Token

  final class Children private[Node] (
      val parent: Node.Parent,
      childrenInit: IterableOnce[Node.Child] @constructorOnly
  ) extends mutable.IndexedBuffer[Node.Child]:
    private val _children = mutable.ArrayBuffer.from(childrenInit)
    _children.iterator.zipWithIndex.foreach: (child, idx) =>
      child.ensureParent(parent, idx)
    export _children.{length, apply}

    private def reIdxFromIdx(idx: Int): this.type =
      var curr = idx
      while curr < length
      do
        _children(curr).ensureParent(parent, curr)
        curr += 1
      this

    private[Node] def makeHole(idx: Int): Unit =
      val hole = Hole()
      _children(idx) match
        case node: Node       => hole.like(node)
        case _: Node.Embed[?] =>
      _children(idx) = hole
      hole.ensureParent(parent, idx)

    // can't export due to redef rules
    override def knownSize: Int = _children.knownSize

    def ensureWithKey(elem: Node): this.type =
      val keys = elem.lookupKeys
      if !_children.exists(_.lookupKeys.intersect(keys).nonEmpty)
      then addOne(elem)
      this

    override def prepend(elem: Node.Child): this.type =
      _children.prepend(elem.ensureParent(parent, 0))
      reIdxFromIdx(1)
      parent.assertErrorRefCounts()
      this

    override def insert(idx: Int, elem: Node.Child): Unit =
      _children.insert(idx, elem)
      elem.ensureParent(parent, idx)
      reIdxFromIdx(idx + 1)
      parent.assertErrorRefCounts()

    @scala.annotation.tailrec
    override def insertAll(idx: Int, elems: IterableOnce[Node.Child]): Unit =
      elems match
        case elems: Iterable[Node.Child] =>
          // Keeping this separate allows ensureParent to fail without corrupting the structure.
          elems.iterator.zipWithIndex
            .foreach: (child, childIdx) =>
              child.ensureParent(parent, idx + childIdx)

          _children.insertAll(idx, elems)
          reIdxFromIdx(idx + elems.size)
          parent.assertErrorRefCounts()
        case elems => insertAll(idx, mutable.ArrayBuffer.from(elems))

    override def remove(idx: Int): Node.Child =
      _children(idx).unparent(unsafe = true)
      val child = _children.remove(idx)
      reIdxFromIdx(idx)
      parent.assertErrorRefCounts()
      child

    override def remove(idx: Int, count: Int): Unit =
      (idx until idx + count).foreach: childIdx =>
        _children(childIdx).unparent(unsafe = true)
      _children.remove(idx, count)
      reIdxFromIdx(idx)
      parent.assertErrorRefCounts()

    override def clear(): Unit =
      _children.foreach(_.unparent(unsafe = true))
      _children.clear()
      parent.assertErrorRefCounts()

    override def addOne(elem: Child): this.type =
      val idx = length
      _children.addOne(elem)
      elem.ensureParent(parent, idx)
      parent.assertErrorRefCounts()
      this

    override def update(idx: Int, child: Node.Child): Unit =
      val existingChild = _children(idx)
      if existingChild ne child
      then
        existingChild.unparent(unsafe = true)
        _children(idx) = child
        child.ensureParent(parent, idx)
      parent.assertErrorRefCounts()

    override def iterator: Iterator[Node.Child] =
      (0 until length).iterator.map(this)
  end Children
end Node
