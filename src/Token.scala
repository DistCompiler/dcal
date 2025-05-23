// Copyright 2024-2025 Forja Team
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

package forja

import java.lang.ref.{ReferenceQueue, WeakReference}

import forja.dsl.*
import forja.source.SourceRange
import forja.util.{Named, TokenMapFactory}

/** Defines a "token", a unique identifier used to label and distinguish
  * [[forja.Node]] instances.
  *
  * The name is a list of string segments, following an `x.y.z` pattern which
  * maps conceptually to the token object's own name and its package / enclosing
  * object prefix.
  *
  * If this trait is directly inherited by an object or defined using
  * [[forja.wf.WellformedDef#t]], then it will guess its own name using macros
  * (see [[forja.util.Named]]). If doing something more esoteric, pass the
  * correct name as an implicit parameter [[forja.util.Named.OwnName]] to the
  * [[forja.util.Named]] supertrait.
  *
  * @note
  *   The name is the unique identifier, not object identity, so while Scala's
  *   type system makes it hard to get name collisions, they are possible.
  * @note
  *   Name inference is currently known not to work with enum syntax.
  */
trait Token extends Named, TokenMapFactory.Mapped:
  private val sym = Token.TokenSym(nameSegments)

  /** Checks whether this and that have the same token name.
    *
    * Given that two tokens with the same name must also have the same identity,
    * this is done efficiently using an identity comparison.
    */
  final override def equals(that: Any): Boolean =
    that match
      case tok: Token => sym == tok.sym
      case _          => false

  /** Returns a hash code representing the token's name.
    */
  final override def hashCode(): Int = sym.hashCode()

  /** Forwards to the constructor of [[forja.Node]].
    */
  final def mkNode(childrenInit: IterableOnce[Node.Child] = Nil): Node =
    Node(this)(childrenInit)

  /** Forwards to the constructor of [[forja.Node]].
    */
  final def mkNode(childrenInit: Node.Child*): Node =
    Node(this)(childrenInit)

  override def toString(): String =
    s"Token($name)"

  final def canBeLookedUp: Boolean = !lookedUpBy.isBacktrack

  def symbolTableFor: Set[Token] = Set.empty
  def lookedUpBy: Manip[Set[Node]] = backtrack
  def showSource: Boolean = false
end Token

object Token:
  private final class TokenSym private (val nameSegments: List[String]):
    override def equals(that: Any): Boolean =
      this `eq` that.asInstanceOf[AnyRef]
    override def hashCode(): Int = nameSegments.hashCode()
  end TokenSym

  private object TokenSym:
    private final class TokenSymRef(
        val nameSegments: List[String],
        sym: TokenSym,
    ) extends WeakReference[TokenSym](sym, canonicalRefQueue)

    private val canonicalRefQueue = ReferenceQueue[TokenSym]()
    private val canonicalMap =
      scala.collection.concurrent.TrieMap[List[String], TokenSymRef]()

    private def cleanQueue(): Unit =
      var ref: TokenSymRef | Null = null
      while
        ref = canonicalRefQueue.poll().asInstanceOf[TokenSymRef | Null]
        ref ne null
      do canonicalMap.remove(ref.nameSegments)
      end while

    def apply(nameSegments: List[String]): TokenSym =
      var sym: TokenSym | Null = null
      /* If invoked, forms a GC root for our new sym so it won't be reclaimed
       * immediately after construction. */
      lazy val freshSym = new TokenSym(nameSegments)
      while sym eq null do
        cleanQueue()
        val ref = canonicalMap.getOrElseUpdate(
          nameSegments,
          TokenSymRef(nameSegments, freshSym),
        )
        /* We might have just barely sniped a ref that cleanQueue missed. If sym
         * is null, go around again. */
        sym = ref.get()
      end while
      sym.nn
  end TokenSym

  /** Helper trait that overrides [[forja.Token#showSource]] to return true.
    */
  trait ShowSource extends Token:
    override def showSource: Boolean = true

  extension (token: Token)
    /** `Token(x, y, z)` constructor syntax.
      */
    def apply(children: Node.Child*): Node =
      Node(token)(children)

    /** `Token(iter)` constructor syntax.
      */
    def apply(children: IterableOnce[Node.Child]): Node =
      Node(token)(children)

    /** `Token("foo")` constructor syntax, setting the source location to "foo"
      * (implies no children).
      */
    def apply(sourceRange: String): Node =
      Node(token)().at(sourceRange)

    /** `Token(sourceRange)` constructor syntax (implies no children).
      */
    def apply(sourceRange: SourceRange): Node =
      Node(token)().at(sourceRange)

    /** Allow patten matching using Token.
      *
      * @example
      *   {{{??? match case Tok(child1, child2) => ???}}}
      */
    def unapplySeq(node: Node): Option[Node.childrenAccessor] =
      if node.token == token
      then Some(Node.childrenAccessor(node.children))
      else None
end Token
