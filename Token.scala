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

import scala.collection.mutable
import distcompiler.Manip.ops.backtrack

trait Token extends Equals, Named:
  require(!Token._nameSet.contains(name), s"duplicate name $name")
  Token._nameSet += name

  final def mkNode(childrenInit: IterableOnce[Node.Child] = Nil): Node =
    Node(this)(childrenInit)

  final def mkNode(childrenInit: Node.Child*): Node =
    Node(this)(childrenInit)

  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[Token]

  override def equals(that: Any): Boolean =
    this eq that.asInstanceOf[AnyRef]

  override def hashCode(): Int =
    System.identityHashCode(this)

  override def toString(): String =
    s"Token(@${hashCode()} $name)"

  final def canBeLookedUp: Boolean = !lookedUpBy.isBacktrack

  def symbolTableFor: Set[Token] = Set.empty
  def lookedUpBy: Manip[Set[Node]] = backtrack
  def showSource: Boolean = false
end Token

object Token:
  private val _nameSet: mutable.HashSet[String] = mutable.HashSet.empty

  trait ShowSource extends Token:
    override def showSource: Boolean = true

  private var _freshCounter: Long = 0
  private def incFreshCounter(): Long =
    val result = _freshCounter
    _freshCounter += 1
    result

  abstract class Fresh
      extends Token,
        Named(using Named.OwnName(List(s"$$${{ incFreshCounter() }}")))

  extension (token: Token)
    def apply(children: Node.Child*): Node =
      Node(token)(children)
    def apply(children: IterableOnce[Node.Child]): Node =
      Node(token)(children)
    def apply(sourceRange: String): Node =
      Node(token)().at(sourceRange)
    def apply(sourceRange: SourceRange): Node =
      Node(token)().at(sourceRange)

    def unapplySeq(node: Node): Option[Node.childrenAccessor] =
      if node.token == token
      then Some(Node.childrenAccessor(node.children))
      else None
end Token
