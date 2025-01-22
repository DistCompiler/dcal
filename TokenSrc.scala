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

import scala.language.dynamics

import scala.collection.mutable
import scala.annotation.constructorOnly

trait TokenSrc extends Named, Dynamic:
  private val _namespace = mutable.HashMap.empty[String, Token & TokenSrc]

  private class FreshToken(name: String @constructorOnly)
      extends Token,
        TokenSrc,
        Named(using Named.OwnName(nameSegments :+ name))

  def selectDynamic(name: String): Token & TokenSrc =
    _namespace.getOrElseUpdate(name, FreshToken(name))

  // This emulates the overloads for Token.apply, because tokenSrc.foo(...) goes to applyDynamic and I don't want
  // the user to have to write tokenSrc.foo.apply(...)
  // Note: the weird signature is because you aren't allowed to overload applyDynamic, and I don't want to have to reimplement
  // the params with macros... if you use 2 SourceRange params, it will just crash w/ the double .at(...) error.
  def applyDynamic(name: String)(
      params: (Node.Child | IterableOnce[Node.Child] | String | SourceRange)*
  ): Node =
    val node = selectDynamic(name).apply()
    params.foreach:
      case child: Node.Child =>
        node.children.addOne(child)
      case src: SourceRange =>
        node.at(src)
      case children: IterableOnce[Node.Child @unchecked] =>
        node.children ++= children
      case str: String =>
        node.at(str)
    node
