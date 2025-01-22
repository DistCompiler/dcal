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

final class ById[T <: AnyRef](val ref: T) extends Equals:
  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[ById[?]]

  override def equals(that: Any): Boolean =
    that match
      case that: ById[t] => ref eq that.ref
      case _             => false

  override def hashCode(): Int =
    System.identityHashCode(ref)

  override def toString(): String =
    s"ById(@${hashCode()} $ref)"
end ById

object ById:
  final class unapplyImpl[T <: AnyRef](val id: ById[T]) extends AnyVal:
    def isEmpty: false = false
    def get: T = id.ref

  def unapply[T <: AnyRef](byId: ById[T]): unapplyImpl[T] = unapplyImpl(byId)
end ById
