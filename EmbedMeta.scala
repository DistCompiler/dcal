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

import izumi.reflect.Tag
import geny.Writable
import java.io.OutputStream

trait EmbedMeta[T](using val tag: Tag[T]):
  def doClone(self: T): T = self

  def serialize(self: T): Writable
  def deserialize(src: SourceRange): T

  final def canonicalName: String =
    tag.tag.scalaStyledName

  override def equals(that: Any): Boolean =
    that match
      case that: EmbedMeta[?] => tag == that.tag
      case _                  => false

  override def hashCode(): Int = tag.hashCode()

  override def toString(): String =
    s"EmbedMeta($canonicalName)"

object EmbedMeta:
  inline def apply[T: EmbedMeta]: EmbedMeta[T] = summon[EmbedMeta[T]]

  given embedString: EmbedMeta[String] with
    def serialize(self: String): Writable = self
    def deserialize(src: SourceRange): String =
      src.decodeString()

  // all primitive types
  given embedDouble: EmbedMeta[Double] with
    def serialize(self: Double): Writable = self.toString()
    def deserialize(src: SourceRange): Double =
      src.decodeString().toDouble
  given embedFloat: EmbedMeta[Float] with
    def serialize(self: Float): Writable = self.toString()
    def deserialize(src: SourceRange): Float =
      src.decodeString().toFloat
  given embedLong: EmbedMeta[Long] with
    def serialize(self: Long): Writable = self.toString()
    def deserialize(src: SourceRange): Long =
      src.decodeString().toLong
  given embedInt: EmbedMeta[Int] with
    def serialize(self: Int): Writable = self.toString()
    def deserialize(src: SourceRange): Int =
      src.decodeString().toInt
  given embedChar: EmbedMeta[Char] with
    def serialize(self: Char): Writable = self.toString()
    def deserialize(src: SourceRange): Char =
      val str = src.decodeString()
      assert(str.size == 1)
      str.head
  given embedShort: EmbedMeta[Short] with
    def serialize(self: Short): Writable = self.toString()
    def deserialize(src: SourceRange): Short =
      src.decodeString().toShort
  given embedByte: EmbedMeta[Byte] with
    def serialize(self: Byte): Writable = new Writable:
      def writeBytesTo(out: OutputStream): Unit =
        out.write(self)
    def deserialize(src: SourceRange): Byte =
      assert(src.size == 1)
      src.head
  given embedBoolean: EmbedMeta[Boolean] with
    private val t = SourceRange.entire(Source.fromString("true"))
    private val f = SourceRange.entire(Source.fromString("false"))

    def serialize(self: Boolean): Writable =
      if self then t else f
    def deserialize(src: SourceRange): Boolean =
      if src == t
      then true
      else if src == f
      then false
      else throw IllegalArgumentException(s"$src must be $t or $f")

  given embedSingleton[T <: Singleton: Tag](using ValueOf[T]): EmbedMeta[T] with
    def serialize(self: T): Writable = ""
    def deserialize(src: SourceRange): T =
      assert(src.isEmpty)
      summon[ValueOf[T]].value
