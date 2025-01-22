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

import scala.util.Using

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.channels.FileChannel
import java.nio.channels.FileChannel.MapMode

trait Source:
  def origin: Option[os.Path]
  def byteBuffer: ByteBuffer

  object lines:
    val nlOffsets: IArray[Int] =
      IArray.from:
        SourceRange
          .entire(Source.this)
          .iterator
          .zipWithIndex
          .collect:
            case ('\n', idx) => idx

    def lineColAtOffset(offset: Int): (Int, Int) =
      import scala.collection.Searching.*
      val lineIdx =
        nlOffsets.search(offset) match
          case Found(foundIndex)              => foundIndex
          case InsertionPoint(insertionPoint) => insertionPoint
      val colIdx =
        if lineIdx == 0
        then offset
        else offset - 1 - nlOffsets(lineIdx - 1)

      (lineIdx, colIdx)

    def lineStartOffset(lineIdx: Int): Int =
      if lineIdx == 0
      then 0
      else if lineIdx - 1 == nlOffsets.length
      then SourceRange.entire(Source.this).length
      else nlOffsets(lineIdx - 1) + 1
end Source

object Source:
  object empty extends Source:
    def origin: Option[os.Path] = None
    val byteBuffer: ByteBuffer =
      ByteBuffer.wrap(IArray.emptyByteIArray.unsafeArray)

  def fromString(string: String): Source =
    StringSource(string)

  def fromByteBuffer(byteBuffer: ByteBuffer): Source =
    ByteBufferSource(None, byteBuffer)

  def fromWritable(writable: geny.Writable): Source =
    val out = java.io.ByteArrayOutputStream()
    writable.writeBytesTo(out)
    fromByteBuffer(ByteBuffer.wrap(out.toByteArray()))

  def mapFromFile(path: os.Path): Source =
    Using.resource(FileChannel.open(path.toNIO)): channel =>
      val mappedBuf = channel.map(MapMode.READ_ONLY, 0, channel.size())
      ByteBufferSource(Some(path), mappedBuf)

  final class StringSource(val string: String) extends Source:
    def origin: Option[os.Path] = None
    lazy val byteBuffer: ByteBuffer =
      StandardCharsets.UTF_8.encode(string)

  final class ByteBufferSource(
      val origin: Option[os.Path],
      override val byteBuffer: ByteBuffer
  ) extends Source
