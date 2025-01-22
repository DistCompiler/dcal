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

import sourcecode.*
import cats.data.NonEmptyChain

final case class DebugInfo(records: NonEmptyChain[DebugInfo.Record]):
  override def toString(): String =
    records.iterator
      .map(_.toString())
      .mkString("\nand ")

  @scala.annotation.targetName("concat")
  def ++(other: DebugInfo): DebugInfo =
    DebugInfo(records ++ other.records)

object DebugInfo:
  final case class Record(file: String, fileName: String, line: Int):
    override def toString(): String =
      s"$file:$line"

  given instance(using file: File, fileName: FileName, line: Line): DebugInfo =
    DebugInfo(NonEmptyChain(Record(file.value, fileName.value, line.value)))

  inline def apply()(using DebugInfo): DebugInfo = summon[DebugInfo]

  // Put this in implicit scope (as an inline given) when you want to be sure you're not accidentally
  // summoning DebugInfo that points inside your implementation.
  // It will flag all such mistakes with a compile-time error.
  inline def poison: DebugInfo =
    scala.compiletime.error("implementation bug: used a poison DebugInfo value")

  // If you have a nested scope where you used the above but actually want DebugInfo
  // to work again, shadow the poison given with another inline given that expands to this.
  inline def notPoison: DebugInfo =
    import scala.compiletime.summonInline
    instance(using
      file = summonInline[File],
      fileName = summonInline[FileName],
      line = summonInline[Line]
    )
