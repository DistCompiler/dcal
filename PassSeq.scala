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
import cats.syntax.all.given
import dsl.*

transparent trait PassSeq:
  def inputWellformed: Wellformed
  final def outputWellformed: Wellformed =
    assert(entriesSealed)
    entries.last.wellformed

  private val entries = mutable.ListBuffer.empty[PassSeq.Entry]
  private var entriesSealed = false

  protected def prevWellformed(using BuildCtx): Wellformed =
    require(entries.nonEmpty, "there is not previous Wellformed")
    entries.last.wellformed

  protected object wellformed:
    def :=(using ctx: BuildCtx)(wellformed: Wellformed): Unit =
      require(ctx.wellformedOpt.isEmpty)
      ctx.wellformedOpt = Some(wellformed)

  protected final class BuildCtx:
    var wellformedOpt: Option[Wellformed] = None

  final def passDef(using DebugInfo)(
      fn: BuildCtx ?=> Manip[Unit]
  ): PassSeq.Entry =
    require(!entriesSealed, "tried to add a pass after PassSeq was constructed")
    val ctx = BuildCtx()
    val rules = fn(using ctx)

    require(ctx.wellformedOpt.nonEmpty)
    val entry = PassSeq.Entry(ctx.wellformedOpt.get, rules)
    entries += entry

    entry

  private lazy val allPasses =
    entriesSealed = true
    entries
      .foldLeft(inputWellformed.markErrorsPass): (acc, entry) =>
        acc
        *> getNode.flatMap: node =>
          if node.hasErrors
          then Manip.unit
          else
            entry.rules
              *> entry.wellformed.markErrorsPass

  final def apply(top: Node.Top, tracer: Manip.Tracer = Manip.NopTracer): Unit =
    atNode(top)(allPasses)
      .withTracer(tracer)
      .perform()

object PassSeq:
  final case class Entry(wellformed: Wellformed, rules: Manip[Unit])
