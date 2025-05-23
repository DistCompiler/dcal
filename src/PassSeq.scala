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

import cats.syntax.all.given

import forja.dsl.*
import forja.wf.Wellformed

/** A sequence of passes that iteratively transform a tree of [[forja.Node]].
  *
  * While technically everything this does can be replicated with relatively
  * simple [[forja.manip.Manip]] compositions, the goal here is to provide a
  * default structure for specifying any sort of transformation workflow. See
  * [[forja.langs.calc]] for an example of this trait in action.
  */
transparent trait PassSeq:
  export PassSeq.Pass

  /** Returns the passes that make up this sequence, in the order they should be
    * applied.
    */
  def passes: List[Pass]

  /** Returns the intended input structure for the first pass in the sequence,
    * which is also the starting [[forja.PassSeq.Pass#prevWellformed]] that each
    * pass may transform.
    */
  def inputWellformed: Wellformed

  /** Returns the expected final Wellformed after running all passes.
    */
  final def outputWellformed: Wellformed = outputWellformedImpl

  /** Returns a Manip representing all passes in aggregate. This is what
    * [[forja.PassSeq#apply]] uses internally.
    */
  final def allPasses: Manip[Unit] = allPassesImpl

  private final lazy val (
    outputWellformedImpl: Wellformed,
    allPassesImpl: Manip[Unit],
  ) =
    def whenNoErrors(manip: Manip[Unit]): Manip[Unit] =
      commit:
        (getNode.filter(!_.hasErrors) *> commit(manip))
          | Manip.unit

    val initWf = inputWellformed
    passes.foldLeft((initWf, initWf.markErrorsPass)): (acc, pass) =>
      val (prevWf, accRules) = acc
      val wf = pass.wellformed(using PassSeq.BuildCtx(prevWf))
      (
        wf,
        accRules *> whenNoErrors:
          pass.rules
            *> wf.markErrorsPass,
      )

  /** Run all [[forja.PassSeq#passes]] in sequence on a given
    * [[forja.Node.Top]].
    *
    * Addionally, all wellformed assertions will be performed, and the passes
    * will stop on error. The transformation is destructive. Use
    * [[forja.Node#clone]] to copy the tree if you want to keep the original.
    *
    * @param top
    *   root of the [[forja.Node]] tree to transform
    */
  final def apply(top: Node.Top): Unit =
    initNode(top)(allPasses).perform()
end PassSeq

object PassSeq:
  /** Contextual information about the sequence where this Pass is being used.
    * If the Pass is used in multiple places, relevant parts will be recomputed
    * with a different context.
    *
    * @param inputWellformed
    *   the previous pass's output, or the intial Wellformed.
    */
  final class BuildCtx private[PassSeq] (val inputWellformed: Wellformed)

  abstract class Pass:
    /** Returns the previous pass's [[forja.wf.Wellformed]].
      */
    protected def prevWellformed(using ctx: BuildCtx): Wellformed =
      ctx.inputWellformed

    /** Returns the [[forja.wf.Wellformed]] that outputs of
      * [[forja.PassSeq.Pass#rules]] should satisfy.
      * @note
      *   For maximum reusability, define this in terms of
      *   [[forja.PassSeq.Pass#prevWellformed]]
      */
    def wellformed: BuildCtx ?=> Wellformed

    /** Returns a [[forja.manip.Manip]] defining what this pass does.
      * @note
      *   Normally defined using a variation of [[forja.manip.ManipOps#pass]]
      */
    def rules: Manip[Unit]
  end Pass
end PassSeq
