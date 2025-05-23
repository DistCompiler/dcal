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

package forja.langs.calc

import cats.syntax.all.given

import forja.*
import forja.dsl.*
import forja.source.Reader
import forja.wf.Wellformed

import lang.*

object CalcEvaluator extends PassSeq:
  import Reader.*
  import CalcReader.*

  def inputWellformed: Wellformed = lang.wf

  val passes = List(
    SimplifyPass,
    RemoveLayerPass,
  )

  object SimplifyPass extends PassSeq.Pass:
    val wellformed = prevWellformed.makeDerived:
      Node.Top ::=! Expression
    val rules = pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
          field(tok(Expression)) *> onlyChild(
            tok(Add).withChildren:
              field(tok(Expression) *> onlyChild(tok(Number)))
                ~ field(tok(Expression) *> onlyChild(tok(Number)))
                ~ eof,
          ),
        ).rewrite: (left, right) =>
          val leftNum = left.unparent().sourceRange.decodeString().toInt
          val rightNum = right.unparent().sourceRange.decodeString().toInt

          splice(
            Expression(
              Number(
                (leftNum + rightNum).toString(),
              ),
            ),
          )
        | on(
          field(tok(Expression)) *> onlyChild(
            tok(Sub).withChildren:
              field(tok(Expression) *> onlyChild(tok(Number)))
                ~ field(tok(Expression) *> onlyChild(tok(Number)))
                ~ eof,
          ),
        ).rewrite: (left, right) =>
          val leftNum = left.unparent().sourceRange.decodeString().toInt
          val rightNum = right.unparent().sourceRange.decodeString().toInt

          splice(
            Expression(
              Number(
                (leftNum - rightNum).toString(),
              ),
            ),
          )
        | on(
          field(tok(Expression)) *> onlyChild(
            tok(Mul).withChildren:
              field(tok(Expression) *> onlyChild(tok(Number)))
                ~ field(tok(Expression) *> onlyChild(tok(Number)))
                ~ eof,
          ),
        ).rewrite: (left, right) =>
          val leftNum = left.unparent().sourceRange.decodeString().toInt
          val rightNum = right.unparent().sourceRange.decodeString().toInt

          splice(
            Expression(
              Number(
                (leftNum * rightNum).toString(),
              ),
            ),
          )
        | on(
          field(tok(Expression)) *> onlyChild(
            tok(Div).withChildren:
              field(tok(Expression) *> onlyChild(tok(Number)))
                ~ field(tok(Expression) *> onlyChild(tok(Number)))
                ~ eof,
          ),
        ).rewrite: (left, right) =>
          val leftNum = left.unparent().sourceRange.decodeString().toInt
          val rightNum = right.unparent().sourceRange.decodeString().toInt

          splice(
            Expression(
              Number(
                (leftNum / rightNum).toString(),
              ),
            ),
          )
    end rules
  end SimplifyPass

  object RemoveLayerPass extends Pass:
    val wellformed = prevWellformed.makeDerived:
      Node.Top ::=! Number
    val rules = pass(once = true, strategy = pass.topDown)
      .rules:
        on(
          tok(Expression).withChildren:
            field(tok(Number))
              ~ eof,
        ).rewrite: (number) =>
          splice(
            number.unparent(),
          )
    end rules
  end RemoveLayerPass
end CalcEvaluator
