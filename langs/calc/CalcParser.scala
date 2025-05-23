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

object CalcParser extends PassSeq:
  import Reader.*
  import CalcReader.*

  lazy val passes = List(
    MulDivPass,
    AddSubPass,
  )

  def inputWellformed: Wellformed = CalcReader.wellformed

  object MulDivPass extends Pass:
    val wellformed = prevWellformed.makeDerived:
      Node.Top.removeCases(MulOp, DivOp)
      Expression.addCases(Mul, Div)
      Mul ::= fields(
        Expression,
        Expression,
      )
      Div ::= fields(
        Expression,
        Expression,
      )
    end wellformed
    val rules = pass(once = false, strategy = pass.topDown)
      .rules:
        on(
          field(tok(Expression))
            ~ skip(tok(MulOp))
            ~ field(tok(Expression))
            ~ trailing,
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Mul(
                left.unparent(),
                right.unparent(),
              ),
            ),
          )
        | on(
          field(tok(Expression))
            ~ skip(tok(DivOp))
            ~ field(tok(Expression))
            ~ trailing,
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Div(
                left.unparent(),
                right.unparent(),
              ),
            ),
          )
    end rules
  end MulDivPass

  object AddSubPass extends Pass:
    val wellformed = prevWellformed.makeDerived:
      Node.Top.removeCases(AddOp, SubOp)
      Expression.addCases(Add, Sub)
      Add ::= fields(
        Expression,
        Expression,
      )
      Sub ::= fields(
        Expression,
        Expression,
      )
    end wellformed
    val rules = pass(once = false, strategy = pass.topDown)
      .rules:
        on(
          field(tok(Expression))
            ~ skip(tok(AddOp))
            ~ field(tok(Expression))
            ~ trailing,
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Add(
                left.unparent(),
                right.unparent(),
              ),
            ),
          )
        | on(
          field(tok(Expression))
            ~ skip(tok(SubOp))
            ~ field(tok(Expression))
            ~ trailing,
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Sub(
                left.unparent(),
                right.unparent(),
              ),
            ),
          )
    end rules
  end AddSubPass
end CalcParser
