// Copyright 2024-2025 DCal Team
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

package distcompiler.calc

import cats.syntax.all.given

import distcompiler.*
import dsl.*
import distcompiler.Builtin.{Error, SourceMarker}
import distcompiler.calc.tokens.*

object CalcParser extends PassSeq:
  import distcompiler.dsl.*
  import Reader.*
  import CalcReader.*

  def inputWellformed: Wellformed =
    CalcReader.wellformed.makeDerived:
      Add ::= fields(
        Expression,
        Expression
      )

      Sub ::= fields(
        Expression,
        Expression
      )

      Mul ::= fields(
        Expression,
        Expression
      )

      Div ::= fields(
        Expression,
        Expression
      )

      Expression ::=! choice(
        Number,
        Add,
        Sub,
        Mul,
        Div
      )

  private val mulDivPass = passDef:
    wellformed := inputWellformed.makeDerived:
      Node.Top ::=! repeated(choice(Expression, AddOp, SubOp))

    pass(once = false, strategy = pass.topDown)
      .rules:
        on(
          field(tok(Expression))
            ~ skip(tok(MulOp))
            ~ field(tok(Expression))
            ~ trailing
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Mul(
                left.unparent(),
                right.unparent()
              )
            )
          )
        | on(
          field(tok(Expression))
            ~ skip(tok(DivOp))
            ~ field(tok(Expression))
            ~ trailing
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Div(
                left.unparent(),
                right.unparent()
              )
            )
          )

  private val addSubPass = passDef:
    wellformed := prevWellformed.makeDerived:
      Node.Top ::=! repeated(Expression)

    pass(once = false, strategy = pass.topDown)
      .rules:
        on(
          field(tok(Expression))
            ~ skip(tok(AddOp))
            ~ field(tok(Expression))
            ~ trailing
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Add(
                left.unparent(),
                right.unparent()
              )
            )
          )
        | on(
          field(tok(Expression))
            ~ skip(tok(SubOp))
            ~ field(tok(Expression))
            ~ trailing
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Sub(
                left.unparent(),
                right.unparent()
              )
            )
          )
