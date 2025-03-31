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

import scala.collection.mutable

import distcompiler.*
import dsl.*
import distcompiler.calc.tokens.*

object tokens:
  object Expression extends Token:
    override def showSource: Boolean = false

  object AddOp extends Token:
    override def showSource: Boolean = true

  object DivOp extends Token:
    override def showSource: Boolean = true

  object MulOp extends Token:
    override def showSource: Boolean = true

  object SubOp extends Token:
    override def showSource: Boolean = true

  object Add extends Token:
    override def showSource: Boolean = true

  object Sub extends Token:
    override def showSource: Boolean = true

  object Mul extends Token:
    override def showSource: Boolean = true

  object Div extends Token:
    override def showSource: Boolean = true

  object Number extends Token:
    override def showSource: Boolean = true

val wellformed: Wellformed =
  Wellformed:
    Node.Top ::= repeated(tokens.Expression)

    tokens.Number ::= Atom

    tokens.Add ::= fields(
      tokens.Expression,
      tokens.Expression
    )

    tokens.Sub ::= fields(
      tokens.Expression,
      tokens.Expression
    )

    tokens.Mul ::= fields(
      tokens.Expression,
      tokens.Expression
    )

    tokens.Div ::= fields(
      tokens.Expression,
      tokens.Expression
    )

    tokens.Expression ::= choice(
      tokens.Number,
      tokens.Add,
      tokens.Sub,
      tokens.Mul,
      tokens.Div
    )

object read:
  def fromSourceRange(sourceRange: SourceRange): Node.Top =
    CalcReader(sourceRange)
