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

package distcompiler.tla

import scala.language.dynamics
import cats.syntax.all.given
import distcompiler.*
import dsl.*

object tokens extends TokenSrc:
  object Module extends Token:
    override val symbolTableFor: Set[Token] =
      Set(Id, OpSym)

    object Extends extends Token
    object Defns extends Token

  object Id extends Token.ShowSource
  object OpSym extends Token.ShowSource
  object Order2 extends Token:
    override val lookedUpBy: Manip[Set[Node]] =
      on(
        Order2.withChildren(tok(Id).map(Set(_)))
      ).value

  object Expr extends Token, TokenSrc:
    object NumberLiteral extends Token.ShowSource
    object StringLiteral extends Token.ShowSource

    object Let extends Token:
      override val symbolTableFor: Set[Token] =
        Set(Id, OpSym)

      object Defns extends Token

    transparent trait TokenWithQuantifierBounds extends Token:
      override val symbolTableFor: Set[Token] =
        Set(Id)

    object Exists extends TokenWithQuantifierBounds
    object Forall extends TokenWithQuantifierBounds
    object Function extends TokenWithQuantifierBounds
    object SetComprehension extends TokenWithQuantifierBounds
    object SetRefinement extends TokenWithQuantifierBounds
    object Choose extends TokenWithQuantifierBounds

    object Except extends Token:
      object Substitutions extends Token
      object Substitution extends Token:
        override val symbolTableFor: Set[Token] =
          Set(Anchor)
      object Path extends Token:
        override val lookedUpBy: Manip[Set[Node]] =
          on(Path).check.as(Set(Anchor()))
      object Anchor extends Token

    object Lambda extends Token:
      override val symbolTableFor: Set[Token] =
        Operator.symbolTableFor

      object Params extends Token

  object Operator extends Token:
    override val symbolTableFor: Set[Token] =
      Set(Id, OpSym)
    override val lookedUpBy: Manip[Set[Node]] =
      on(tok(Operator) *> children(tok(Id, OpSym)).map(Set(_))).value

    object Params extends Token

  object Variable extends Token:
    override val lookedUpBy: Manip[Set[Node]] =
      on(tok(Variable) *> children(tok(Id).map(Set(_)))).value

  object Constant extends Token:
    override val lookedUpBy: Manip[Set[Node]] =
      on(
        tok(Constant).withChildren:
          tok(Id).map(Set(_))
            | refine(Order2.lookedUpBy)
      ).value

  object Recursive extends Token:
    override val lookedUpBy: Manip[Set[Node]] =
      on(
        tok(Recursive).withChildren:
          tok(Id).map(Set(_))
            | refine(Order2.lookedUpBy)
      ).value

  object QuantifierBound extends Token:
    override val lookedUpBy: Manip[Set[Node]] =
      on(
        tok(QuantifierBound)
        *> children:
          tok(Id).map(Set(_))
            | tok(tokens.Ids) *> children:
              repeated(tok(Id)).map(_.toSet)
      ).value

val wellformed: Wellformed =
  import dsl.*
  import tokens as t
  Wellformed:
    Node.Top ::= repeated(t.Module)

    t.Module ::= fields(
      t.Id,
      t.Module.Extends,
      t.Module.Defns
    )
    t.Module.Extends ::= repeated(t.Id)
    t.Module.Defns ::= repeated(
      choice(
        t.Local,
        t.Recursive,
        t.Operator,
        t.Variable,
        t.Constant,
        t.Assumption,
        t.Theorem,
        t.Instance,
        t.Module,
        t.ModuleDefinition
      )
    )

    t.Local ::= choice(
      t.Operator,
      t.Instance,
      t.ModuleDefinition
    )

    t.Recursive ::= choice(
      t.Id,
      t.Order2
    )

    t.ModuleDefinition ::= fields(
      t.Id,
      t.Operator.Params,
      t.Instance
    )

    t.Id ::= Atom
    t.Ids ::= repeated(t.Id, minCount = 1)
    t.OpSym ::= choice(defns.Operator.instances.toSet)
    defns.Operator.instances.foreach(_ ::= Atom)
    t.Order2 ::= fields(
      t.Id,
      embedded[Int]
    )

    t.Expr ::= choice(
      t.Expr.NumberLiteral,
      t.Expr.StringLiteral,
      t.Expr.SetLiteral,
      t.Expr.TupleLiteral,
      t.Expr.RecordLiteral,
      t.Expr.Project,
      t.Expr.OpCall,
      t.Expr.FnCall,
      t.Expr.If,
      t.Expr.Case,
      t.Expr.Let,
      t.Expr.Exists,
      t.Expr.Forall,
      t.Expr.Function,
      t.Expr.SetComprehension,
      t.Expr.SetRefinement,
      t.Expr.Choose,
      t.Expr.Except,
      t.Expr.Lambda
    )

    t.Expr.NumberLiteral ::= Atom
    t.Expr.StringLiteral ::= Atom

    t.Expr.SetLiteral ::= repeated(t.Expr)
    t.Expr.TupleLiteral ::= repeated(t.Expr)
    t.Expr.RecordLiteral ::= repeated(t.Expr.RecordLiteral.Field, minCount = 1)
    t.Expr.RecordLiteral.Field ::= fields(
      t.Id,
      t.Expr
    )
    t.Expr.Project ::= fields(
      t.Expr,
      t.Id
    )
    t.Expr.RecordSetLiteral ::= repeated(
      t.Expr.RecordLiteral.Field,
      minCount = 1
    )
    t.Expr.RecordSetLiteral.Field ::= fields(
      t.Id,
      t.Expr
    )

    t.Expr.OpCall ::= fields(
      choice(t.Id, t.OpSym),
      t.Expr.OpCall.Params
    )
    t.Expr.OpCall.Params ::= repeated(t.Expr)

    t.Expr.FnCall ::= fields(t.Expr, t.Expr)

    t.Expr.If ::= fields(
      t.Expr,
      t.Expr,
      t.Expr
    )

    t.Expr.Case ::= repeated(t.Expr.Case.Branch, minCount = 1)
    t.Expr.Case.Branch ::= fields(
      t.Expr,
      t.Expr
    )

    t.Expr.Let ::= fields(
      t.Expr.Let.Defns,
      t.Expr
    )
    t.Expr.Let.Defns ::= repeated(
      choice(
        t.Operator,
        t.ModuleDefinition,
        t.Recursive
      ),
      minCount = 1
    )

    t.Expr.Exists ::= fields(
      t.QuantifierBounds,
      t.Expr
    )
    t.Expr.Forall ::= fields(
      t.QuantifierBounds,
      t.Expr
    )
    t.Expr.Function ::= fields(
      t.QuantifierBounds,
      t.Expr
    )
    t.Expr.SetComprehension ::= fields(
      t.Expr,
      t.QuantifierBounds
    )
    t.Expr.SetRefinement ::= fields(
      t.QuantifierBound,
      t.Expr
    )
    t.Expr.Choose ::= fields(
      t.QuantifierBound,
      t.Expr
    )

    t.Expr.Except ::= fields(
      t.Expr,
      t.Expr.Except.Substitutions
    )
    t.Expr.Except.Substitutions ::= repeated(
      t.Expr.Except.Substitution,
      minCount = 1
    )
    t.Expr.Except.Substitution ::= fields(
      t.Expr.Except.Path,
      t.Expr
    )
    t.Expr.Except.Path ::= repeated(t.Expr, minCount = 1)
    t.Expr.Except.Anchor ::= Atom

    t.Expr.Lambda ::= fields(
      t.Expr.Lambda.Params,
      t.Expr
    )
    t.Expr.Lambda.Params ::= repeated(
      choice(
        t.Id,
        t.Order2
      ),
      minCount = 1
    )

    t.Operator ::= fields(
      choice(t.Id, t.OpSym),
      t.Operator.Params,
      t.Expr
    )
    t.Operator.Params ::= repeated(
      choice(
        t.Id,
        t.Order2
      )
    )

    t.Variable ::= t.Id
    t.Constant ::= choice(t.Id, t.Order2)
    t.Anonymous ::= Atom
    t.Assumption ::= fields(
      choice(t.Id, t.Anonymous),
      t.Expr
    )
    t.Theorem ::= fields(
      choice(t.Id, t.Anonymous),
      choice(t.Expr, t.Theorem.AssumeProve),
      t.Theorem.Proofs
    )
    // unparsed placeholders
    t.Theorem.AssumeProve ::= AnyShape
    t.Theorem.Proofs ::= AnyShape
    t.UseOrHide ::= AnyShape

    t.Instance ::= fields(
      t.Id,
      t.Instance.Substitutions
    )
    t.Instance.Substitutions ::= repeated(t.Instance.Substitution)
    t.Instance.Substitution ::= fields(
      choice(t.Id, t.OpSym),
      t.Expr
    )

    t.QuantifierBound ::= fields(
      choice(t.Id, t.Ids),
      t.Expr
    )

    t.QuantifierBounds ::= repeated(t.QuantifierBound, minCount = 1)
