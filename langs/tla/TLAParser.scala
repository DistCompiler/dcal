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

package forja.langs.tla

import cats.syntax.all.given

import forja.*
import forja.dsl.*
import forja.source.{Source, SourceRange}
import forja.wf.Wellformed

import scala.collection.IndexedSeqView

object TLAParser extends PassSeq:
  import TLAReader.*

  def inputWellformed: Wellformed = TLAReader.wellformed

  lazy val passes = List(
    ReservedWordsAndComments,
    ModuleGroups,
    UnitDefns,
    AddLocals,
  )

  val opDeclPattern: SeqPattern[(Node, Int)] =
    val alphaCase =
      field(Alpha)
      ~ field:
        tok(ParenthesesGroup).withChildren:
          field(repeatedSepBy(`,`)(Alpha.src("_")).map(_.size))
            ~ eof
      ~ trailing
    val prefixCase =
      locally:
        field(tok(defns.PrefixOperator.instances*))
          ~ skip(Alpha.src("_"))
          ~ trailing
      .map((_, 1))
    val infixCase =
      locally:
        skip(Alpha.src("_"))
          ~ field(tok(defns.InfixOperator.instances*))
          ~ skip(Alpha.src("_"))
          ~ trailing
      .map((_, 2))
    val postfixCase =
      locally:
        skip(Alpha.src("_"))
          ~ field(tok(defns.PostfixOperator.instances*))
          ~ trailing
      .map((_, 1))

    alphaCase
      | prefixCase
      | infixCase
      | postfixCase

  val expressionDelimiters = Seq(
    defns.VARIABLE,
    defns.VARIABLES,
    defns.CONSTANT,
    defns.CONSTANTS,
    defns.ASSUME,
    defns.AXIOM,
    defns.ASSUMPTION,
    defns.THEOREM,
    defns.PROPOSITION,
    defns.LEMMA,
    defns.COROLLARY,
    defns.INSTANCE,
    defns.LOCAL,
    // For use in parsing opdecls and WITH, so we stop at the right places.
    // Because we call these delimiters, we have to
    // skip over them on purpose in nested expressions
    // that have exposed commas or colons.
    // We special-case: \A, \E, CHOOSE, LAMBDA.
    // All the others are in () etc so don't count.
    `<-`,
    `,`,
    `:`,
    // what the beginning of a proof may look like
    TLAReader.StepMarker,
    defns.PROOF,
    defns.BY,
    defns.OBVIOUS,
    defns.OMITTED,
    // UseOrHide is its own unit
    defns.USE,
    defns.HIDE,
    // may appear between top-level units
    DashSeq,
    // nested modules look like this (we will have parsed them earlier)
    lang.Module,
    // can occur in nested ASSUME ... ASSUME ... PROVE <expr> PROVE
    defns.PROVE,
  )

  final case class RawExpression(nodes: IndexedSeqView[Node.Child]):
    def mkNode: Node =
      lang.Expr(nodes.map(_.unparent()))

  private lazy val operatorDefnBeginnings: SeqPattern[EmptyTuple] =
    (skip(Alpha) ~ skip(optional(ParenthesesGroup)) ~ skip(`_==_`) ~ trailing)
      | (skip(Alpha) ~ skip(SqBracketsGroup) ~ skip(`_==_`) ~ trailing)
      | (skip(Alpha) ~ skip(tok(defns.InfixOperator.instances*)) ~ skip(
        Alpha,
      ) ~ skip(`_==_`) ~ trailing)
      | (skip(Alpha) ~ skip(tok(defns.PostfixOperator.instances*)) ~ skip(
        `_==_`,
      ) ~ trailing)
      | (skip(tok(defns.PrefixOperator.instances*)) ~ skip(Alpha) ~ skip(
        `_==_`,
      ) ~ trailing)

  lazy val rawExpression: SeqPattern[RawExpression] =
    val simpleCases: SeqPattern[Unit] =
      anyChild.void <* not(
        tok(expressionDelimiters*)
        // stop at operator definitions: all valid patterns leading to == here
          | operatorDefnBeginnings,
      )

    lazy val quantifierBound: SeqPattern[EmptyTuple] =
      skip(
        tok(TupleGroup).as(EmptyTuple)
          | repeatedSepBy1(`,`)(Alpha),
      )
        ~ skip(defns.`\\in`)
        ~ skip(defer(impl))
        ~ trailing

    lazy val quantifierBounds: SeqPattern[Unit] =
      repeatedSepBy1(`,`)(quantifierBound).void

    lazy val forallExists: SeqPattern[EmptyTuple] =
      skip(
        tok(LaTexLike).src("\\A") | tok(LaTexLike).src("\\AA") | tok(LaTexLike)
          .src("\\E") | tok(LaTexLike).src("\\EE"),
      )
        ~ skip(quantifierBounds | repeatedSepBy1(`,`)(Alpha))
        ~ skip(`:`)
        ~ trailing

    lazy val choose: SeqPattern[EmptyTuple] =
      skip(defns.CHOOSE)
        ~ skip(quantifierBound | repeatedSepBy1(`,`)(Alpha))
        ~ skip(`:`)
        ~ trailing

    lazy val lambda: SeqPattern[EmptyTuple] =
      skip(defns.LAMBDA)
        ~ skip(repeatedSepBy1(`,`)(Alpha))
        ~ skip(`:`)
        ~ trailing

    // this is an over-approximation of what can show up
    // in an identifier prefix
    // TODO: why does the grammar make it look like it goes the other way round?
    lazy val idFrag: SeqPattern[EmptyTuple] =
      skip(`!`)
        ~ skip(`:`)
        ~ trailing

    lazy val impl: SeqPattern[Unit] =
      repeated1(
        forallExists
          | choose
          | lambda
          | idFrag
          | simpleCases, // last, otherwise it eats parts of the above
      ).void

    nodeSpanMatchedBy(impl).map(RawExpression.apply)

  val proofDelimiters: Seq[Token] = Seq(
    defns.ASSUME,
    defns.VARIABLE,
    defns.VARIABLES,
    defns.CONSTANT,
    defns.CONSTANTS,
    defns.AXIOM,
    defns.ASSUMPTION,
    defns.THEOREM,
    defns.PROPOSITION,
    defns.LEMMA,
    defns.COROLLARY,
    defns.INSTANCE,
    defns.LOCAL,
  )

  lazy val assumeProve: SeqPattern[EmptyTuple] =
    skip(defns.ASSUME)
      ~ skip(
        repeated(
          anyChild <* not(tok(defns.PROVE, defns.ASSUME))
            | defer(assumeProve),
        ),
      )
      ~ skip(defns.PROVE)
      ~ skip(rawExpression)
      ~ trailing

  lazy val rawProofs: SeqPattern[IndexedSeqView[Node.Child]] =
    val simpleCases: SeqPattern[Unit] =
      anyChild.void <* not(
        tok(proofDelimiters*)
          | operatorDefnBeginnings,
      )

    val innerDefn: SeqPattern[EmptyTuple] =
      skip(StepMarker)
        ~ skip(
          (
            skip(optional(defns.DEFINE))
            // you can have a chain of operator defs after DEFINE
              ~ skip(repeatedSepBy1(rawExpression)(operatorDefnBeginnings))
              ~ trailing
          )
            | tok(defns.INSTANCE),
        )
        ~ trailing

    val impl: SeqPattern[Unit] =
      repeated(
        assumeProve
          | innerDefn
          | simpleCases,
      ).void

    nodeSpanMatchedBy(impl)

  object ReservedWordsAndComments extends Pass:
    val wellformed = prevWellformed.makeDerived:
      TLAReader.groupTokens.foreach: tok =>
        tok.addCases(defns.ReservedWord.instances*)
      defns.ReservedWord.instances.iterator
        .filter(!_.isInstanceOf[defns.Operator])
        .foreach(_ ::= Atom)

    val reservedWordMap =
      defns.ReservedWord.instances.view
        .map: word =>
          SourceRange.entire(Source.fromString(word.spelling)) -> word
        .toMap

    val laTexLikeOperatorMap =
      defns.Operator.instances.view
        .filter(_.spelling.startsWith("\\"))
        .map: op =>
          SourceRange.entire(Source.fromString(op.spelling)) -> op
        .toMap

    val rules = pass(once = true, strategy = pass.bottomUp)
      .rules:
        on(
          tok(Alpha).filter(node => reservedWordMap.contains(node.sourceRange)),
        ).rewrite: word =>
          splice(reservedWordMap(word.sourceRange)().like(word))
        | on(
          tok(LaTexLike).filter(node =>
            laTexLikeOperatorMap.contains(node.sourceRange),
          ),
        ).rewrite: op =>
          splice(laTexLikeOperatorMap(op.sourceRange)().like(op))
        | on(
          TLAReader.Comment,
        ).rewrite: _ =>
          splice() // delete it. TODO: maybe try to gather comments?
  end ReservedWordsAndComments

  object ModuleGroups extends Pass:
    val wellformed = prevWellformed.makeDerived:
      Node.Top ::=! repeated(lang.Module)
      TLAReader.groupTokens.foreach: tok =>
        tok.removeCases(defns.MODULE, DashSeq, EqSeq)

      lang.Module ::= fields(
        lang.Id,
        lang.Module.Extends,
        lang.Module.Defns,
      )
      lang.Module.Extends.importFrom(lang.wf)
      lang.Module.Defns ::= repeated(
        choice(ModuleGroup.existingCases + DashSeq + lang.Module),
      )

    val rules = pass(once = false, strategy = pass.topDown)
      .rules:
        // remove top-level modules from ModuleGroup
        on(
          tok(ModuleGroup) *> onlyChild(lang.Module),
        ).rewrite: mod =>
          splice(mod.unparent())
        /* Any module that doesn't have what looks like an unparsed nested
         * module in it. */
        | on(
          skip(DashSeq)
            ~ skip(defns.MODULE)
            ~ field(Alpha)
            ~ skip(DashSeq)
            ~ field(
              optional(
                skip(defns.EXTENDS)
                  ~ field(repeatedSepBy1(`,`)(Alpha))
                  ~ trailing,
              ).map(_.getOrElse(Nil)),
            )
            ~ field(
              repeated(
                anyChild <* not(
                  tok(EqSeq)
                    | (skip(DashSeq) ~ skip(defns.MODULE) ~ skip(Alpha) ~ skip(
                      DashSeq,
                    ) ~ trailing),
                ),
              ),
            )
            ~ skip(EqSeq)
            ~ trailing, // we might be inside another module
        ).rewrite: (name, exts, unitSoup) =>
          splice(
            lang.Module(
              lang.Id().like(name),
              lang.Module.Extends(exts.map(ext => lang.Id().like(ext))),
              lang.Module.Defns(unitSoup.map(_.unparent())),
            ),
          )
  end ModuleGroups

  object UnitDefns extends Pass:
    val wellformed = prevWellformed.makeDerived:
      lang.OpSym.importFrom(lang.wf)

      lang.Module.Defns ::=! repeated(
        choice(
          lang.Operator,
          lang.Variable,
          lang.Constant,
          lang.Assumption,
          lang.Theorem,
          lang.Recursive,
          lang.Instance,
          lang.ModuleDefinition,
          lang.Module,
          lang.UseOrHide,
          defns.LOCAL, // goes away on next pass!
        ),
      )

      TLAReader.groupTokens.foreach: tok =>
        tok.removeCases(
          `_==_`,
          defns.VARIABLE,
          defns.VARIABLES,
          defns.CONSTANT,
          defns.CONSTANTS,
          defns.ASSUME,
          defns.AXIOM,
          defns.ASSUMPTION,
          defns.THEOREM,
          defns.INSTANCE,
        )

      lang.Expr ::= repeated(
        choice(TLAReader.ParenthesesGroup.existingCases),
        minCount = 1,
      )

      TLAReader.LetGroup ::=! repeated(
        choice(
          lang.Operator,
          lang.ModuleDefinition,
          lang.Recursive,
        ),
      )

      lang.Operator.importFrom(lang.wf)
      lang.Variable.importFrom(lang.wf)
      lang.Constant.importFrom(lang.wf)
      lang.OpSym.importFrom(lang.wf)
      lang.Assumption.importFrom(lang.wf)
      lang.Theorem.importFrom(lang.wf)
      lang.Recursive.importFrom(lang.wf)
      lang.Instance.importFrom(lang.wf)
      lang.ModuleDefinition.importFrom(lang.wf)
      lang.UseOrHide.importFrom(lang.wf)

    final case class Instance(
        name: Node,
        substitutions: List[(Node, RawExpression)],
    ):
      def mkNode: Node =
        lang.Instance(
          lang.Id().like(name),
          lang.Instance.Substitutions(
            substitutions.iterator
              .map: (name, expr) =>
                lang.Instance.Substitution(
                  if name.token == Alpha
                  then lang.Id().like(name)
                  else name.unparent(),
                  expr.mkNode,
                ),
          ),
        )

    object Instance:
      val pattern: SeqPattern[Instance] =
        (
          skip(defns.INSTANCE)
            ~ field(TLAReader.Alpha)
            ~ field(
              optional(
                skip(defns.WITH)
                  ~ field(repeatedSepBy1(`,`):
                    field(tok(Alpha) | tok(defns.Operator.instances*))
                      ~ skip(`<-`)
                      ~ field(rawExpression)
                      ~ trailing)
                  ~ trailing,
              ),
            )
            ~ trailing
        ).map: (name, subsOpt) =>
          Instance(name, subsOpt.getOrElse(Nil))

    val rules = pass(once = true, strategy = pass.bottomUp)
      .rules:
        // operator defn variations
        on(
          field(Alpha)
            ~ field(
              optional(
                tok(ParenthesesGroup) *> children(
                  repeatedSepBy(`,`)(TLAReader.Alpha),
                ),
              ),
            )
            ~ skip(`_==_`)
            ~ field(rawExpression)
            ~ trailing,
        ).rewrite: (name, paramsOpt, body) =>
          splice(
            lang.Operator(
              lang.Id().like(name),
              paramsOpt match
                case None => lang.Operator.Params()
                case Some(params) =>
                  lang.Operator.Params(
                    params.iterator.map(p => lang.Id().like(p)),
                  ),
              body.mkNode,
            ),
          )
        | on(
          field(Alpha)
            ~ field(tok(defns.InfixOperator.instances*))
            ~ field(Alpha)
            ~ skip(`_==_`)
            ~ field(rawExpression)
            ~ trailing,
        ).rewrite: (param1, op, param2, body) =>
          splice(
            lang.Operator(
              lang.OpSym(op.unparent()),
              lang.Operator
                .Params(lang.Id().like(param1), lang.Id().like(param2)),
              body.mkNode,
            ),
          )
        | on(
          field(Alpha)
            ~ field(tok(defns.PostfixOperator.instances*))
            ~ skip(`_==_`)
            ~ field(rawExpression)
            ~ trailing,
        ).rewrite: (param, op, body) =>
          splice(
            lang.Operator(
              lang.OpSym(op.unparent()),
              lang.Operator.Params(lang.Id().like(param)),
              body.mkNode,
            ),
          )
        | on(
          field(tok(defns.PrefixOperator.instances*))
            ~ field(Alpha)
            ~ skip(`_==_`)
            ~ field(rawExpression)
            ~ trailing,
        ).rewrite: (op, param, body) =>
          splice(
            lang.Operator(
              lang.OpSym(op.unparent()),
              lang.Operator.Params(lang.Id().like(param)),
              body.mkNode,
            ),
          )
        | on(
          field(Alpha)
            ~ field(SqBracketsGroup)
            ~ skip(`_==_`)
            ~ field(rawExpression)
            ~ trailing,
        ).rewrite: (name, params, body) =>
          splice(
            lang.Operator(
              lang.Id().like(name),
              lang.Operator.Params(),
              lang.Expr(
                SqBracketsGroup(
                  params.children.view.map(_.unparent())
                    ++ List(`|->`())
                    ++ body.nodes.map(_.unparent()),
                ),
              ),
            ),
          )
        // variable decls
        | on(
          skip(tok(defns.VARIABLE, defns.VARIABLES))
            ~ field(repeatedSepBy1(`,`)(Alpha))
            ~ trailing,
        ).rewrite: vars =>
          splice(
            vars.map: v =>
              lang.Variable(lang.Id().like(v)),
          )
        // constant decls
        | on(
          skip(tok(defns.CONSTANT, defns.CONSTANTS))
            ~ field(
              repeatedSepBy1(`,`):
                opDeclPattern
                  | tok(Alpha),
            ) ~ trailing,
        ).rewrite: decls =>
          splice(
            decls.iterator
              .map:
                case (alpha, arity) if alpha.token == Alpha =>
                  lang.Constant(
                    lang.Order2(
                      lang.Id().like(alpha),
                      Node.Embed(arity),
                    ),
                  )
                case (op, arity) =>
                  lang.Constant(
                    lang.Order2(
                      op.unparent(),
                      Node.Embed(arity),
                    ),
                  )
                case alpha: Node =>
                  lang.Constant(lang.Id().like(alpha)),
          )
        // assume
        | on(
          skip(tok(defns.ASSUME, defns.ASSUMPTION, defns.AXIOM))
            ~ field(
              optional(
                field(Alpha)
                  ~ skip(`_==_`)
                  ~ trailing,
              ),
            )
            ~ field(rawExpression)
            ~ trailing,
        ).rewrite: (nameOpt, rawExpr) =>
          splice(
            lang.Assumption(
              nameOpt match
                case None       => lang.Anonymous()
                case Some(name) => lang.Id().like(name),
              rawExpr.mkNode,
            ),
          )
        // theorem
        | on(
          skip(
            tok(defns.THEOREM, defns.PROPOSITION, defns.LEMMA, defns.COROLLARY),
          )
            ~ field(
              optional(
                field(Alpha)
                  ~ skip(`_==_`)
                  ~ trailing,
              ),
            )
            ~ field(nodeSpanMatchedBy(assumeProve.void) | rawExpression)
            ~ field(
              optional(
                // without ~, this works like a lookahead
                tok(
                  TLAReader.StepMarker,
                  defns.PROOF,
                  defns.BY,
                  defns.OBVIOUS,
                  defns.OMITTED,
                )
                  *> rawProofs,
              ),
            )
            ~ trailing,
        ).rewrite: (nameOpt, body, proofsOpt) =>
          splice(
            lang.Theorem(
              nameOpt match
                case None       => lang.Anonymous()
                case Some(name) => lang.Id().like(name),
              body match
                case rawExpr: RawExpression => rawExpr.mkNode
                case assumeProveNodes: IndexedSeqView[Node.Child] =>
                  lang.Theorem.AssumeProve(
                    assumeProveNodes.map(_.unparent()),
                  ),
              proofsOpt match
                case None => lang.Theorem.Proofs()
                case Some(proofs) =>
                  lang.Theorem.Proofs(proofs.map(_.unparent())),
            ),
          )
        // recursive
        | on(
          skip(tok(defns.RECURSIVE))
            ~ field(
              repeatedSepBy1(`,`):
                opDeclPattern
                  | tok(Alpha),
            )
            ~ trailing,
        ).rewrite: decls =>
          splice(
            decls.iterator
              .map:
                case (alpha, arity) if alpha.token == Alpha =>
                  lang.Recursive(
                    lang.Order2(
                      lang.Id().like(alpha),
                      Node.Embed(arity),
                    ),
                  )
                case (op, arity) =>
                  lang.Recursive(
                    lang.Order2(
                      op.unparent(),
                      Node.Embed(arity),
                    ),
                  )
                case alpha: Node =>
                  lang.Recursive(lang.Id().like(alpha)),
          )
        // instance
        | on(
          Instance.pattern,
        ).rewrite: inst =>
          splice(inst.mkNode)
        // module definition
        | on(
          field(Alpha)
            ~ field(
              optional(
                tok(ParenthesesGroup) *> children(
                  repeated1(Alpha),
                ),
              ),
            )
            ~ skip(`_==_`)
            ~ field(Instance.pattern)
            ~ trailing,
        ).rewrite: (name, paramsOpt, instance) =>
          splice(
            lang.ModuleDefinition(
              lang.Id().like(name),
              paramsOpt match
                case None => lang.Operator.Params()
                case Some(params) =>
                  lang.Operator.Params(
                    params.map(param => lang.Id().like(param)),
                  ),
              instance.mkNode,
            ),
          )
        // dashseq
        | on(
          tok(DashSeq),
        ).rewrite: _ =>
          splice()
        // useOrHide
        | on(
          tok(defns.USE, defns.HIDE)
            *> rawProofs,
        ).rewrite: contents =>
          splice(lang.UseOrHide(contents.map(_.unparent())))
  end UnitDefns

  object AddLocals extends Pass:
    val wellformed = prevWellformed.makeDerived:
      lang.Module.Defns.removeCases(defns.LOCAL)
      lang.Module.Defns.addCases(lang.Local)
      lang.Local.importFrom(lang.wf)

    val rules = pass(once = true, strategy = pass.bottomUp)
      .rules:
        // LOCAL <op>
        on(
          field(tok(defns.LOCAL))
            ~ field(
              tok(lang.Operator, lang.Instance, lang.ModuleDefinition),
            )
            ~ trailing,
        ).rewrite: (local, op) =>
          splice(lang.Local(op.unparent()).like(local))
  end AddLocals
end TLAParser
