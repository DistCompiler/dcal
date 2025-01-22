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

import cats.syntax.all.given

import distcompiler.*
import dsl.*

object TLAReader extends Reader:
  lazy val groupTokens: List[Token] = List(
    ModuleGroup,
    ParenthesesGroup,
    SqBracketsGroup,
    BracesGroup,
    TupleGroup,
    LetGroup
  )

  lazy val wellformed = Wellformed:
    Node.Top ::= repeated(ModuleGroup)

    val allToks =
      choice(
        StringLiteral,
        NumberLiteral,
        // ---
        StepMarker,
        // ---
        ModuleGroup,
        ParenthesesGroup,
        SqBracketsGroup,
        BracesGroup,
        TupleGroup,
        LetGroup,
        // ---
        Alpha,
        LaTexLike,
        // ---
        Comment,
        DashSeq,
        EqSeq
      )
        | choice(NonAlpha.instances.toSet)
        | choice(allOperators.toSet)

    ModuleGroup ::= repeated(allToks)

    ParenthesesGroup ::= repeated(allToks)
    SqBracketsGroup ::= repeated(allToks)
    BracesGroup ::= repeated(allToks)
    TupleGroup ::= repeated(allToks)
    LetGroup ::= repeated(allToks)

    StringLiteral ::= Atom
    NumberLiteral ::= Atom
    Alpha ::= Atom
    LaTexLike ::= Atom

    StepMarker ::= fields(
      choice(StepMarker.Num, StepMarker.Plus, StepMarker.Star),
      StepMarker.Ident,
      embedded[Int]
    )
    StepMarker.Num ::= Atom
    StepMarker.Plus ::= Atom
    StepMarker.Star ::= Atom
    StepMarker.Ident ::= Atom

    Comment ::= Atom
    DashSeq ::= Atom
    EqSeq ::= Atom

    allOperators.foreach(_ ::= Atom)
    NonAlpha.instances.foreach(_ ::= Atom)

  import Reader.*

  // override protected def tracePathOpt: Option[os.Path] = Some(os.pwd / "tlareader.log")
  // override protected def traceLimit: Int = 200

  object StringLiteral extends Token.ShowSource
  object NumberLiteral extends Token.ShowSource

  object StepMarker extends Token:
    object Num extends Token.ShowSource
    object Plus extends Token
    object Star extends Token
    object Ident extends Token.ShowSource

  object ModuleGroup extends Token
  object ParenthesesGroup extends Token
  object SqBracketsGroup extends Token
  object BracesGroup extends Token
  object TupleGroup extends Token
  object LetGroup extends Token

  object Alpha extends Token.ShowSource
  object LaTexLike extends Token.ShowSource

  object Comment extends Token.ShowSource
  object DashSeq extends Token
  object EqSeq extends Token

  sealed trait NonAlpha extends Product, Token:
    def spelling: String = productPrefix
  object NonAlpha extends util.HasInstanceArray[NonAlpha]

  case object `:` extends NonAlpha
  case object `::` extends NonAlpha
  case object `<-` extends NonAlpha
  case object `|->` extends NonAlpha
  case object `,` extends NonAlpha
  case object `.` extends NonAlpha
  case object `!` extends NonAlpha
  case object `@` extends NonAlpha
  case object `_==_` extends NonAlpha:
    override def spelling: String = "=="

  case object `WF_` extends NonAlpha
  case object `SF_` extends NonAlpha

  val digits: Set[Char] =
    ('0' to '9').toSet
  val letters: Set[Char] =
    ('a' to 'z').toSet ++ ('A' to 'Z').toSet
  val spaces: Set[Char] =
    Set(' ', '\t', '\n', '\r')

  val allOperators =
    defns.PrefixOperator.instances
      ++ defns.InfixOperator.instances
      ++ defns.PostfixOperator.instances

  val nonAlphaNonLaTexOperators: IArray[defns.Operator] =
    allOperators.filter: op =>
      (!op.spelling.startsWith(
        "\\"
      ) || op == defns.`\\/` || op == defns.`\\`)
        && !letters(op.spelling.head)

  extension (sel: bytes.selecting[SourceRange])
    private def installNonAlphaNonLatexOperators: bytes.selecting[SourceRange] =
      nonAlphaNonLaTexOperators.foldLeft(sel): (sel, op) =>
        sel.onSeq(op.spelling):
          consumeMatch: m =>
            addChild(op(m))
              *> tokens

    private def installNonAlphas: bytes.selecting[SourceRange] =
      NonAlpha.instances.foldLeft(sel): (sel, nonAlpha) =>
        sel.onSeq(nonAlpha.spelling):
          consumeMatch: m =>
            addChild(nonAlpha(m))
              *> tokens

    private def installLatexLikes: bytes.selecting[SourceRange] =
      lazy val impl =
        bytes.selectManyLike(letters):
          consumeMatch: m =>
            addChild(LaTexLike(m))
              *> tokens
      letters.foldLeft(sel): (sel, l) =>
        sel.onSeq(s"\\$l")(impl)

    private def installAlphas(
        next: => Manip[SourceRange]
    ): bytes.selecting[SourceRange] =
      val nxt = defer(next)
      sel.onOneOf(letters + '_'):
        bytes.selectManyLike(letters ++ digits + '_'):
          consumeMatch: m =>
            addChild(Alpha(m))
              *> nxt

    private def installDotNumberLiterals: bytes.selecting[SourceRange] =
      digits.foldLeft(sel): (sel, d) =>
        sel.onSeq(s".$d")(numberLiteralAfterPoint)

    private def installStepMarkers: bytes.selecting[SourceRange] =
      digits
        .foldLeft(sel): (sel, d) =>
          sel.onSeq(s"<$d")(maybeNumStepMarker)
        .onSeq("<*>"):
          consumeMatch: m1 =>
            finishStepMarkerWithIdx(
              m1,
              StepMarker.Star(m1.drop(1).dropRight(1))
            )
        .onSeq("<+>"):
          consumeMatch: m1 =>
            finishStepMarkerWithIdx(
              m1,
              StepMarker.Plus(m1.drop(1).dropRight(1))
            )

  override protected lazy val rules: Manip[SourceRange] =
    moduleSearch

  lazy val moduleSearchNeverMind: Manip[SourceRange] =
    on(
      tok(ModuleGroup) *> refine(atParent(on(theTop).value))
    ).value.flatMap: top =>
      dropMatch:
        effect(top.children.dropRightInPlace(1))
          *> atNode(top)(moduleSearch)

  private lazy val moduleSearch: Manip[SourceRange] =
    lazy val onAlpha: Manip[SourceRange] =
      val validCases =
        on(
          tok(ModuleGroup).withChildren:
            skip(DashSeq)
              ~ skip(Alpha.src("MODULE"))
              ~ skip(optional(Alpha))
              ~ eof
        ).check

      (validCases *> moduleSearch)
      | on(theTop).value.flatMap: top =>
        // Saw an alpha at top level. Delete it.
        effect(top.children.dropRightInPlace(1))
          *> moduleSearch
      | moduleSearchNeverMind // otherwise we failed to parse a module start

    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(spaces)(dropMatch(moduleSearch))
        .onSeq("----"):
          bytes.selectManyLike(Set('-')):
            commit:
              (on(theTop).check
              *> addChild(ModuleGroup())
                .here:
                  consumeMatch: m =>
                    addChild(DashSeq(m))
                      *> moduleSearch
              )
                | (on(
                  tok(ModuleGroup).withChildren:
                    skip(DashSeq)
                      ~ skip(Alpha.src("MODULE"))
                      ~ skip(Alpha)
                      ~ eof
                ).check *> consumeMatch: m =>
                  addChild(DashSeq(m))
                    *> tokens)
                | moduleSearchNeverMind
        .installAlphas(onAlpha)
        .onOneOf(digits):
          // This case handles enough of the number -> alpha promotion that we can parse
          // the module name if it starts with one or more digits.
          bytes.selectManyLike(digits):
            bytes
              .selecting[SourceRange]
              .installAlphas(onAlpha)
              .fallback:
                on(theTop).check
                // Saw digits (?) at top level. We didn't even make a node, so just drop the match and go back to
                // business as usual.
                  *> dropMatch(moduleSearch)
                  | moduleSearchNeverMind // Or we saw an integer in the middle of module pattern, in which case drop this match.
        .fallback:
          bytes.selectOne:
            commit:
              (on(theTop).check *> moduleSearch)
                | moduleSearchNeverMind
          | consumeMatch: m =>
            on(theTop).check *> Manip.pure(m)
              | moduleSearchNeverMind

  private def openGroup(tkn: Token): Manip[SourceRange] =
    consumeMatch: m =>
      addChild(tkn(m))
        .here(tokens)

  private def closeGroup(
      tkn: Token,
      reinterpretTok: Token
  ): Manip[SourceRange] =
    val rest: Manip[SourceRange] =
      if tkn == reinterpretTok
      then on(tok(tkn)).check *> extendThisNodeWithMatch(atParent(tokens))
      else
        on(tok(tkn)).value.flatMap: node =>
          effect(
            node.replaceThis(reinterpretTok(node.unparentedChildren).like(node))
          )
            *> tokens

    on(tok(tkn)).check *> rest

  private def closeGroup(tkn: Token): Manip[SourceRange] =
    closeGroup(tkn, tkn)

  private def invalidGroupClose(tkn: Token, name: String): Manip[SourceRange] =
    // TODO: close a parent group if you can
    consumeMatch: m =>
      addChild(
        Builtin.Error(
          s"unexpected end of $name group",
          Builtin.SourceMarker(m)
        )
      )
        *> tokens

  private lazy val unexpectedEOF: Manip[SourceRange] =
    consumeMatch: m =>
      addChild(
        Builtin.Error(
          "unexpected EOF",
          Builtin.SourceMarker(m)
        )
      )
        *> Manip.pure(m)

  private lazy val letGroupSemantics: Manip[SourceRange] =
    commit:
      on(
        lastChild(Alpha.src("LET"))
      ).value.flatMap: node =>
        effect(node.replaceThis(LetGroup(node.sourceRange)))
          .here(tokens)
      | on(
        tok(LetGroup) *> lastChild(Alpha.src("IN"))
      ).value.flatMap: node =>
        effect(node.removeThis().sourceRange)
          .flatMap: m =>
            extendThisNode(m)
            atParent(tokens)
      | on(
        lastChild(Alpha.src("IN"))
      ).value.flatMap: node =>
        effect(node.removeThis().sourceRange)
          .flatMap: m =>
            addChild(
              Builtin.Error(
                s"unexpected end of LET group",
                Builtin.SourceMarker(m)
              )
            )
              *> tokens
      | on(not(lastChild(Alpha.src("IN")))).check *> tokens

  private lazy val tokens: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(spaces)(dropMatch(tokens))
        .onSeq("===="):
          bytes.selectManyLike(Set('=')):
            (on(tok(ModuleGroup)).check
            *> consumeMatch: m =>
              addChild(EqSeq(m))
              *> locally:
                atParent(on(theTop).check *> moduleSearch)
            )
              | atParent(tokens)
              | consumeMatch: m =>
                addChild(
                  Builtin.Error(
                    "unexpected end of module",
                    Builtin.SourceMarker(m)
                  )
                )
                  *> on(ancestor(theTop)).value.here(moduleSearch)
        .onSeq("----"):
          bytes.selectManyLike(Set('-')):
            consumeMatch: m =>
              addChild(DashSeq(m))
              *> locally:
                val handleNestedModule =
                  atFirstChild(
                    atIdxFromRight(3):
                      on(
                        field(DashSeq)
                          ~ field(tok(Alpha).src("MODULE"))
                          ~ field(Alpha)
                          ~ field(DashSeq)
                          ~ eof
                      ).value.tapEffect: (initDashes, mod, name, endDashes) =>
                        val parent = initDashes.parent.get
                        parent.children.patchInPlace(
                          initDashes.idxInParent,
                          Iterator.single(
                            ModuleGroup(
                              initDashes.unparent(),
                              mod.unparent(),
                              name.unparent(),
                              endDashes.unparent()
                            )
                          ),
                          replaced = 4
                        )
                  ) *> atLastChild(tokens)

                handleNestedModule | tokens
        .on('"')(stringLiteral)
        .onSeq("(*")(multiComment)
        .onSeq("\\*")(lineComment)
        .on('(')(openGroup(ParenthesesGroup))
        .on(')'):
          closeGroup(ParenthesesGroup)
            | invalidGroupClose(ParenthesesGroup, "parentheses")
        .on('[')(openGroup(SqBracketsGroup))
        .on(']'):
          closeGroup(SqBracketsGroup)
            | invalidGroupClose(SqBracketsGroup, "square brackets")
        .on('{')(openGroup(BracesGroup))
        .on('}'):
          closeGroup(BracesGroup)
            | invalidGroupClose(BracesGroup, "braces")
        .installStepMarkers
        .onSeq("<<")(openGroup(TupleGroup))
        .onSeq(">>"):
          closeGroup(TupleGroup)
            | invalidGroupClose(TupleGroup, "tuple")
        .onOneOf(digits)(numberLiteral)
        .installDotNumberLiterals
        .installAlphas(letGroupSemantics)
        .installLatexLikes
        .installNonAlphaNonLatexOperators
        .installNonAlphas
        .fallback:
          bytes.selectOne:
            consumeMatch: m =>
              addChild(
                Builtin.Error(
                  "invalid character",
                  Builtin.SourceMarker(m)
                )
              )
                *> tokens
          | unexpectedEOF

  private lazy val lineComment: Manip[SourceRange] =
    val endComment: Manip[SourceRange] =
      consumeMatch: m =>
        addChild(Comment(m))
          *> tokens

    commit:
      bytes
        .selecting[SourceRange]
        .on('\n')(endComment)
        .onSeq("\r\n")(endComment)
        .fallback:
          bytes.selectOne(lineComment)

  private lazy val multiComment: Manip[SourceRange] =
    multiCommentRec:
      consumeMatch: m =>
        addChild(Comment(m))
          *> tokens

  private def multiCommentRec(outer: Manip[SourceRange]): Manip[SourceRange] =
    lazy val impl: Manip[SourceRange] =
      commit:
        bytes
          .selecting[SourceRange]
          .onSeq("*)")(outer)
          .onSeq("(*")(multiCommentRec(impl))
          .fallback:
            bytes.selectOne:
              impl

    impl

  private lazy val stringLiteral: Manip[SourceRange] =
    object builderRef extends Manip.Ref[SourceRange.Builder]

    def addByte(b: Byte): Manip[SourceRange] =
      dropMatch:
        builderRef.get
          .tapEffect(_.addOne(b))
          *> stringLiteralLoop

    def finishStringLiteral(rest: Manip[SourceRange]): Manip[SourceRange] =
      dropMatch:
        builderRef.get.flatMap: builder =>
          addChild(StringLiteral(builder.result()))
            *> rest

    lazy val stringLiteralLoop: Manip[SourceRange] =
      commit:
        bytes
          .selecting[SourceRange]
          .on('"')(finishStringLiteral(tokens))
          .onSeq("\\\"")(addByte('"'))
          .onSeq("\\\\")(addByte('\\'))
          .onSeq("\\t")(addByte('\t'))
          .onSeq("\\n")(addByte('\n'))
          .onSeq("\\f")(addByte('\f'))
          .onSeq("\\r")(addByte('\r'))
          .fallback:
            bytes.selectOne:
              consumeMatch: m =>
                assert(m.length == 1)
                addByte(m.head)
            | finishStringLiteral(unexpectedEOF)

    builderRef.init(SourceRange.newBuilder)(dropMatch(stringLiteralLoop))

  lazy val endNumberLiteral: Manip[SourceRange] =
    consumeMatch: m =>
      addChild(NumberLiteral(m))
        *> tokens

  private lazy val numberLiteralAfterPoint: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(digits)(numberLiteralAfterPoint)
        .fallback(endNumberLiteral)

  private lazy val numberLiteral: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(digits)(numberLiteral)
        // If we find a letter, then we're not in a num literal.
        // Act like we were processing an Alpha all along.
        .installAlphas(letGroupSemantics)
        .installDotNumberLiterals
        .fallback(endNumberLiteral)

  private lazy val maybeNumStepMarker: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(digits)(maybeNumStepMarker)
        .on('>'):
          consumeMatch: m1 =>
            finishStepMarkerWithIdx(m1, StepMarker.Num(m1.drop(1).dropRight(1)))
        .fallback:
          Reader.matchedRef.get.flatMap: m =>
            // throw away the leading <, then pretend it was a num all along
            // note: this may in fact be `<5x__`  which should lex as op `<`, id `5x__`
            //       (num lexer already handles this)
            addChild(defns.`<`(m.take(1)))
            *> Reader.matchedRef.updated(_ => m.drop(1)):
              numberLiteral

  private def finishStepMarkerWithIdx(
      m1: SourceRange,
      idx: Node
  ): Manip[SourceRange] =
    bytes.selectManyLike(digits ++ letters + '_'):
      consumeMatch: m2 =>
        bytes.selectManyLike(Set('.')):
          consumeMatch: m3 =>
            addChild(
              StepMarker(
                idx,
                StepMarker.Ident(m2),
                Node.Embed(m3.size)
              )
                .at(m1 <+> m2 <+> m3)
            )
              *> tokens
