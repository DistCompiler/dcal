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

package distcompiler.sexpr

import cats.syntax.all.given

import distcompiler.*

object SExprReader extends Reader:
  import distcompiler.dsl.*
  import distcompiler.Builtin.{Error, SourceMarker}
  import Reader.*

  def wellformed: Wellformed = distcompiler.sexpr.wellformed

  private val alpha: Set[Char] =
    ('a' to 'z').toSet ++ ('A' to 'Z').toSet
  private val pseudoAlpha: Set[Char] =
    Set('-', '.', '/', '_', ':', '*', '+', '=')
  private val digit: Set[Char] = ('0' to '9').toSet
  private val whitespace: Set[Char] = Set(' ', '\n', '\r', '\t')

  private lazy val unexpectedEOF: Manip[SourceRange] =
    consumeMatch: m =>
      addChild(Error("unexpected EOF", SourceMarker(m)))
        *> Manip.pure(m)

  def canBeEncodedAsToken(src: SourceRange): Boolean =
    src.nonEmpty
      && (alpha(src.head.toChar) || pseudoAlpha(src.head.toChar))
      && src.tail.forall(b =>
        alpha(b.toChar) || pseudoAlpha(b.toChar) || digit(b.toChar)
      )

  protected lazy val rules: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(whitespace):
          extendThisNodeWithMatch(rules)
        .on('('):
          addChild(tokens.List())
            .here(extendThisNodeWithMatch(rules))
        .on(')'):
          extendThisNodeWithMatch:
            atParent:
              rules
          | consumeMatch: m =>
            addChild(Error("unexpected end of list", SourceMarker(m)))
              *> rules
        .onOneOf(digit):
          rawMode
        .onOneOf(alpha ++ pseudoAlpha):
          tokenMode
        .on('"'):
          dropMatch(stringMode) // opening `"` is not part of string
        .fallback:
          bytes.selectCount(1):
            consumeMatch: m =>
              addChild(Error("invalid byte", SourceMarker(m)))
                *> rules
          | consumeMatch: m =>
            // a perfectly normal EOF
            on(theTop).check
              *> Manip.pure(m)
          | unexpectedEOF

  private lazy val rawMode: Manip[SourceRange] =
    commit:
      bytes.selectManyLike(digit):
        consumeMatch: m =>
          m.decodeString().toIntOption match
            case None =>
              addChild(
                Error(
                  "length doesn't fit in a machine int",
                  SourceMarker(m)
                )
              )
                *> rules
            case Some(lengthPrefix) =>
              bytes
                .selecting[SourceRange]
                .on(':'):
                  dropMatch:
                    bytes.selectCount(lengthPrefix):
                      consumeMatch: m =>
                        addChild(tokens.Atom(m))
                          *> rules
                  | bytes.getSrc.flatMap: src =>
                    val srcEnd = src.drop(src.length)
                    addChild(
                      Error(
                        "unexpected EOF before end of raw atom",
                        SourceMarker(srcEnd)
                      )
                    )
                      *> Manip.pure(srcEnd)
                .fallback:
                  addChild(
                    Error(
                      "expected : after length prefix",
                      SourceMarker(m)
                    )
                  )
                    *> rules

  private lazy val tokenMode: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(alpha)(tokenMode)
        .onOneOf(pseudoAlpha)(tokenMode)
        .onOneOf(digit)(tokenMode)
        .fallback:
          consumeMatch: m =>
            addChild(tokens.Atom(m))
              *> rules

  private lazy val stringMode: Manip[SourceRange] =
    object builderRef extends Manip.Ref[SourceRange.Builder]

    def addByte(byte: Byte): Manip[SourceRange] =
      dropMatch:
        builderRef.doEffect(_.addOne(byte))
          *> impl

    def addStr(str: String): Manip[SourceRange] =
      dropMatch:
        val bytes = str.getBytes()
        builderRef.doEffect(_.addAll(bytes))
          *> impl

    def finish(rest: Manip[SourceRange]): Manip[SourceRange] =
      dropMatch:
        builderRef.get.flatMap: builder =>
          val stringContents = builder.result()
          addChild(tokens.Atom(stringContents))
            *> rest

    lazy val impl: Manip[SourceRange] =
      commit:
        bytes
          .selecting[SourceRange]
          .on('"')(finish(rules))
          .onSeq("\\\"")(addByte('"'))
          .onSeq("\\\\")(addByte('\\'))
          .onSeq("\\'")(addByte('\''))
          .onSeq("\\n")(addByte('\n'))
          .onSeq("\\t")(addByte('\t'))
          .onSeq("\\r")(addByte('\r'))
          .onSeq("\\\n\r")(impl)
          .onSeq("\\\r\n")(impl)
          .onSeq("\\\n")(impl)
          .onSeq("\\\r")(impl)
          .on('\\'):
            bytes.selectOne:
              consumeMatch: mark =>
                addChild(
                  Error(
                    s"invalid escape sequence ${mark.decodeString()}",
                    Builtin.SourceMarker(mark)
                  )
                )
                  *> addByte('?')
          .fallback:
            // everything else goes in the literal, except EOF in which case we bail to default rules
            bytes.selectOne:
              consumeMatch: m =>
                assert(m.length == 1)
                addByte(m.head)
            | finish(unexpectedEOF)

    commit:
      builderRef.init(SourceRange.newBuilder):
        dropMatch(impl)
