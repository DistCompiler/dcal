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

import dsl.*

class WellformedTests extends munit.FunSuite:
  import WellformedTests.*
  val wf = Wellformed:
    Node.Top ::= AnyShape
    tok1 ::= AnyShape
    tok2 ::= AnyShape

    // dummy to make sure the wf knows how to embed ints
    tok3 ::= embedded[Int]

  val tmpSrc = Source.mapFromFile(os.temp("foo"))
  val src1 = SourceRange.entire(tmpSrc).take(2)
  val src2 = SourceRange.entire(tmpSrc).drop(2)
  val src3 = SourceRange.entire(Source.fromString("bar"))

  def joinN(
      breadth: Int,
      iterFn: () => Iterator[Node.Child]
  ): Iterator[List[Node.Child]] =
    breadth match
      case 0 => Iterator.single(Nil)
      case breadth =>
        for
          hd <- iterFn()
          tl <- joinN(breadth - 1, iterFn)
        yield hd.clone() :: tl

  def exampleNodes(depth: Int): Iterator[Node.Child] =
    depth match
      case 0 => Iterator.empty
      case _ if depth > 0 =>
        for
          breadth <- (0 until 3).iterator
          parent <-
            Iterator(tok1(src1), tok2(src2), tok1(src3), tok1())
            ++ locally:
              if breadth == 0
              then Iterator.single(Node.Embed(42))
              else Iterator.empty
          children <- joinN(breadth, () => exampleNodes(depth - 1))
        yield locally:
          val p = parent.clone()
          if breadth != 0
          then p.asParent.children.addAll(children)
          p

  def examples(depth: Int): Iterator[Node.Top] =
    for
      breadth <- (0 until 3).iterator
      children <- joinN(breadth, () => exampleNodes(depth - 1))
    yield Node.Top(children)

  test("serialization back and forth"):
    examples(3).foreach: tree =>
      import dsl.*
      val orig = tree.clone()
      val ser = wf.serializeTree(tree)
      if orig.children.nonEmpty
      then assertNotEquals(ser, orig)

      val deser = wf.deserializeTree(ser)
      assertEquals(deser, orig)

  def expectNoErrors(using munit.Location)(wf: Wellformed)(
      ast: Node.Top
  ): Unit =
    wf.markErrors(ast)
    if ast.hasErrors
    then fail(s"unexpected errors: ${ast.toPrettyString(wf)}")

  def expectErrors(using munit.Location)(wf: Wellformed)(ast: Node.Top): Unit =
    wf.markErrors(ast)
    if !ast.hasErrors
    then fail(s"should have had errors: ${ast.toPrettyString(wf)}")

  val wf1 = Wellformed:
    Node.Top ::= tok2
    tok2 ::= fields(tok3, tok3)
    tok3 ::= Atom

  test("fields: correct"):
    expectNoErrors(wf1):
      Node.Top(
        tok2(
          tok3(),
          tok3()
        )
      )

  test("fields: no fields"):
    expectErrors(wf1):
      Node.Top(tok2())
  test("fields: wrong tok"):
    expectErrors(wf1):
      Node.Top(tok3())
  test("fields: swapped colors"):
    expectErrors(wf1):
      Node.Top(
        tok3(
          tok2(),
          tok2()
        )
      )
  test("fields: one is not an atom"):
    expectErrors(wf1):
      Node.Top(
        tok2(
          tok3(tok3()),
          tok3()
        )
      )

object WellformedTests:
  object tok1 extends Token.ShowSource
  object tok2 extends Token
  object tok3 extends Token
