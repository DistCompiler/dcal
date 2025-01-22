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

import cats.syntax.all.given
import dsl.*

class ManipTests extends munit.FunSuite:
  import ManipTests.*

  def repeat(breadth: Int, iterFn: () => Iterator[Node]): Iterator[List[Node]] =
    breadth match
      case 0 => Iterator.single(Nil)
      case _ if breadth > 0 =>
        for
          hd <- iterFn()
          tl <- repeat(breadth - 1, iterFn)
        yield hd.clone() :: tl

  def exampleNodes(depth: Int, shouldVary: Boolean): Iterator[Node] =
    depth match
      case 0 => Iterator.empty
      case _ if depth > 0 =>
        for
          breadth <- (0 until 3).iterator
          parent <- Iterator(tok1(), if shouldVary then tok2() else tok1())
          children <- repeat(breadth, () => exampleNodes(depth - 1, shouldVary))
        yield locally:
          val p = parent.clone()
          p.children.addAll(children)
          p

  def examples(depth: Int, shouldVary: Boolean): Iterator[Node.Top] =
    for
      breadth <- (0 until 3).iterator
      children <- repeat(breadth, () => exampleNodes(depth - 1, shouldVary))
    yield Node.Top(children)

  def treePairs: Iterator[(Node.Top, Node.Top)] =
    examples(4, shouldVary = true).zip(examples(4, shouldVary = false))

  // test sanity condition
  test("sanity: all trees are equal to themselves".fail):
    treePairs.foreach: (bi, mono) =>
      assertEquals(bi, mono)

  val unifyColors1 =
    pass(once = true)
      .rules:
        on(tok2).rewrite: node =>
          spliceThen(tok1(node.unparentedChildren)):
            continuePassAtNextNode

  test("treePairs unifyColors1"):
    treePairs.foreach: (bi, mono) =>
      atNode(bi)(unifyColors1).perform()
      assertEquals(bi, mono)

  test("unifyColors1 on a single node"):
    val bi = Node.Top(tok2())
    val mono = Node.Top(tok1())
    atNode(bi)(unifyColors1).perform()
    assertEquals(bi, mono)

  val unifyColors2 =
    pass(once = false)
      .rules:
        on(tok2).rewrite: node =>
          splice(tok1(node.unparentedChildren))

  test("treePairs unifyColors2"):
    treePairs.foreach: (bi, mono) =>
      atNode(bi)(unifyColors2).perform()
      assertEquals(bi, mono)

  val unifyColors3 =
    pass(once = true, strategy = pass.bottomUp)
      .rules:
        on(tok2).rewrite: node =>
          splice(tok1(node.unparentedChildren))

  test("treePairs unifyColors3"):
    treePairs.foreach: (bi, mono) =>
      atNode(bi)(unifyColors3).perform()
      assertEquals(bi, mono)

  val unifyColors4 =
    pass(once = true, strategy = pass.bottomUp)
      .rules:
        on(repeated1(tok2)).rewrite: nodes =>
          splice(nodes.map(node => tok1(node.unparentedChildren)))

  test("treePairs unifyColors4"):
    treePairs.foreach: (bi, mono) =>
      atNode(bi)(unifyColors4).perform()
      assertEquals(bi, mono)

  val unifyColors5 =
    pass(once = true, strategy = pass.bottomUp)
      .rules:
        on(nodeSpanMatchedBy(repeated1(tok2).void)).rewrite: span =>
          splice(
            span.map(node => tok1(node.asInstanceOf[Node].unparentedChildren))
          )

  test("treePairs unifyColors5"):
    treePairs.foreach: (bi, mono) =>
      atNode(bi)(unifyColors5).perform()
      assertEquals(bi, mono)

  val unifyColors6 =
    pass(once = true, strategy = pass.bottomUp)
      .rules:
        on(nodeSpanMatchedBy(repeated1(anyChild <* not(tok1)).void)).rewrite:
          span =>
            splice(
              span.map(node => tok1(node.asInstanceOf[Node].unparentedChildren))
            )

  test("treePairs unifyColors6"):
    treePairs.foreach: (bi, mono) =>
      atNode(bi)(unifyColors6).perform()
      assertEquals(bi, mono)

  val unifyColors7 =
    pass(once = true, strategy = pass.bottomUp)
      .rules:
        on(nodeSpanMatchedBy(repeated1(anyChild <* not(repeated1(tok1))).void))
          .rewrite: span =>
            splice(
              span.map(node => tok1(node.asInstanceOf[Node].unparentedChildren))
            )

  test("treePairs unifyColors7"):
    treePairs.foreach: (bi, mono) =>
      atNode(bi)(unifyColors7).perform()
      assertEquals(bi, mono)

  val unifyColors8 =
    pass(once = true, strategy = pass.bottomUp)
      .rules:
        on(tok(tok2) *> repeated1(tok2)).rewrite: nodes =>
          splice(nodes.map(node => tok1(node.unparentedChildren)))

  test("treePairs unifyColors8"):
    treePairs.foreach: (bi, mono) =>
      atNode(bi)(unifyColors8).perform()
      assertEquals(bi, mono)

object ManipTests:
  object tok1 extends Token
  object tok2 extends Token
