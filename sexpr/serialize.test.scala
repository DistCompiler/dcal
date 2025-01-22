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

import java.io.ByteArrayOutputStream

import distcompiler.*

class serializeTests extends munit.FunSuite:
  extension (writable: geny.Writable)
    def writeToString: String =
      val out = ByteArrayOutputStream()
      writable.writeBytesTo(out)
      out.toString()

  extension (top: Node.Top)
    def serializeCompact: String =
      serialize.toCompactWritable(top).writeToString
    def serializePretty: String =
      serialize.toPrettyWritable(top).writeToString

  val eg1 = Node.Top(tokens.List(tokens.Atom("foo"), tokens.Atom("bar")))

  test("eg1 compact"):
    assertEquals(eg1.serializeCompact, "(3:foo3:bar)")

  test("eg1 pretty"):
    assertEquals(
      eg1.serializePretty,
      """(foo
        |  bar)""".stripMargin
    )

  val eg2 = Node.Top(
    tokens.List(tokens.List(), tokens.List(), tokens.List(tokens.List()))
  )

  test("nested lists compact"):
    assertEquals(eg2.serializeCompact, "(()()(()))")

  test("nested lists pretty"):
    assertEquals(
      eg2.serializePretty,
      """(
        |  ()
        |  ()
        |  (()))""".stripMargin
    )
