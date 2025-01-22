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

import distcompiler.*
import Builtin.{Error, SourceMarker}

class SExprReaderTests extends munit.FunSuite:
  import tokens.Atom
  extension (str: String)
    def parse: Node.Top =
      sexpr.parse.fromSourceRange(SourceRange.entire(Source.fromString(str)))

  test("foo string literal"):
    assertEquals("\"foo\"".parse, Node.Top(Atom("foo")))

  test("escape tab char"):
    assertEquals("\"\\t\"".parse, Node.Top(Atom("\t")))

  test("ignore crlf"):
    assertEquals("\"\\\r\n\"".parse, Node.Top(Atom("")))

  test("error: invalid string escape"):
    assertEquals(
      "\"\\^\"".parse,
      Node.Top(
        Error("invalid escape sequence \\^", Builtin.SourceMarker("\\^")),
        Atom("?")
      )
    )

  test("empty string"):
    assertEquals("".parse, Node.Top())
  test("one space"):
    assertEquals(" ".parse, Node.Top())
  test("all misc whitespace"):
    assertEquals(" \n\n \t\r\n".parse, Node.Top())
  test("one atom"):
    assertEquals("8:deadbeef".parse, Node.Top(Atom("deadbeef")))
  test("two atoms"):
    assertEquals("3:foo 3:bar".parse, Node.Top(Atom("foo"), Atom("bar")))
  test("atom with weird chars"):
    assertEquals("9:(foo bar)".parse, Node.Top(Atom("(foo bar)")))

  test("empty atom"):
    assertEquals("0:".parse, Node.Top(Atom("")))
  test("empty list"):
    assertEquals("()".parse, Node.Top(tokens.List()))

  test("list of two atoms"):
    assertEquals(
      "(3:foo 3:bar)".parse,
      Node.Top(tokens.List(Atom("foo"), Atom("bar")))
    )
  test("list of three lists"):
    assertEquals(
      "((3:foo) (3:bar) ())".parse,
      Node.Top(
        tokens.List(
          tokens.List(Atom("foo")),
          tokens.List(Atom("bar")),
          tokens.List()
        )
      )
    )

  test("error: stray list close"):
    assertEquals(
      ")".parse,
      Node.Top(Error("unexpected end of list", SourceMarker(")")))
    )

  test("error: stray list close, recovery"):
    assertEquals(
      ") 3:foo".parse,
      Node.Top(
        Error("unexpected end of list", SourceMarker(")")),
        Atom("foo")
      )
    )

  test("error: missing list terminator"):
    assertEquals(
      "(4: foo".parse,
      Node.Top(
        tokens.List(Atom(" foo"), Error("unexpected EOF", SourceMarker()))
      )
    )

  test("empty string literal"):
    assertEquals("\"\"".parse, Node.Top(tokens.Atom("")))

  test("3 empty string literals with a list"):
    assertEquals(
      "\"\" (\"\" ) \"\"".parse,
      Node.Top(
        tokens.Atom(""),
        tokens.List(tokens.Atom("")),
        tokens.Atom("")
      )
    )

  test("list of 3 string literals"):
    assertEquals(
      raw"""("foo" "bar" " ")""".parse,
      Node.Top(
        tokens.List(
          tokens.Atom("foo"),
          tokens.Atom("bar"),
          tokens.Atom(" ")
        )
      )
    )

  test("string with escapes"):
    assertEquals(
      raw""" "\tfoo\\bar" """.parse,
      Node.Top(tokens.Atom("\tfoo\\bar"))
    )

  test("misc token atoms"):
    assertEquals(
      "foo bar =ping= :this /42a".parse,
      Node.Top(
        Atom("foo"),
        Atom("bar"),
        Atom("=ping="),
        Atom(":this"),
        Atom("/42a")
      )
    )
