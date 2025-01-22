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

import os.read.bytes

class SourceRangeTests extends munit.FunSuite:
  val ipsum =
    """Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
      |incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
      |exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure
      |dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
      |Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit
      |anim id est laborum.""".stripMargin

  extension (src: SourceRange)
    def takeLine: SourceRange =
      src.take(src.indexOf('\n'))
    def dropLine: SourceRange =
      src.drop(src.indexOf('\n') + 1)
    def lastLine: SourceRange =
      src.drop(src.lastIndexOf('\n') + 1)
    def startingFrom(str: String): SourceRange =
      val bytes = str.getBytes()
      src.drop(src.indexOfSlice(bytes))
    def upTo(str: String): SourceRange =
      val bytes = str.getBytes()
      src.take(src.indexOfSlice(bytes) + bytes.size)

  val ipsumSrc = SourceRange.entire(Source.fromString(ipsum))

  test("highlight entire first line"):
    assertEquals(
      ipsumSrc.takeLine.showInSource,
      """Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
        |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^""".stripMargin
    )

  test("highlight entire second line"):
    assertEquals(
      ipsumSrc.dropLine.takeLine.showInSource,
      """incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
        |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^""".stripMargin
    )

  test("highlight last line"):
    assertEquals(
      ipsumSrc.lastLine.showInSource,
      """anim id est laborum.
        |^^^^^^^^^^^^^^^^^^^^""".stripMargin
    )

  test("highlight second 2 lines"):
    assertEquals(
      (ipsumSrc.dropLine.takeLine <+> ipsumSrc.dropLine.dropLine.takeLine).showInSource,
      """vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        |incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
        |exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure
        |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^""".stripMargin
    )

  test("highlight second 3 lines"):
    assertEquals(
      (ipsumSrc.dropLine.takeLine <+> ipsumSrc.dropLine.dropLine.dropLine.takeLine).showInSource,
      """vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        |incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
        |...
        |dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
        |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^""".stripMargin
    )

  test("highlight all but 1st line"):
    assertEquals(
      ipsumSrc.dropLine.showInSource,
      """vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        |incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
        |...
        |anim id est laborum.
        |^^^^^^^^^^^^^^^^^^^^""".stripMargin
    )

  test("highlight amet to nisi"):
    assertEquals(
      ipsumSrc.startingFrom("amet").upTo("nisi").showInSource,
      """                      vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        |Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
        |...
        |exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure
        |^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^""".stripMargin
    )

  test("highlight just nisi"):
    assertEquals(
      ipsumSrc.startingFrom("nisi").take("nisi".size).showInSource,
      """exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure
        |                             ^^^^""".stripMargin
    )
