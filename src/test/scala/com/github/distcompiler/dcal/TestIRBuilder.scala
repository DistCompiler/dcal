package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class TestIRBuilder extends AnyFunSuite {
  import IRBuilder.*

  val moduleName = "TestModule"
  val testModule = s"module $moduleName"

  val testDefNoParamsNoBody = "def mt() {}"
  val expectedDefNoParamsNoBody = IR.Definition(
    name = "mt",
    params = List("_state1"),
    body = List(
      IR.Node.Name("_state1")
    )
  )
  
  val testStateAssignPairs = """def resetString() { str := "new string" }"""
  // Expected TLA+:
  //  resetString(_state1) ==
  //    LET
  //      _state2 == { [s EXCEPT !.str = "new string"]: s \in _state1 }
  //    IN
  //      _state2
  val expectedStateAssignPairs = IR.Definition(
    name = "resetString",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List( IR.Node.Name("_state1") ),
            setMember = "s",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.str = "),
              IR.Node.Uninterpreted(""""new string""""),
              IR.Node.Uninterpreted("]")
            )
          )
        ),
        body = List(
          IR.Node.Name("_state2")
        )
      )
    )
  )

  val testLongAssignPairs = "def baz() { y := y - 1 || x := x + 1 }"
  val expectedLongAssignPairs = IR.Definition(
    name = "baz",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "s",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(".y"),
              IR.Node.Uninterpreted(" - "),
              IR.Node.Uninterpreted("1"),
              IR.Node.Uninterpreted(", "),
              IR.Node.Uninterpreted("!.x = "),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(".x"),
              IR.Node.Uninterpreted(" + "),
              IR.Node.Uninterpreted("1"),
              IR.Node.Uninterpreted("]")
            )
          )
        ),
        body = List(IR.Node.Name("_state2"))
      )
    )
  )

  val testLet = "def sum(p1, p2) { let local = p1 + p2 x := local }"
  // Expected TLA+:
  //  sum(_state1, p1, p2) ==
  //    LET
  //      _state2 == UNION {
  //          LET
  //            local == p1 + p2
  //          IN
  //            { [ss EXCEPT !.x = local] : ss \in { s } }
  //          : s \in _state1
  //      }
  //    IN
  //      _state2
  val expectedLet = IR.Definition(
    name = "sum",
    params = List("_state1", "p1", "p2"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.Uninterpreted("UNION { "),
          IR.Node.MapOnSet(
            set = List( IR.Node.Name("_state1") ),
            setMember = "s",
            proc = List(
              IR.Node.Let(
                name = "local",
                binding = List(
                  IR.Node.Name("p1"),
                  IR.Node.Uninterpreted(" + "),
                  IR.Node.Name("p2")
                ),
                // { [ss EXCEPT !.x = local] : ss \in { s } }
                body = List(
                  IR.Node.MapOnSet(
                    set = List(
                      IR.Node.Uninterpreted("{ "),
                      IR.Node.Name("s"),
                      IR.Node.Uninterpreted(" }")
                    ),
                    setMember = "ss",
                    proc = List(
                      IR.Node.Uninterpreted("["),
                      IR.Node.Name("ss"),
                      IR.Node.Uninterpreted(" EXCEPT !.x = "),
                      IR.Node.Name("local"),
                      IR.Node.Uninterpreted("]")
                    )
                  )
                )
              )
            )
          ),
          IR.Node.Uninterpreted("}")
        ),
        body = List(
          IR.Node.Name("_state2")
        )
      )
    )
  )

  val testDefParam = "def bar(v) { y := y - v i := i + 1 }"
  // Expected TLA+:
  //  change(_state1, v) ==
  //    LET
  //      _state2 == { [s EXCEPT !.y = s.y - v]: s \ in _state1 }
  //    IN
  //      LET
  //        _state3 == { [s EXCEPT !.i = s.i + 1]: s \ in _state2 }
  //      IN
  //        _state3
  val expectedDefParam = IR.Definition(
    name = "bar",
    params = List("_state1", "v"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        // { [s EXCEPT !.y = s.y - v]: s \in _state1 }
        binding = List(
          IR.Node.MapOnSet(
            set = List( IR.Node.Name("_state1") ),
            setMember = "s",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(".y"),
              IR.Node.Uninterpreted(" - "),
              IR.Node.Name("v"),
              IR.Node.Uninterpreted("]")
            )
          )
        ),
        body = List(
          IR.Node.Let(
            name = "_state3",
            // { [s EXCEPT !.i = s.i + 1]: s \in _state2 }
            binding = List(
              IR.Node.MapOnSet(
                set = List( IR.Node.Name("_state2") ),
                setMember = "s",
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Name("s"),
                  IR.Node.Uninterpreted(" EXCEPT "),
                  IR.Node.Uninterpreted("!.i = "),
                  IR.Node.Name("s"),
                  IR.Node.Uninterpreted(".i"),
                  IR.Node.Uninterpreted(" + "),
                  IR.Node.Uninterpreted("1"),
                  IR.Node.Uninterpreted("]")
                )
              )
            ),
            body = List(
              IR.Node.Name("_state3")
            )
          )
        )
      )
    )
  )

  val testMultiLineDef = "def bar() { y := y - 1 x := x + 1 }"
  val expectedMultiLineDef = IR.Definition(
    name = "bar",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        // { [s EXCEPT !.y = s.y - 1]: s \in _state1 }
        binding = List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "s",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(".y"),
              IR.Node.Uninterpreted(" - "),
              IR.Node.Uninterpreted("1"),
              IR.Node.Uninterpreted("]")
            )
          )
        ),
        body = List(
          IR.Node.Let(
            name = "_state3",
            // { [s EXCEPT !.x = s.x + 1]: s \in _state2 }
            binding = List(
              IR.Node.MapOnSet(
                set = List(IR.Node.Name("_state2")),
                setMember = "s",
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Name("s"),
                  IR.Node.Uninterpreted(" EXCEPT "),
                  IR.Node.Uninterpreted("!.x = "),
                  IR.Node.Name("s"),
                  IR.Node.Uninterpreted(".x"),
                  IR.Node.Uninterpreted(" + "),
                  IR.Node.Uninterpreted("1"),
                  IR.Node.Uninterpreted("]")
                )
              )
            ),
            body = List(
              IR.Node.Name("_state3")
            )
          )
        )
      )
    )
  )

  val testVar = "def makeVar() { var v = x }"
  val expectedVar = IR.Definition(
    name = "makeVar",
    params = List("_state1"),
    body = Nil // Stub
  )

  val testIfThenElse = "def branch() { if x <= y then { x := x + 1 } else { y := y - 1 } }"
  //  branch(_state1) ==
  //    LET
  //      _state2 == { IF s.x <= s.y
  //                   THEN [s EXCEPT !.x = s.x + 1]
  //                   ELSE [s EXCEPT !.y = s.y - 1]: s \in _state1 }
  //    IN
  //      _state2
  val expectedIfThenElse = IR.Definition(
    name = "branch",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "s",
            proc = List(
              // IF s.x <= s.y
              IR.Node.Uninterpreted("IF "),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(".x"),
              IR.Node.Uninterpreted(" <= "),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(".y"),
              // TODO: Possibly add a whitespace or newline here, between IF ... THEN ... ELSE?
              // THEN [s EXCEPT !.x = s.x + 1]
              IR.Node.Uninterpreted("THEN "),
              IR.Node.Uninterpreted("["),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.x = "),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(".x"),
              IR.Node.Uninterpreted(" + "),
              IR.Node.Uninterpreted("1"),
              // ELSE [s EXCEPT !.y = s.y - 1]
              IR.Node.Uninterpreted("ELSE "),
              IR.Node.Uninterpreted("["),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(".y"),
              IR.Node.Uninterpreted(" - "),
              IR.Node.Uninterpreted("1")
            )
          )
        ),
        body = List(IR.Node.Name("_state2"))
      )
    )
  )

  val testAwait = "def wait() { await x < 0 }"
  val expectedAwait = IR.Definition(
    name = "wait",
    params = List("_state1"),
    body = Nil // Stub
  )

  List(
    TestUtils.sequenceLines(testModule, testLet) -> IR.Module(
      name = moduleName,
      definitions = List(expectedLet)
    ),
    TestUtils.sequenceLines(testModule, testDefParam) -> IR.Module(
      name = moduleName, definitions = List(expectedDefParam)
    ),
    TestUtils.sequenceLines(
      testModule, testStateAssignPairs, testDefParam, testDefNoParamsNoBody
    ) -> IR.Module(
      name = moduleName,
      definitions = List(expectedStateAssignPairs, expectedDefParam, expectedDefNoParamsNoBody)
    )
  ).foreach {
    case (input, expectedOutput) =>
      ignore(s"generateIR($input)") {
        val actualOutput = IRBuilder(
          contents = input,
          fileName = "<testfile>",
        )
        assert(actualOutput == expectedOutput)
      }
  }

  List(
    testModule -> IR.Module(
      name = moduleName, definitions = Nil
    ),
    TestUtils.sequenceLines(testModule, testDefNoParamsNoBody) -> IR.Module(
      name = moduleName,
      definitions = List(
        expectedDefNoParamsNoBody
      )
    ),
    TestUtils.sequenceLines(testModule, testStateAssignPairs) -> IR.Module(
      name = moduleName, definitions = List(expectedStateAssignPairs)
    ),
    TestUtils.sequenceLines(testModule, testLongAssignPairs) -> IR.Module(
      name = moduleName, definitions = List(expectedLongAssignPairs)
    ),
    TestUtils.sequenceLines(testModule, testIfThenElse) -> IR.Module(
      name = moduleName, definitions = List(expectedIfThenElse)
    ),
    TestUtils.sequenceLines(testModule, testMultiLineDef) -> IR.Module(
      name = moduleName, definitions = List(expectedMultiLineDef)
    )
  ).foreach {
    case (input, expectedOutput) =>
      test(s"generateIR($input)") {
        val actualOutput = IRBuilder(
          contents = input,
          fileName = "<testfile>",
        )
        assert(actualOutput == expectedOutput)
      }
  }
}
