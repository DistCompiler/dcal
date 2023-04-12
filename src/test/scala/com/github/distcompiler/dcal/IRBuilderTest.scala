package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.List

class IRBuilderTest extends AnyFunSuite {
  import IRBuilder.*

  val moduleName = "TestModule"
  val testModule = s"module $moduleName"

  val testDefNoParamNoBody = "def mt() {}"
  val expectedDefNoParamsNoBody = IR.Definition(
    name = "mt",
    params = List("_state1"),
    body = List(
      IR.Node.Name("_state1")
    )
  )

  val testParams = "def bar(v) { y := y - v; i := i + 1; }"
  val expectedParams = IR.Definition(
    name = "bar",
    params = List("_state1", "v"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "l1",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("l1"),
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
            binding = List(
              IR.Node.MapOnSet(
                set = List(IR.Node.Name("_state2")),
                setMember = "l1",
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(" EXCEPT "),
                  IR.Node.Uninterpreted("!.i = "),
                  IR.Node.Name("l1"),
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

  val testAwait = "def wait() { await x > 4; }"
  val expectedAwait = IR.Definition(
    name = "wait",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.FilterOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "l1",
            pred = List(
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(".x"),
              IR.Node.Uninterpreted(" > "),
              IR.Node.Uninterpreted("4")
            )
          )
        ),
        body = List(IR.Node.Name("_state2"))
      )
    )
  )

  val testStateAssignPairs = """def resetString() { str := "new string"; }"""
  val expectedStateAssignPairs = IR.Definition(
    name = "resetString",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List( IR.Node.Name("_state1") ),
            setMember = "l1",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("l1"),
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

  val testLongAssignPairs = "def baz() { y := y - 1 || x := x + 1; }"
  val expectedLongAssignPairs = IR.Definition(
    name = "baz",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "l1",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(".y"),
              IR.Node.Uninterpreted(" - "),
              IR.Node.Uninterpreted("1"),
              IR.Node.Uninterpreted(", "),
              IR.Node.Uninterpreted("!.x = "),
              IR.Node.Name("l1"),
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

  val testLetEqualTo = "def sum(p1, p2) { let local = p1 + p2; x := local; }"
  val expectedLetEqualTo = IR.Definition(
    name = "sum",
    params = List("_state1", "p1", "p2"),
    body = List(
      IR.Node.Uninterpreted("UNION"),
      IR.Node.MapOnSet(
        set = List(IR.Node.Name("_state1")),
        setMember = "l1",
        proc = List(
          IR.Node.Let(
            name = "local",
            binding = List(
              IR.Node.Name("p1"),
              IR.Node.Uninterpreted(" + "),
              IR.Node.Name("p2")
            ),
            body = List(
              IR.Node.Let(
                name = "_state2",
                binding = List(
                  IR.Node.Uninterpreted("{ "),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(" }")
                ),
                body = List(
                  IR.Node.Let(
                    name = "_state3",
                    binding = List(
                      IR.Node.MapOnSet(
                        set = List(IR.Node.Name("_state2")),
                        setMember = "l2",
                        proc = List(
                          IR.Node.Uninterpreted("["),
                          IR.Node.Name("l2"),
                          IR.Node.Uninterpreted(" EXCEPT "),
                          IR.Node.Uninterpreted("!.x = "),
                          IR.Node.Name("local"),
                          IR.Node.Uninterpreted("]")
                        )
                      )
                    ),
                    body = List(IR.Node.Name("_state3"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  val testLetIn = """def testLetIn() { let z \in set; x := x + z; await x > 10; }"""
  val expectedLetIn = IR.Definition(
    name = "testLetIn",
    params = List("_state1"),
    body = List(
      IR.Node.Uninterpreted("UNION "),
      IR.Node.MapOnSet(
        set = List(IR.Node.Name("_state1")),
        setMember = "l1",
        proc = List(
          IR.Node.Uninterpreted("UNION "),
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("l1"), IR.Node.Uninterpreted(".set")),
            setMember = "z",
            proc = List(
              IR.Node.Let(
                name = "_state2",
                binding = List(
                  IR.Node.Uninterpreted("{ "),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(" }")
                ),
                body = List(
                  IR.Node.Let(
                    name = "_state3",
                    binding = List(
                      IR.Node.MapOnSet(
                        set = List(IR.Node.Name("_state2")),
                        setMember = "l2",
                        proc = List(
                          IR.Node.Uninterpreted("["),
                          IR.Node.Name("l2"),
                          IR.Node.Uninterpreted(" EXCEPT "),
                          IR.Node.Uninterpreted("!.x = "),
                          IR.Node.Name("l2"),
                          IR.Node.Uninterpreted(".x"),
                          IR.Node.Uninterpreted(" + "),
                          IR.Node.Name("z"),
                          IR.Node.Uninterpreted("]")
                        )
                      )
                    ),
                    body = List(
                      IR.Node.Let(
                        name = "_state4",
                        binding = List(
                          IR.Node.FilterOnSet(
                            set = List(IR.Node.Name("_state3")),
                            setMember = "l2",
                            pred = List(
                              IR.Node.Name("l2"),
                              IR.Node.Uninterpreted(".x"),
                              IR.Node.Uninterpreted(" > "),
                              IR.Node.Uninterpreted("10")
                            )
                          )
                        ),
                        body = List(IR.Node.Name("_state4"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  val testVarEqualTo = "def testVar() { var z = 10; x := x + z; }"
  val expectedVarEqualTo = IR.Definition(
    name = "testVar",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "l1",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Uninterpreted("l2 \\in DOMAIN "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(""" \cup { "z" } |-> IF l2 = "z" THEN """),
              IR.Node.Uninterpreted("10"),
              IR.Node.Uninterpreted(" ELSE "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted("[l2]]")
            )
          )
        ),
        body = List(
          IR.Node.Let(
            name = "_state3",
            binding = List(
              IR.Node.MapOnSet(
                set = List(IR.Node.Name("_state2")),
                setMember = "l1",
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(" EXCEPT "),
                  IR.Node.Uninterpreted("!.x = "),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(".x"),
                  IR.Node.Uninterpreted(" + "),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(".z"),
                  IR.Node.Uninterpreted("]"),
                )
              )
            ),
            body = List(IR.Node.Name("_state3"))
          )
        )
      )
    )
  )

  val testVarIn = """def testVarIn() { var z \in { 1, 2, 3, 4, 5 }; }"""
  val expectedVarIn = IR.Definition(
    name = "testVarIn",
    params = List("_state1"),
    body = List(
      IR.Node.Uninterpreted("UNION "),
      IR.Node.MapOnSet(
        set = List(IR.Node.Name("_state1")),
        setMember = "l2",
        proc = List(
          IR.Node.Uninterpreted("UNION "),
          IR.Node.MapOnSet(
            set = List(
              IR.Node.Uninterpreted("{ "),
              IR.Node.Uninterpreted("1"),
              IR.Node.Uninterpreted(", "),
              IR.Node.Uninterpreted("2"),
              IR.Node.Uninterpreted(", "),
              IR.Node.Uninterpreted("3"),
              IR.Node.Uninterpreted(", "),
              IR.Node.Uninterpreted("4"),
              IR.Node.Uninterpreted(", "),
              IR.Node.Uninterpreted("5"),
              IR.Node.Uninterpreted(" }")
            ),
            setMember = "l1",
            proc = List(
              IR.Node.Let(
                name = "_state2",
                binding = List(
                  IR.Node.Uninterpreted("{ "),
                  IR.Node.Name("l2"),
                  IR.Node.Uninterpreted(" }")
                ),
                body = List(
                  IR.Node.Let(
                    name = "_state3",
                    binding = List(
                      IR.Node.MapOnSet(
                        set = List(IR.Node.Name("_state2")),
                        setMember = "l3",
                        proc = List(
                          IR.Node.Uninterpreted("["),
                          IR.Node.Uninterpreted("l4 \\in DOMAIN "),
                          IR.Node.Name("l3"),
                          IR.Node.Uninterpreted(""" \cup { "z" } |-> IF l4 = "z" THEN """),
                          IR.Node.Name("l1"),
                          IR.Node.Uninterpreted(" ELSE "),
                          IR.Node.Name("l3"),
                          IR.Node.Uninterpreted("[l4]]")
                        )
                      )
                    ),
                    body = List(IR.Node.Name("_state3"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  val testIfThenElse = "def branch() { if x <= y then { x := x + 1; } else { y := y - 1; } i := x + y; }"
  val expectedIfThenElse = IR.Definition(
    name = "branch",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.Uninterpreted("UNION "),
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "l1",
            proc = List(
              IR.Node.Uninterpreted("IF "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(".x"),
              IR.Node.Uninterpreted(" <= "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(".y"),
              IR.Node.Uninterpreted("\nTHEN "),
              IR.Node.Let(
                name = "_state2",
                binding = List(
                  IR.Node.Uninterpreted("{ "),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(" }")
                ),
                body = List(
                  IR.Node.Let(
                    name = "_state3",
                    binding = List(
                      IR.Node.MapOnSet(
                        set = List(IR.Node.Name("_state2")),
                        setMember = "l2",
                        proc = List(
                          IR.Node.Uninterpreted("["),
                          IR.Node.Name("l2"),
                          IR.Node.Uninterpreted(" EXCEPT "),
                          IR.Node.Uninterpreted("!.x = "),
                          IR.Node.Name("l2"),
                          IR.Node.Uninterpreted(".x"),
                          IR.Node.Uninterpreted(" + "),
                          IR.Node.Uninterpreted("1"),
                          IR.Node.Uninterpreted("]")
                        )
                      )
                    ),
                    body = List(IR.Node.Name("_state3"))
                  )
                )
              ),
              IR.Node.Uninterpreted("\nELSE "),
              IR.Node.Let(
                name = "_state2",
                binding = List(
                  IR.Node.Uninterpreted("{ "),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(" }")
                ),
                body = List(
                  IR.Node.Let(
                    name = "_state3",
                    binding = List(
                      IR.Node.MapOnSet(
                        set = List(IR.Node.Name("_state2")),
                        setMember = "l2",
                        proc = List(
                          IR.Node.Uninterpreted("["),
                          IR.Node.Name("l2"),
                          IR.Node.Uninterpreted(" EXCEPT "),
                          IR.Node.Uninterpreted("!.y = "),
                          IR.Node.Name("l2"),
                          IR.Node.Uninterpreted(".y"),
                          IR.Node.Uninterpreted(" - "),
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
        ),
        body = List(
          IR.Node.Let(
            name = "_state3",
            binding = List(
              IR.Node.MapOnSet(
                set = List(IR.Node.Name("_state2")),
                setMember = "l1",
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(" EXCEPT "),
                  IR.Node.Uninterpreted("!.i = "),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(".x"),
                  IR.Node.Uninterpreted(" + "),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(".y"),
                  IR.Node.Uninterpreted("]")
                )
              )
            ),
            body = List(IR.Node.Name("_state3"))
          )
        )
      )
    )
  )

  val testMultiLineDef = "def bar() { y := y - 1; x := x + 1; }"
  val expectedMultiLineDef = IR.Definition(
    name = "bar",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "l1",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("l1"),
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
            binding = List(
              IR.Node.MapOnSet(
                set = List(IR.Node.Name("_state2")),
                setMember = "l1",
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(" EXCEPT "),
                  IR.Node.Uninterpreted("!.x = "),
                  IR.Node.Name("l1"),
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

  List(
    // Place failing tests here
  ).foreach {
    case (input, expectedOutput) =>
      ignore(s"buildIR($input)") {
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
    TestUtils.sequenceLines(testModule, testDefNoParamNoBody) -> IR.Module(
      name = moduleName,
      definitions = List(
        expectedDefNoParamsNoBody
      )
    ),
    TestUtils.sequenceLines(testModule, testParams) -> IR.Module(
      name = moduleName, definitions = List(expectedParams)
    ),
    TestUtils.sequenceLines(testModule, testAwait) -> IR.Module(
      name = moduleName, definitions = List(expectedAwait)
    ),
    TestUtils.sequenceLines(testModule, testStateAssignPairs) -> IR.Module(
      name = moduleName, definitions = List(expectedStateAssignPairs)
    ),
    TestUtils.sequenceLines(testModule, testLongAssignPairs) -> IR.Module(
      name = moduleName, definitions = List(expectedLongAssignPairs)
    ),
    TestUtils.sequenceLines(testModule, testLetEqualTo) -> IR.Module(
      name = moduleName,
      definitions = List(expectedLetEqualTo)
    ),
    TestUtils.sequenceLines(testModule, testLetIn) -> IR.Module(
      name = moduleName,
      definitions = List(expectedLetIn)
    ),
    TestUtils.sequenceLines(testModule, testVarEqualTo) -> IR.Module(
      name = moduleName, definitions = List(expectedVarEqualTo)
    ),
    TestUtils.sequenceLines(testModule, testVarIn) -> IR.Module(
      name = moduleName, definitions = List(expectedVarIn)
    ),
    TestUtils.sequenceLines(testModule, testIfThenElse) -> IR.Module(
      name = moduleName, definitions = List(expectedIfThenElse)
    ),
    TestUtils.sequenceLines(testModule, testMultiLineDef) -> IR.Module(
      name = moduleName, definitions = List(expectedMultiLineDef)
    ),
    TestUtils.sequenceLines(testModule, "def f1() {}", "def f2() {}", "def f3() {}") -> IR.Module(
      name = moduleName,
      definitions = List(
        IR.Definition(name = "f1", params = List("_state1"), body = List(IR.Node.Name("_state1"))),
        IR.Definition(name = "f2", params = List("_state1"), body = List(IR.Node.Name("_state1"))),
        IR.Definition(name = "f3", params = List("_state1"), body = List(IR.Node.Name("_state1")))
      )
    ),
    TestUtils.sequenceLines(
      testModule, testStateAssignPairs, testParams, testDefNoParamNoBody
    ) -> IR.Module(
      name = moduleName,
      definitions = List(expectedStateAssignPairs, expectedParams, expectedDefNoParamsNoBody)
    )
  ).foreach {
    case (input, expectedOutput) =>
      test(s"buildIR($input)") {
        val actualOutput = IRBuilder(
          contents = input,
          fileName = "<testfile>",
        )
        assert(actualOutput == expectedOutput)
      }
  }

  test("Tests that params of one def does not leak to another def") {
    val input = TestUtils.sequenceLines(testModule, "def f1(v) {}", "def f2() { x := x + v; }")
    assertThrows[NoSuchElementException](
      IRBuilder(
        contents = input, fileName = "<testfile>"
      )
    )
  }
}
