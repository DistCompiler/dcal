package distcompiler.calc

import distcompiler.*
import Builtin.{Error, SourceMarker}

class CalcReaderTests extends munit.FunSuite:
  extension (str: String)
    def parse: Node.Top =
      calc.parse.fromSourceRange(SourceRange.entire(Source.fromString(str)))

    def evaluate: Node.Top =
      val top = parse

      CalcParser(top, tracer = Manip.RewriteDebugTracer(os.pwd / "dbg_calc_passes"))

      os.write.over(
        os.pwd / "dbg_calc_parser" / "test_output.dbg",
        top.toPrettyWritable(CalcReader.wellformed),
        createFolders = true
      )

      top

  test("empty string"):
    assertEquals("".parse, Node.Top())

  test("only whitespace"):
    assertEquals("    \n\t".parse, Node.Top())

  test("error: invalid character"):
    assertEquals("k".parse, Node.Top(Error("invalid byte", SourceMarker("k"))))

  test("single number"):
    assertEquals(
      "5".parse, 
      Node.Top(
        tokens.Number("5")
      )
    )

  test("simple addition"):
    assertEquals(
      "5 + 11".parse, 
      Node.Top(
        tokens.Number("5"),
        tokens.LowPrecOp("+"),
        tokens.Number("11")
      )
    )

  test("simple multiplication"):
    assertEquals(
      "5 * 11".parse, 
      Node.Top(
        tokens.Number("5"),
        tokens.HighPrecOp("*"),
        tokens.Number("11")
      )
    )

  test("addition calculation"):
    assertEquals(
      "5 + 11".evaluate,
      Node.Top(
        tokens.Number("16")
      ))

  test("multiplication calculation"):
    assertEquals(
      "5 * 11".evaluate,
      Node.Top(
        tokens.Number("55")
      ))

  test("full calculation"):
    assertEquals(
      "5 + 11 * 4".evaluate,
      Node.Top(
        tokens.Number("49")
      ))

  test("full calculation 2"):
    assertEquals(
      "5 * 4 + 4 / 2".evaluate,
      Node.Top(
        tokens.Number("22")
      ))

  test("full calculation 3"):
    assertEquals(
      "5 * 4 + 4 / 2 - 6".evaluate,
      Node.Top(
        tokens.Number("16")
      ))

  test("full calculation 4"):
    assertEquals(
      "5 * 4 + 4 / 2 - 6 * 2".evaluate,
      Node.Top(
        tokens.Number("10")
      ))

  // test("error: incorrect format"):
  //   intercept[IllegalArgumentException] (
  //     "10 - 10 7".evaluate
  //   )

  // test("error: divide by zero"):
  //   intercept[ArithmeticException] (
  //     "16 * 2 / 0".evaluate
  //   )