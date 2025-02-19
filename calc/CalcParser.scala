package distcompiler.calc

import cats.syntax.all.given

import distcompiler.*
import dsl.*
import distcompiler.Builtin.{Error, SourceMarker}
import distcompiler.calc.tokens.*

object CalcParser extends PassSeq:
  import distcompiler.dsl.*
  import Reader.*
  import CalcReader.*

  def inputWellformed: Wellformed = CalcReader.wellformed

  private val mulDivPass = passDef:
    wellformed := inputWellformed.makeDerived:
      Node.Top ::=! repeated(choice(Number, Expression, LowPrecOp))
    
    pass(once = true, strategy = pass.topDown)
      .rules:
        on(
          field(tok(Number, Expression))
          ~ field(tok(HighPrecOp))
          ~ field(tok(Number, Expression))
          ~ trailing
        ).rewrite: (left, operator, right) =>
          splice(
            Expression(
              left.unparent(),
              operator.unparent(),
              right.unparent()
            )
          )

  private val addSubPass = passDef:
    wellformed := prevWellformed.makeDerived:
      Node.Top ::=! repeated(choice(Number, Expression))

    pass(once = true, strategy = pass.topDown)
      .rules:
        on(
          field(tok(Number, Expression))
          ~ field(tok(LowPrecOp))
          ~ field(tok(Number, Expression))
          ~ trailing
        ).rewrite: (left, operator, right) =>
          splice(
            Expression(
              left.unparent(),
              operator.unparent(),
              right.unparent()
            )
          )

  private val simplifyPass = passDef:
    wellformed := prevWellformed.makeDerived:
      Node.Top ::=! fields(Number)

    def evaluateOperation(left: Node, op: Node, right: Node): Int =
      val leftNum = left.sourceRange.decodeString().toInt
      val rightNum = right.sourceRange.decodeString().toInt
      val operation = op.sourceRange.decodeString()
      
      operation match
        case "+" => leftNum + rightNum
        case "-" => leftNum - rightNum
        case "*" => leftNum * rightNum
        case "/" => leftNum / rightNum
        case _ => throw IllegalArgumentException("Unknown operator")

    pass(once = false, strategy = pass.topDown)
      .rules:
        on(
          tok(Expression) *> children(
            field(tok(Number))
            ~ field(tok(LowPrecOp, HighPrecOp))
            ~ field(tok(Number))
            ~ trailing
          )
        ).rewrite: (left, op, right) =>
          splice(
            Number(
              evaluateOperation(
                left.unparent(),
                op.unparent(),
                right.unparent()
              ).toString
            )
          )