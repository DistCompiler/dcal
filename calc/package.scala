package distcompiler.calc

import cats.syntax.all.given

import scala.collection.mutable

import distcompiler.*
import dsl.*
import distcompiler.calc.tokens.*

object tokens:
  object LowPrecOp extends Token:
    override def showSource: Boolean = true

  object HighPrecOp extends Token:
    override def showSource: Boolean = true

  object Expression extends Token:
    override def showSource: Boolean = true

  object Number extends Token:
    override def showSource: Boolean = true

val wellformed: Wellformed =
  Wellformed:
    Node.Top ::= repeated(choice(tokens.Number, tokens.Expression, tokens.LowPrecOp, tokens.HighPrecOp))
    
    tokens.Number ::= Atom
    tokens.LowPrecOp ::= Atom
    tokens.HighPrecOp ::= Atom

    tokens.Expression ::= fields(
      choice(tokens.Number, tokens.Expression),
      choice(tokens.LowPrecOp, tokens.HighPrecOp),
      choice(tokens.Number, tokens.Expression),
    )

object parse:
  def fromSourceRange(sourceRange: SourceRange): Node.Top =
    CalcReader(sourceRange)