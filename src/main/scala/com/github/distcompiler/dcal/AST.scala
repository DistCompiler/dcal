package com.github.distcompiler.dcal

object AST {
  /*
  module ::= `module` <name> <imports>? <definition>*

  imports ::= `import` <name> (`,` <name>)*

  definition ::=
    | `def` <name> `(` (<name> (`,` <name>)* )? `)` <block>

  block ::= `{` <statement>* `}

  statement ::=
    | `await` <expression>
    | <assign_pair> (`||` <assign_pair>)*
    | `let` <name> `=` <expression>
    | `var` <name> ((`=` | `\in`) <expression>)?

  // e.g `x := y || y := x` swaps x and y
  assign_pair ::= <name> `:=` <expression>

  expression ::= <expression_binop>

  expression_binop ::= <expression_base> (<binop> <expression_base>)?

  expression_base ::=
      | <int_literal>
      | <string_literal>
      | <name>
      | `(` <expression> `)`

  binop ::= TODO
  */

  final case class Module(name: String, imports: List[String], definitions: List[Definition])
  final case class Definition(name: String, params: List[String], body: Block)
  final case class Block(statements: List[Statement])

  enum Statement {
    case Await(expression: Expression)
    case AssignPairs(assignPairs: List[AssignPair])
    case Let(name: String, expression: Expression)
    case Var(name: String, opExpression: Option[(BinOp, Expression)])
  }

  final case class AssignPair(name: String, expression: Expression)

  enum Expression {
    case ExpressionBinOp(left: Expression, binOp: AST.BinOp, right: Expression)
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    // TODO: Could this case be just a String, instead of Name?
    case Name(name: String)
  }

  enum BinOp {
    case Equals
    case SlashIn
    case Placeholder
  }
}
