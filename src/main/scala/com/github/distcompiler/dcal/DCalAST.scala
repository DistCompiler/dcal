package com.github.distcompiler.dcal

object DCalAST {
  /*
  module ::= `module` <name> <imports>? <definition>*

  imports ::= `import` <name> (`,` <name>)*

  definition ::=
    | `def` <name> `(` (<name> (`,` <name>)* )? `)` <block>

  block ::= `{` <statement>* `}

  statement ::=
    | `await` <predicate>
    | <assign_pair> (`||` <assign_pair>)*
    | `let` <name> (`=` | `\in`) (<expression> | <call>)
    | `var` <name> ((`=` | `\in`) <expression>)?
    | `if` <expression> `then` <block> `else` <block>
    | <call>

  // e.g `x := y || y := x` swaps x and y
  assign_pair ::= <name> `:=` <expression>

  call ::= <name>(`.`<name>)? `(` (<expression> (`,` <expression>))? `)`

  expression ::= <expression_unop> | <expression_logicop> | <expression_relop> | <expression_binop>

  expression_binop ::= <expression_base> (<binop> <expression_base>)?

  expression_logicop ::= <expression_base> <logicop> <expression_base>

  expression_relop ::= <expression_base> <relop> <expression_base>

  expression_unop ::= <unop> <expression_base>

  expression_base ::=
      | <int_literal>
      | <string_literal>
      | <name>
      | <set>
      | `(` <expression> `)`

  set ::= `{` <expression>* `}`


  binop ::= `+` | `-`

  relop ::= `=` | `#` | `<` | `>` | `<=` | `>=`

  logicop ::= `\/` | `/\`

  unop ::= ~
  */

  final case class Module(name: String, imports: List[String], definitions: List[Definition])
  final case class Definition(name: String, params: List[String], body: Block)
  final case class Block(statements: List[Statement])

  enum Statement {
    case Await(expression: Expression)
    case AssignPairs(assignPairs: List[AssignPair])
    case Let(name: String, assignmentOp: AssignmentOp, binding: Either[aCall, Expression])
    case Var(name: String, expressionOpt: Option[(AssignmentOp, Expression)])
    case IfThenElse(predicate: Expression, thenBlock: Block, elseBlock: Block)
    case Call(call: aCall)
  }

  final case class AssignPair(name: String, expression: Expression)

  final case class aCall(moduleNameOpt: Option[String], definitionName: String, args: List[Expression])

  enum Expression {
    case True
    case False
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case Name(name: String)
    case Set(members: List[Expression])
    case ExpressionBinOp(lhs: Expression, binOp: BinOp, rhs: Expression)
    case ExpressionRelOp(lhs: Expression, relOp: RelOp, rhs: Expression)
    case ExpressionLogicOp(lhs: Expression, logicOp: LogicOp, rhs: Expression)
    case ExpressionUnOp(unop: UnOp, expr: Expression)
    case BracketedExpression(expression: Expression)
  }

  enum AssignmentOp {
    case EqualTo
    case SlashIn
  }

  enum BinOp {
    case Plus
    case Minus
  }

  enum RelOp {
    case EqualTo
    case NotEqualTo
    case LesserThan
    case GreaterThan
    case LesserThanOrEqualTo
    case GreaterThanOrEqualTo
  }

  enum LogicOp {
    case And
    case Or
    case Not
  }

  enum UnOp {
    case Not
  }
}
