package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.Expression.{BracketedExpression, ExpressionBinOp, False, IntLiteral, Name, StringLiteral, True}
import com.github.distcompiler.dcal.DCalAST.Statement.{AssignPairs, Await, IfThenElse, Let, Var}
import com.github.distcompiler.dcal.DCalParser.*

import scala.collection.mutable.ListBuffer

/**
 * Compiles DCal concrete syntax to an IR that resembles TLA+. DCal statements that compile to TLA+ identifiers, let
 * expressions, and set maps are preserved by their respective IR Node structures. All other DCal statements are
 * uninterpreted by this IR pass.
 *
 * Each DCal statement compiles to a TLA+ nested let expression, not another TLA+ statement in a one-to-one mapping
 * manner.
 */
object IRBuilder {
  enum NameInfo {
    case Local
    case State
  }

  // Context tracks the current state that exists in the program and the mapping of names to name infos, where name
  // is the string literal referring to the name and name info categorises the name itself (either a local or a state
  // member variable)
  // SetMember is set some non-null string value when we enter the scope of a MapOnSet's proc
  final case class Context(state: Int,
                           nameInfoOf: Map[String,NameInfo],
                           setMember: String = null) {

    def withState(state: Int): Context =
      copy(state = state)

    def withNameInfo(name: String, nameInfo: NameInfo): Context =
      copy(nameInfoOf = nameInfoOf.updated(name, nameInfo))

    def withSetMember(setMember: String): Context =
      copy(setMember = setMember)

    val stateName: String = {
      s"_state$state"
    }
  }

  def generateBinOp(dcalBinOp: DCalAST.BinOp): IR.Node = {
    dcalBinOp match {
      case DCalAST.BinOp.Plus => IR.Node.Uninterpreted(" + ")
      case DCalAST.BinOp.Minus => IR.Node.Uninterpreted(" - ")
      case _ => ???
    }
  }

  def generateExpression(dcalExpr: DCalAST.Expression)(using ctx: Context): List[IR.Node] = {
    dcalExpr match {
      case ExpressionBinOp(lhs, binOp, rhs) =>
        generateExpression(lhs) ++ List(generateBinOp(binOp)) ++ generateExpression(rhs)

      case True => List(IR.Node.Uninterpreted("TRUE"))

      case False => List(IR.Node.Uninterpreted("FALSE"))

      case IntLiteral(value) => List(IR.Node.Uninterpreted(value.toString))

      case StringLiteral(value) => List(IR.Node.Uninterpreted(s""""$value""""))

      case Name(name) => ctx.nameInfoOf(name) match {
        case NameInfo.State => List(IR.Node.Name(ctx.setMember), IR.Node.Uninterpreted(s".$name"))
        case _ => ???
      }

      case BracketedExpression(expr) =>
        List(IR.Node.Uninterpreted("(")) ++ generateExpression(expr) ++ List(IR.Node.Uninterpreted(")"))
    }
  }

  /**
   * Maps the DCal statement to the current state set, producing a new state set.
   * Examples:
   * str is a state, _state1 is the current state
   *    str := "new string" -> { [s EXCEPT !.str = "new string"]: s \in _state1 }
   * y & i are states, v is a local, _state1 is the current state
   *    y := y - v || i := i + 1 -> { [s EXCEPT !.y = s.y - v, !.i = s.i + 1 ]: s \in _state1 }
   */
  def generateAssignPairs(dcalAssignPairs: List[DCalAST.AssignPair])
                         (using ctx: Context): List[IR.Node] = {
    // 1: Creates a new local to use as set member
    // 2: Processes assign pairs
    //    For each assign pair <name> := <expression>
    //      - If <name> is a state, creates Uninterpreted(s"!.$name = "), else TODO
    //      - Calls generateExpression(expression): y - v -> s.y - v, i + i -> s.i + 1
    //      - Prepends Uninterpreted(s"!.$name = ") to generateExpression(expression) and returns
    // 3: Add a Uninterpreted(", ") between each assign pairs then inserts the result into s"[$setMember EXCEPT]
    // <processed assign pairs>]"

    /**
     * Examples:
     * str := "new string"  -> !.str = "new string"
     * y := y - v           -> !.y = s.y - v
     * i := i + 1           -> !.i = s.i + 1
     */
    // TODO: This assume that name in dcalAssignPair is always a state. The behaviour for a local name is undefined.
    def generateAssignPair(dcalAssignPair: DCalAST.AssignPair)(using ctx: Context): ListBuffer[IR.Node] =
      ctx.nameInfoOf(dcalAssignPair.name) match {
        case NameInfo.Local => ???
        case NameInfo.State =>
          // How to do the transformation y - v -> s.y - v? Adding "s" to the context mapping is not sufficient for y
          // to be transformed to "s.y".
          ListBuffer[IR.Node](IR.Node.Uninterpreted(s"!.${dcalAssignPair.name} = ")) ++= generateExpression(dcalAssignPair.expression)(using ctx).toBuffer
      }

    def generateDelimitedAssignPairs(aps: List[DCalAST.AssignPair])(using ctx: Context) = {
      def delimitHelper(lst: List[DCalAST.AssignPair], acc: ListBuffer[IR.Node]): ListBuffer[IR.Node] = {
        lst match {
          case Nil => acc
          case h::t => t match {
            case Nil => delimitHelper(t, acc :++ generateAssignPair(h)(using ctx))
            case _ => delimitHelper(t, acc :++ (generateAssignPair(h)(using ctx) += IR.Node.Uninterpreted(", ")))
          }
        }
      }
      delimitHelper(aps, ListBuffer[IR.Node]())
    }

    def generateProc(using ctx: Context): List[IR.Node] = {
      val pb = ListBuffer[IR.Node](
        IR.Node.Uninterpreted("["),
        IR.Node.Name(ctx.setMember),
        IR.Node.Uninterpreted(" EXCEPT "),
      )

      pb.appendAll(generateDelimitedAssignPairs(dcalAssignPairs)(using ctx))
      pb.append(IR.Node.Uninterpreted("]"))
      pb.toList
    }

    val setMember = "s" // TODO: Use a fresh utility?

    // If we are generating a MapOnSet, the proc is operating on setMember so setMember should be passed into context
    List(IR.Node.MapOnSet(
      set = List(IR.Node.Name(ctx.stateName)),
      setMember = setMember,
      proc = generateProc(using ctx.withSetMember(setMember).withNameInfo(setMember, NameInfo.Local))
    ))
  }

  /**
   * Maps the DCal statement to the current state set, producing a new state set
   */
  def generateStatement(dcalStmt: DCalAST.Statement)
                       (using ctx: Context): List[IR.Node] = {
    dcalStmt match {
      case Await(expr) => List(IR.Node.Uninterpreted("await")) ++ generateExpression(expr)
      case AssignPairs(assignPairs) => generateAssignPairs(assignPairs)
      case Let(name: String, expression: DCalAST.Expression) => ???
      case Var(name: String, optExpression: Option[(DCalAST.BinOp, DCalAST.Expression)]) => ???
      case IfThenElse(predicate: DCalAST.Expression, thenBlock: DCalAST.Block, elseBlock: DCalAST.Block) => ???
    }
  }

  /**
   * Recursively generates statements.
   * In the base case, when there is no DCal statement left to generate, produces the current state.
   * In the recursive case, when there is at least a DCal statement to generate, produces a LET expression whose:
   * - name is a new state,
   * - binding is a set of states produced by mapping the DCal statement to the current state,
   * - body is an expression produced by the generating the remaining DCal statements.
   *
   * Invariants: TODO
   */
  def generateStatements(dcalStmts: List[DCalAST.Statement])
                        (using ctx: Context): List[IR.Node] = {
    dcalStmts match {
      case Nil => List(IR.Node.Name(ctx.stateName))
      case s::ss =>
        val newCtx = ctx.withState(ctx.state + 1)
        List(
          IR.Node.Let(
            name = newCtx.stateName,
            binding = generateStatement(dcalStmt = s),
            body = generateStatements(dcalStmts = ss)(using newCtx)
          )
        )
    }
  }

  def generateDefinition(dcalDef: DCalAST.Definition): IR.Definition = {
    val initialCtx = Context(
      state = 1,
      nameInfoOf = Map[String, NameInfo](
        "str" -> NameInfo.State,
        "x" -> NameInfo.State,
        "y" -> NameInfo.State,
        "i" -> NameInfo.State
      )
    )

    IR.Definition(
      name = dcalDef.name,
      params = initialCtx.stateName +: dcalDef.params,
      body = generateStatements(
        dcalStmts = dcalDef.body.statements
      )(using initialCtx)
    )
  }

  def generateDefinition(dcalImport: String): IR.Definition = ???

  def build(dcalModule: DCalAST.Module): IR.Module = {
    // Construct the IR Module to return, which holds all the generated TLA+ code
    val definitions = dcalModule.definitions.map(generateDefinition)
    val imports = dcalModule.imports.map(generateDefinition)
    IR.Module(
      name = dcalModule.name,
      definitions = imports ++ definitions,
    )
  }

  def apply(contents: String, fileName: String): IR.Module = {
    // Set up logging
    // Parse DCal contents
    val dcalModule = DCalParser(contents = contents, fileName = fileName)
    // Build the IR
    build(dcalModule = dcalModule)
    // Log the IR
  }
}
