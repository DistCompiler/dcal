package com.github.distcompiler.dcal

import cats.derived.*
import cats.data.{EitherT, WriterT, Chain}
import cats.*
import cats.syntax.all.given
import cats.instances.all.{given Ordering[?]}

import parsing.{Ps, PsK}
import transform.Transform
import transform.instances.all.given

object Scoping {
  import AST.*

  enum ScopingError {
    case Redefinition(first: PsK[Def], second: PsK[Def])
    case UndefinedReference(ref: PsK[Referent])
    case ArityMismatch(ref: PsK[Referent], defn: PsK[Def])
    case KindMismatch(ref: PsK[Referent], defn: PsK[Def])
  }

  type Def =
    Definition
    | Import
    | DefParam
    | Statement.Let
    | Statement.Var

  type Referent =
    Expression.OpCall
    | Statement.Call
    | Binding.Call

  final case class ScopingContext(defs: Map[String | BinaryOperator, Ps[Def]]) {
    def withDefs(defs: Map[String, Ps[Def]]): ScopingContext =
      copy(defs = this.defs ++ defs)

    def lookup(name: Path | BinaryOperator): Option[Ps[Def]] =
      name match {
        case op: BinaryOperator => defs.get(op)
        case Path.Name(name) => defs.get(name)
        case _ => None // TODO: actually implement this part
      }
  }
  object ScopingContext {
    val empty: ScopingContext = ScopingContext(defs = Map.empty)
  }

  private inline def ctx(using ctx: ScopingContext): ScopingContext = ctx

  final case class ScopingInfo(referencePairs: Chain[ReferencePair], errors: Chain[ScopingError]) derives Monoid

  type ReferencePair = (PsK[Referent], PsK[Def])
  type ReferenceInfo = Chain[ReferencePair]
  type Scoping[T] = WriterT[Eval, ScopingInfo, T]
  object Scoping {
    val unit: Scoping[Unit] = pure(())

    def pure[T](value: T): Scoping[T] =
      WriterT.value(value)

    def error(error: ScopingError): Scoping[Unit] =
      WriterT.tell(ScopingInfo(referencePairs = Chain.nil, errors = Chain.one(error)))

    def tellRef(ref: ReferencePair): Scoping[Unit] =
      WriterT.tell(ScopingInfo(referencePairs = Chain.one(ref), errors = Chain.nil))
  }

  extension [T](self: Scoping[T]) {
    def errorBarrier(ifOk: =>Scoping[T]): Scoping[T] =
      self.listen.flatMap {
        case (value, scopingInfo) if scopingInfo.errors.nonEmpty =>
          Scoping.pure(value)
        case _ => ifOk
      }

    def mapUnit: Scoping[Unit] = self.map(_ => ())
  }

  private given stringIsEmpty: Transform[String, Scoping[Unit]] =
    Transform.fromFunction(_ => Scoping.unit)

  private given bigIntIsEmpty: Transform[BigInt, Scoping[Unit]] =
    Transform.fromFunction(_ => Scoping.unit)

  def scopeModule(module: Module)(using ScopingContext): Scoping[Unit] = {
    val Module(name, imports, definitions) = module
    //assert(imports.isEmpty)// TODO: fixme

    val topLevelDefs: Map[String, Ps[Def]] = definitions.iterator
      .map {
        case defn @ Ps(Definition(Ps(name), _, _)) =>
          name -> defn.widen
      }
      .toMap

    definitions
      .groupBy(_.value.name.value)
      .toList
      .sortBy(_._1)
      .traverse_ {
        case (_, firstDef :: redefinitions) =>
          redefinitions
            .traverse_ { redefinition =>
              Scoping.error(ScopingError.Redefinition(firstDef.toPsK.widen, redefinition.toPsK.widen))
            }
        case _ =>
          Scoping.unit
      }
      .errorBarrier {
        definitions
          .traverse_(scopeDefinition(_)(using ctx.withDefs(topLevelDefs)))
      }
  }

  def scopeDefinition(defn: Ps[Definition])(using ScopingContext): Scoping[Unit] = {
    val Ps(Definition(name, arguments, body)) = defn

    val argDefs: Map[String, Ps[Def]] = arguments.iterator
      .map {
        case ps @ Ps(DefParam.Name(name)) => name -> ps.widen
      }
      .toMap

    arguments
      .groupBy(_.value)
      .toList
      .sortBy(_._1)
      .traverse_ {
        case (_, firstDef :: redefinitions) =>
          redefinitions.traverse_ { redefinition =>
            Scoping.error(ScopingError.Redefinition(firstDef.toPsK.widen, redefinition.toPsK.widen))
          }
        case _ => Scoping.unit
      }
      .errorBarrier {
        scopeStatement(using ctx.withDefs(argDefs))(body.widen)
      }
  }

  private def scopeCall(from: Ps[Referent], call: Binding.Call)(using ScopingContext): Scoping[Unit] = {
    val Binding.Call(path, arguments) = call
    for {
      _ <- ctx.lookup(path.value) match {
        case None =>
          Scoping.error(ScopingError.UndefinedReference(from.toPsK.widen))
        case Some(defn @ Ps(Definition(_, params, _))) =>
          if(params.size != arguments.size) {
            Scoping.error(ScopingError.ArityMismatch(from.toPsK.widen, defn.toPsK.widen))
          } else {
            Scoping.tellRef(from.toPsK.widen -> defn.toPsK.widen)
          }
        case Some(otherDef) =>
          Scoping.error(ScopingError.KindMismatch(from.toPsK.widen, otherDef.toPsK.widen))
      }
      _ <- arguments.traverse_(scopeExpression.asFunction)
    } yield ()
  }

  given scopeBinding(using ScopingContext): Transform.Refined[Ps[Binding], Scoping[Unit]] =
    Transform.Refined.fromPartialFunction {
      case from @ Ps(call @ Binding.Call(path, arguments)) =>
        scopeCall(from.as(call), call)
    }

  given scopeExpression(using ScopingContext): Transform.Refined[Ps[Expression], Scoping[Unit]] =
    Transform.Refined.fromPartialFunction {
      case from @ Ps(call @ Expression.OpCall(ident, arguments)) =>
        val fromCallK = from.as(call).toPsK
        val id = 
          ident match {
            case Left(op) => op
            case Right(path) => path
          }
        for {
          _ <- ctx.lookup(id.value) match {
            case None =>
              Scoping.error(ScopingError.UndefinedReference(fromCallK.widen))
            case Some(defn @ Ps(DefParam.Name(_) | Statement.Let(_, _) | Statement.Var(_, _))) =>
              if(arguments.isEmpty) {
                Scoping.tellRef(fromCallK.widen -> defn.toPsK.widen)
              } else {
                Scoping.error(ScopingError.ArityMismatch(fromCallK.widen, defn.toPsK.widen))
              }
            case Some(defn @ Ps(_: Definition | Import.Name(_))) =>
              Scoping.error(ScopingError.KindMismatch(fromCallK.widen, defn.toPsK.widen))
          }
          _ <- arguments.traverse_(scopeExpression.asFunction)
        } yield ()
    }

  given scopeStatement(using ScopingContext): Transform.Refined[Ps[Statement], Scoping[Unit]] =
    Transform.Refined.fromPartialFunction {
      case from @ Ps(stmtCall @ Statement.Call(Ps(call))) =>
        scopeCall(from.as(stmtCall).widen, call)
    }

  given scopeStatements(using ScopingContext): Transform.Refined[List[Ps[Statement]], Scoping[Unit]] = {
    def scopeLetVar(name: String, binding: Ps[Binding], defn: Ps[Def], restStmts: List[Ps[Statement]]): Scoping[Unit] = {
      val newCtx = ctx.withDefs(Map(name -> defn))
      for {
        _ <- scopeBinding(binding)
        _ <- restStmts.traverse_(scopeStatement(using newCtx)(_))
      } yield ()
    }

    Transform.Refined.fromPartialFunction[List[Ps[Statement]], Scoping[Unit]] {
      case (defn @ Ps(d @ Statement.Let(name, binding))) :: restStmts =>
        scopeLetVar(name.value, binding, defn.map(_ => d), restStmts)
      case (defn @ Ps(d @ Statement.Var(name, binding))) :: restStmts =>
        scopeLetVar(name.value, binding, defn.map(_ => d), restStmts)
    }
  }
}
