package com.github.distcompiler.dcal

import cats.derived.*
import cats.data.{EitherT, WriterT, Chain}
import cats.{Eval, Monoid}
import cats.implicits.given

import parsing.Ps
import transform.Transform

object Scoping {
  import DCalAST.*

  enum ScopingError {
    case Redefinition(first: Ps[Def], second: Ps[Def])
    case UndefinedReference(ref: Ps[Referent])
  }

  type Def =
    Definition
    | String
    | Statement.Let
    | Statement.Var

  type Referent =
    Expression.OpCall
    | Statement.Call
    | Binding.Call

  final case class ScopingContext(defs: Map[String | DCalTokenizer.BinaryOperator, Ps[Def]]) {
    def withDefs(defs: Map[String, Ps[Def]]): ScopingContext =
      copy(defs = this.defs ++ defs)

    def lookup(name: Path | DCalTokenizer.BinaryOperator): Option[Ps[Def]] =
      name match {
        case op: DCalTokenizer.BinaryOperator => defs.get(op)
        case Path.Name(name) => defs.get(name)
        case _ => None // TODO: actually implement this part
      }
  }
  object ScopingContext {
    val empty: ScopingContext = ScopingContext(defs = Map.empty)
  }

  private inline def ctx(using ctx: ScopingContext): ScopingContext = ctx

  final case class ScopingInfo(referencePairs: Chain[(Ps[Referent], Ps[Def])], errors: Chain[ScopingError]) derives Monoid

  type ReferencePair = (Ps[Referent], Ps[Def])
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

  private given stringIsEmpty: Transform[String, Scoping[Unit]] with {
    override def apply(from: String): Scoping[Unit] = Scoping.unit
  }

  private given psStringIsEmpty: Transform[Ps[String], Scoping[Unit]] with {
    override def apply(from: Ps[String]): Scoping[Unit] = Scoping.unit
  }

  private given bigIntIsEmpty: Transform[BigInt, Scoping[Unit]] with {
    override def apply(from: BigInt): Scoping[Unit] = Scoping.unit
  }

  def scopeModule(module: Module)(using ScopingContext): Scoping[Unit] = {
    val Module(name, imports, definitions) = module
    //assert(imports.isEmpty)// TODO: fixme

    val topLevelDefs: Map[String, Ps[Def]] = definitions.iterator
      .map {
        case defn @ Ps(Definition(Ps(name), _, _)) =>
          name -> defn.up
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
              Scoping.error(ScopingError.Redefinition(firstDef.up, redefinition.up))
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

    val argDefs: Map[String, Ps[Def]] = arguments.iterator.map(ps => ps.value -> ps.up).toMap

    arguments
      .groupBy(_.value)
      .toList
      .sortBy(_._1)
      .traverse_ {
        case (_, firstDef :: redefinitions) =>
          redefinitions.traverse_ { redefinition =>
            Scoping.error(ScopingError.Redefinition(firstDef.up, redefinition.up))
          }
        case _ => Scoping.unit
      }
      .errorBarrier {
        scopeStatement(body.up)(using ctx.withDefs(argDefs))
      }
  }

  def scopeBinding(binding: Ps[Binding])(using ScopingContext): Scoping[Unit] = {
    given transformCall: Transform[Ps[Binding.Call], Scoping[Unit]] with {
      override def apply(from: Ps[Binding.Call]): Scoping[Unit] = {
        val Ps(Binding.Call(path, arguments)) = from
        for {
          _ <- ctx.lookup(path.value) match {
            case None =>
              Scoping.error(ScopingError.UndefinedReference(from.up))
            case Some(defn) =>
              Scoping.tellRef(from.up -> defn.up)
          }
          _ <- arguments.traverse_(scopeExpression)
        } yield ()
      }
    }

    Transform[Ps[Binding], Scoping[Unit]](binding)
  }

  def scopeExpression(expression: Ps[Expression])(using ScopingContext): Scoping[Unit] = {
    given transformOpCall: Transform[Ps[Expression.OpCall], Scoping[Unit]] with {
      override def apply(from: Ps[Expression.OpCall]): Scoping[Unit] = {
        // TODO: arity checking!
        val Ps(Expression.OpCall(ident, arguments)) = from
        val id = 
          ident match {
            case Left(op) => op
            case Right(path) => path
          }
        for {
          _ <- ctx.lookup(id.value) match {
            case None =>
              Scoping.error(ScopingError.UndefinedReference(from.up))
            case Some(defn) =>
              Scoping.tellRef(from.up -> defn.up)
          }
          _ <- arguments.traverse_(scopeExpression)
        } yield ()
      }
    }

    Transform[Ps[Expression], Scoping[Unit]](expression)
  }

  def scopeStatement(stmt: Ps[Statement])(using ScopingContext): Scoping[Unit] = {
    given transformCall: Transform[Ps[Statement.Call], Scoping[Unit]] with {
      override def apply(from: Ps[Statement.Call]): Scoping[Unit] = {
        val Ps(Statement.Call(call)) = from
        scopeBinding(call.up)
      }
    }

    given transformExpr: Transform[Ps[Expression], Scoping[Unit]] = scopeExpression

    given transformStmts: Transform[List[Ps[Statement]], Scoping[Unit]] with {
      override def apply(from: List[Ps[Statement]]): Scoping[Unit] =
        from match {
          case Nil => Scoping.unit
          case stmt :: restStmts =>
            def scopeLetVar(name: String, binding: Ps[Binding], defn: Ps[Def]): Scoping[Unit] = {
              val newCtx = ctx.withDefs(Map(name -> defn))
              for {
                _ <- scopeBinding(binding)
                _ <- restStmts.traverse_(scopeStatement(_)(using newCtx))
              } yield ()
            }

            stmt match {
              case defn @ Ps(d @ Statement.Let(name, binding)) =>
                scopeLetVar(name.value, binding, defn.map(_ => d))
              case defn @ Ps(d @ Statement.Var(name, binding)) =>
                scopeLetVar(name.value, binding, defn.map(_ => d))
              case _ =>
                Monoid.combine(transformStmt(stmt), transformStmts(restStmts))
            }
        }
    }

    lazy val transformStmt: Transform[Ps[Statement], Scoping[Unit]] = Transform.apply

    transformStmt(stmt)
  }
}