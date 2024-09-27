package distcompiler

import scala.quoted.*

transparent trait Named(using ownName: Named.OwnName):
  final def name: String = ownName.name
end Named

object Named:
  final class OwnName(val name: String) extends AnyVal

  private def findOwnNameImpl(using quotes: Quotes): Expr[OwnName] =
    import quotes.reflect.*

    @scala.annotation.tailrec
    def stripMacroConstructorStuff(sym: Symbol): TypeRepr =
      if sym.flags.is(Flags.Macro) || sym.isClassConstructor
      then stripMacroConstructorStuff(sym.owner)
      else if sym.isClassDef && sym.companionModule.exists
      then
        val symTermRef = sym.companionModule.termRef
        if symTermRef.isSingleton
        then symTermRef
        else report.errorAndAbort(s"${symTermRef.show} is not a singleton")
      else report.errorAndAbort(s"${sym.fullName} not a class/object, or has no companion object")

    val theName = stripMacroConstructorStuff(Symbol.spliceOwner).show

    '{ OwnName(${ Expr(theName) }) }

  inline given findOwnName: OwnName =
    ${ findOwnNameImpl }
end Named