package distcompiler.sexpr

import distcompiler.Wellformed
import distcompiler.Node

val SExprWellformed: Wellformed =
  import distcompiler.dsl.*
  Wellformed:
    Node.Top ::= tok(tokens.Atom, tokens.List)
    tokens.Atom ::= atEnd
    tokens.List ::= repeated(tok(tokens.Atom, tokens.List))