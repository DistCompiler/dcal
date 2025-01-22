// Copyright 2025 DCal Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package distcompiler.tla

import distcompiler.*

object defns:
  transparent sealed trait HasSpelling extends Product:
    def spelling: String = productPrefix

  sealed trait ReservedWord extends Token, HasSpelling
  object ReservedWord extends util.HasInstanceArray[ReservedWord]

  case object IN extends ReservedWord
  case object WITH extends ReservedWord
  case object THEN extends ReservedWord
  case object CONSTANTS extends ReservedWord
  case object ASSUMPTION extends ReservedWord
  case object VARIABLE extends ReservedWord
  case object WF_ extends ReservedWord
  case object ASSUME extends ReservedWord
  case object AXIOM extends ReservedWord
  case object CHOOSE extends ReservedWord
  case object OTHER extends ReservedWord
  case object EXTENDS extends ReservedWord
  case object VARIABLES extends ReservedWord
  case object EXCEPT extends ReservedWord
  case object INSTANCE extends ReservedWord
  case object THEOREM extends ReservedWord
  case object SF_ extends ReservedWord
  case object MODULE extends ReservedWord
  case object LET extends ReservedWord
  case object IF extends ReservedWord
  case object LOCAL extends ReservedWord
  case object ELSE extends ReservedWord
  case object CONSTANT extends ReservedWord
  case object CASE extends ReservedWord
  case object COROLLARY extends ReservedWord
  case object BY extends ReservedWord
  case object HAVE extends ReservedWord
  case object QED extends ReservedWord
  case object TAKE extends ReservedWord
  case object DEF extends ReservedWord
  case object HIDE extends ReservedWord
  case object RECURSIVE extends ReservedWord
  case object USE extends ReservedWord
  case object DEFINE extends ReservedWord
  case object PROOF extends ReservedWord
  case object WITNESS extends ReservedWord
  case object PICK extends ReservedWord
  case object DEFS extends ReservedWord
  case object PROVE extends ReservedWord
  case object SUFFICES extends ReservedWord
  case object NEW extends ReservedWord
  case object LAMBDA extends ReservedWord
  case object STATE extends ReservedWord
  case object ACTION extends ReservedWord
  case object TEMPORAL extends ReservedWord
  case object OBVIOUS extends ReservedWord
  case object OMITTED extends ReservedWord
  case object LEMMA extends ReservedWord
  case object PROPOSITION extends ReservedWord
  case object ONLY extends ReservedWord

  sealed trait Operator extends Token, HasSpelling

  object Operator:
    lazy val instances: IArray[Operator] =
      PrefixOperator.instances
        ++ InfixOperator.instances
        ++ PostfixOperator.instances

  sealed trait PrefixOperator(val lowPrecedence: Int, val highPrecedence: Int)
      extends Operator
  object PrefixOperator extends util.HasInstanceArray[PrefixOperator]

  case object `ENABLED` extends PrefixOperator(4, 15), ReservedWord
  case object `SUBSET` extends PrefixOperator(8, 8), ReservedWord
  case object `DOMAIN` extends PrefixOperator(9, 9), ReservedWord
  case object `[]` extends PrefixOperator(4, 15)
  case object `\\neg` extends PrefixOperator(4, 4)
  case object `~` extends PrefixOperator(4, 4)
  case object `UNION` extends PrefixOperator(8, 8), ReservedWord
  case object `<>` extends PrefixOperator(4, 15)
  case object `\\lnot` extends PrefixOperator(4, 4)
  case object `-_` extends PrefixOperator(12, 12)
  case object `UNCHANGED` extends PrefixOperator(4, 15), ReservedWord

  sealed trait InfixOperator(
      val lowPrecedence: Int,
      val highPredecence: Int,
      val isAssociative: Boolean = false
  ) extends Operator
  object InfixOperator extends util.HasInstanceArray[InfixOperator]

  case object `\\cong` extends InfixOperator(5, 5)
  case object `\\cdot` extends InfixOperator(5, 14, true)
  case object `\\sqsubseteq` extends InfixOperator(5, 5)
  case object `\\bullet` extends InfixOperator(13, 13, true)
  case object `**` extends InfixOperator(13, 13, true)
  case object `^^` extends InfixOperator(14, 14)
  case object `/\\` extends InfixOperator(3, 3, true)
  case object `|=` extends InfixOperator(5, 5)
  case object `\\succeq` extends InfixOperator(5, 5)
  case object `\\oslash` extends InfixOperator(13, 13)
  case object `\\sqcap` extends InfixOperator(9, 13, true)
  case object `*` extends InfixOperator(13, 13, true)
  case object `<=` extends InfixOperator(5, 5)
  case object `\\approx` extends InfixOperator(5, 5)
  case object `\\equiv` extends InfixOperator(2, 2)
  case object `%` extends InfixOperator(10, 11)
  case object `/=` extends InfixOperator(5, 5)
  case object `\\lor` extends InfixOperator(3, 3)
  case object `\\in` extends InfixOperator(5, 5)
  case object `\\div` extends InfixOperator(13, 13)
  case object `:>` extends InfixOperator(7, 7)
  case object `.` extends InfixOperator(17, 17, true)
  case object `\\asymp` extends InfixOperator(5, 5)
  case object `=` extends InfixOperator(5, 5)
  case object `\\prec` extends InfixOperator(5, 5)
  case object `\\circ` extends InfixOperator(13, 13, true)
  case object `\\succ` extends InfixOperator(5, 5)
  case object `\\simeq` extends InfixOperator(5, 5)
  case object `<` extends InfixOperator(5, 5)
  case object `\\notin` extends InfixOperator(5, 5)
  case object `::=` extends InfixOperator(5, 5)
  case object `\\cap` extends InfixOperator(8, 8, true)
  case object `\\ominus` extends InfixOperator(11, 11, true)
  case object `-|` extends InfixOperator(5, 5)
  case object `&` extends InfixOperator(13, 13, true)
  case object `=|` extends InfixOperator(5, 5)
  case object `|-` extends InfixOperator(5, 5)
  case object `\\` extends InfixOperator(8, 8)
  case object `=<` extends InfixOperator(5, 5)
  case object `(-)` extends InfixOperator(11, 11)
  case object `\\union` extends InfixOperator(8, 8, true)
  case object `>=` extends InfixOperator(5, 5)
  case object `=>` extends InfixOperator(1, 1)
  case object `\\leq` extends InfixOperator(5, 5)
  case object `\\propto` extends InfixOperator(5, 5)
  case object `\\sqcup` extends InfixOperator(9, 13, true)
  case object `||` extends InfixOperator(10, 11, true)
  case object `~>` extends InfixOperator(2, 2)
  case object `|` extends InfixOperator(10, 11, true)
  case object `\\odot` extends InfixOperator(13, 13, true)
  case object `\\sim` extends InfixOperator(5, 5)
  case object `\\o` extends InfixOperator(13, 13, true)
  case object `\\sqsupseteq` extends InfixOperator(5, 5)
  case object `-` extends InfixOperator(11, 11, true)
  case object `<=>` extends InfixOperator(5, 5)
  case object `@@` extends InfixOperator(6, 6, true)
  case object `??` extends InfixOperator(9, 13, true)
  case object `\\oplus` extends InfixOperator(10, 10, true)
  case object `\\land` extends InfixOperator(3, 3)
  case object `\\bigcirc` extends InfixOperator(13, 13)
  case object `++` extends InfixOperator(10, 10, true)
  case object `\\subset` extends InfixOperator(5, 5)
  case object `#` extends InfixOperator(5, 5)
  case object `\\subseteq` extends InfixOperator(5, 5)
  case object `..` extends InfixOperator(9, 9)
  case object `\\/` extends InfixOperator(3, 3, true)
  case object `\\supseteq` extends InfixOperator(5, 5)
  case object `\\uplus` extends InfixOperator(9, 13, true)
  case object `?` extends InfixOperator(5, 5)
  case object `(/)` extends InfixOperator(13, 13)
  case object `\\geq` extends InfixOperator(5, 5)
  case object `(.)` extends InfixOperator(13, 13)
  case object `(\\X)` extends InfixOperator(13, 13)
  case object `//` extends InfixOperator(13, 13)
  case object `+` extends InfixOperator(10, 10, true)
  case object `<:` extends InfixOperator(7, 7)
  case object `\\doteq` extends InfixOperator(5, 5)
  case object `...` extends InfixOperator(9, 9)
  case object `&&` extends InfixOperator(13, 13, true)
  case object `\\otimes` extends InfixOperator(13, 13, true)
  case object `\\preceq` extends InfixOperator(5, 5)
  case object `\\wr` extends InfixOperator(9, 14)
  case object `\\gg` extends InfixOperator(5, 5)
  case object `--` extends InfixOperator(11, 11, true)
  case object `\\ll` extends InfixOperator(5, 5)
  case object `\\intersect` extends InfixOperator(8, 8)
  case object `\\sqsupset` extends InfixOperator(5, 5)
  case object `$` extends InfixOperator(9, 13, true)
  case object `\\cup` extends InfixOperator(8, 8, true)
  case object `(+)` extends InfixOperator(10, 10)
  case object `:=` extends InfixOperator(5, 5)
  case object `!!` extends InfixOperator(9, 13)
  case object `^` extends InfixOperator(14, 14)
  case object `\\star` extends InfixOperator(13, 13, true)
  case object `$$` extends InfixOperator(9, 13, true)
  case object `>` extends InfixOperator(5, 5)
  case object `_##_` extends InfixOperator(9, 13, true):
    override def spelling: String = "##"
  case object `-+->` extends InfixOperator(2, 2)
  case object `/` extends InfixOperator(13, 13)
  case object `\\sqsubset` extends InfixOperator(5, 5)
  case object `\\supset` extends InfixOperator(5, 5)
  case object `%%` extends InfixOperator(10, 11, true)

  sealed trait PostfixOperator(val predecence: Int) extends Operator
  object PostfixOperator extends util.HasInstanceArray[PostfixOperator]

  case object `^+` extends PostfixOperator(15)
  case object `^*` extends PostfixOperator(15)
  case object `^#` extends PostfixOperator(15)
  case object `'` extends PostfixOperator(15)
