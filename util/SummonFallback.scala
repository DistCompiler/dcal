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

package distcompiler.util

opaque type SummonFallback[First, Second] = Either[Second, First]

object SummonFallback {
  // due to specificity rules, this "subclass"" counts as more specific than above
  opaque type Primary[First, Second] <: SummonFallback[First, Second] =
    Either[Second, First]

  extension [First, Second](self: SummonFallback[First, Second]) {
    def value: Either[Second, First] = self
  }

  given summonFirst[First, Second](using first: First): Primary[First, Second] =
    Right(first)
  // since it returns the base class, the compiler will only use summonSecond if it can't find summonFirst
  // unlike other methods to achieve this, it will also print a mostly-comprehensible summoning diagnostic if something
  // bad happens, unlike say using a two-case summonFrom where the macro trace replaces the summoning diagnostic
  given summonSecond[First, Second](using
      second: Second
  ): SummonFallback[First, Second] = Left(second)
}
