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

transparent trait HasInstanceArray[T](using HasInstanceArray.Instances[T]):
  val instances: IArray[T] = summon[HasInstanceArray.Instances[T]].array

object HasInstanceArray:
  class Instances[T](val array: IArray[T]) extends AnyVal

  inline given summonInstances[T: reflect.ClassTag](using
      mirror: deriving.Mirror.SumOf[T]
  ): Instances[T] =
    given [S <: Singleton](using v: ValueOf[S]): S = v.value
    Instances:
      compiletime
        .summonAll[mirror.MirroredElemTypes]
        .toIArray
        .map(_.asInstanceOf[T])
