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

import scala.util.control.TailCalls.*

object TailCallsUtils:
  extension [T](iter: Iterator[T])
    def intercalate(value: T): Iterator[T] =
      new Iterator[T]:
        var prependSep = false
        def hasNext: Boolean = iter.hasNext
        def next(): T =
          if prependSep && iter.hasNext
          then
            prependSep = false
            value
          else
            prependSep = true
            iter.next()

    def traverse(fn: T => TailRec[Unit]): TailRec[Unit] =
      def impl: TailRec[Unit] =
        if !iter.hasNext
        then done(())
        else
          for
            () <- tailcall(fn(iter.next()))
            () <- tailcall(impl)
          yield ()

      impl
