// Copyright 2024-2025 DCal Team
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

package distcompiler

import edu.berkeley.cs.jqf.fuzz.Fuzz
import edu.berkeley.cs.jqf.fuzz.JQF
import org.junit.runner.RunWith
import org.junit.Assert.*

@RunWith(classOf[JQF])
class FuzzLibTests:
  private val fuzzLib = new FuzzLib()

  @Fuzz
  def simpleTest(flag: Boolean, value: Int): Unit =
    try
      val result = fuzzLib.fillerFunction(flag, value)

      if !flag then fail("Expected exception was not thrown")
      else if value > 0 then assertTrue("Positive value", result > 0)
      else assertTrue("Non-positive value", result <= 0)
    catch
      case e: RuntimeException =>
        assertTrue("Exception thrown when flag was true", !flag)
        assertEquals("Found a failure case", e.getMessage)
