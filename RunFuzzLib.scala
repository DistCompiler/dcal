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

import edu.berkeley.cs.jqf.fuzz.junit.GuidedFuzzing
import edu.berkeley.cs.jqf.fuzz.guidance.Guidance
import edu.berkeley.cs.jqf.fuzz.guidance.Result
import edu.berkeley.cs.jqf.fuzz.ei.ZestGuidance

import java.io.File
import java.io.PrintStream
import java.util.Random

object RunFuzzLib {
  def main(args: Array[String]): Unit = {
    val testClassName = "distcompiler.FuzzLibTests"
    val testMethod = "simpleTest"

    val outputDir = new File("fuzz-output")

    if (!outputDir.exists()) {
      outputDir.mkdirs()
    }

    val guidance = new ZestGuidance(
      testMethod, // Name of the test method
      null, // Duration (null for unlimited time)
      10L, // Trial limit
      outputDir, // Output directory for results
      new Random() // Random number generator
    )

    println(s"Running JQF test: $testClassName#$testMethod")
    println(s"Output directory: ${outputDir.getAbsolutePath}")

    val classLoader = Thread.currentThread().getContextClassLoader()

    val result = GuidedFuzzing.run(
      testClassName,
      testMethod,
      classLoader,
      guidance,
      System.out
    )

    if (result.wasSuccessful()) {
      println("Test completed successfully without finding failures.")
    } else {
      println(s"Test found ${result.getFailureCount()} failures!")
      result.getFailures().forEach { failure =>
        println(s"Failure: ${failure.getMessage()}")
        failure.getException().printStackTrace()
      }
    }
  }
}
