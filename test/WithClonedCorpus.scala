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

package distcompiler.test

transparent trait WithClonedCorpus:
  self: munit.FunSuite =>
  def repoURL: String
  def intoName: String

  def isCorpusFile(file: os.Path): Boolean

  def clonesDir: os.Path = os.pwd / ".clones"
  private def checkoutFolder = clonesDir / intoName

  def testWithCorpusFile(using munit.Location)(fn: os.Path => Unit): Unit =
    if !os.exists(checkoutFolder)
    then
      os.makeDir.all(clonesDir)
      os.proc("git", "clone", "--recurse-submodules", repoURL, checkoutFolder)
        .call()

    var foundAnything = false

    os.walk
      .stream(checkoutFolder)
      .filter(isCorpusFile)
      .foreach: corpusFile =>
        foundAnything = true
        test(corpusFile.relativeTo(clonesDir).toString)(fn(corpusFile))

    test("sanity"):
      assert(
        foundAnything,
        s"must find at least one test file in $checkoutFolder"
      )
