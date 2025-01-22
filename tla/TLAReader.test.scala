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

class TLAReaderTests extends munit.FunSuite, test.WithTLACorpus:
  self => // funny parser ambiguity: try deleting this line

  testWithCorpusFile: file =>
    val src = Source.mapFromFile(file)
    val top = TLAReader(SourceRange.entire(src))

    // re-enable if interesting:
    // os.write.over(
    //   os.pwd / "dbg_tla_reader" / s"${file.last}.dbg",
    //   top.toPrettyWritable(TLAReader.wellformed),
    //   createFolders = true
    // )

    if top.hasErrors
    then fail(top.presentErrors(debug = true))

    if top.children.isEmpty
    then fail("no data extracted")
