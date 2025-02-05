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

//> using scala 3
//> using options -Werror -deprecation -feature -Yexplicit-nulls -Xcheck-macros
//> using dep com.lihaoyi::os-lib:0.11.3
//> using dep com.lihaoyi::sourcecode:0.4.2
//> using dep org.typelevel::cats-core:2.13.0
//> using dep dev.zio::izumi-reflect:3.0.1
//> using dep com.lihaoyi::ujson::4.1.0
//> using dependency "io.github.java-diff-utils:java-diff-utils:4.15"
//> using test.dep org.scalameta::munit:1.1.0

//> using javaProp distcompiler.Node.assertErrorRefCorrectness=no

// discarded flags: -Yrequire-targetName
