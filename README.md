[![BuildStatus](https://travis-ci.org/mlitchard/editor.svg?branch=master)](https://travis-ci.org/mlitchard/editor)
editor
==========
This is an implementation of HackerRank Challenge
https://www.hackerrank.com/challenges/simple-text-editor

Here is the directory structure.

src-exe/Main.hs - The primary executable

src-test/Main.hs - The testing executable

src-test/UnitTests/UnitTests.hs - Where the unit tests live.

src-test/UnitTests/Scaffolding.hs - Scaffolding for the unit tests.

src-test/PropTest.hs - Property tests the for the command functions.

src-benchmark/Main.hs - Benchmark executable

editor-test.hp - graph of profiling data obtained via testing

editor-test.ps - hp file converted via hp2ps

editor-test.prof - profiling data obtained via testing.

edt_benchmark.html - pretty picture from criterion.

src/Types.hs - Types and type synonyms for the editor.

src/Editor.hs - The high level driver code. Where the conduits live.

src/Actions.hs - The stateful functions with corresponding pure functions.

doc/Overview.txt - A wide-brush commentary.

doc/Testing.txt - Commentary on testing.

doc/Design.txt - Some comments on why, plus tradeoffs.

To install:

You may have to run stack setup

then, do

stack install.

To benchmark:
stack bench --benchmark-arguments '--output=edt_benchmark.html'

To run tests (and grab profiling data):
stack test --profile --test-arguments="+RTS -h -RTS"
