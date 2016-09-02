# Chess Move Generator in Haskell

This project is the start of a chess engine written in Haskell.  I simply wanted
to try implementing legal chess move generation in Haskell, so I've completely
prioritized ease-of-use and clarity over efficiency in the implementation.  

This library contains the following functionality.

* Representation of a chess game state, including board, pieces, and moves
* Parses FEN game strings into a `GameState`
* Generates all legal moves
* Perft test suites to verify correctness of move generator

## Building

The build uses cabal.  After checking out the project, execute the following
steps in the top level of the project:

```bash
cabal configure --enable-tests
cabal build
```

If you get errors about missing dependencies, run this command to install
dependencies, and then re-run configure and build.

```bash
cabal sandbox init
cabal install --dependencies-only --enable-tests
```

If you don't mind packages being installed globally, leave off the sandbox
command.

## Running tests

To run all tests:

```
cabal test --show-details=direct
```

This runs all the perft tests, which generate all moves up to 5 or 6 levels deep
from various starting positions, and compares the move counts at each level to
known correct values.  These tests take a long time to run, at least an hour.
This can be sped up considerably by enabling parallel test execution.  Use the
`--test-options` argument to cabal to pass arguments to the test executable, in
this case to tell it to use multiple threads:

```
cabal test --show-details=direct --test-options="+RTS -N -RTS"
```

Instead of using `--test-options`, you can also call the test executable
directly, which makes it easier to pass many options and options with spaces,
for example to run only specific tests for example.

Here is how to run only the FEN parsing tests:

```
dist/build/chess-tests/chess-tests -m Chess.FEN
```

As tests are run, their names are printed in a hierarchically indented format.
You can specify any individual test by specifying the levels separated by /.
Here is how to specify only running the depth 5 detailed move generation tests
for a specific board position:

```
dist/build/chess-tests/chess-tests -m 'Chess.PerftDetailed/Detailed Perft test for "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"/Counts for depth 5'
```

## Testing using ghci

There are two ways to use the library from ghci.  The easiest is:

```
cabal repl
```

This will start ghci and load all the modules under src/ into ghci.  The nice thing about this is it loads the modules
from source, so you always have the latest version.  The downside is it loads them in interpreted mode, so everything
runs very slow. 

If I'm worried about the speed, because for instance I want to do interactive perft testing, then here's how I can get
the compiled version in ghci, assuming I'm using a cabal sandbox:

```
cabal install
ghci -no-user-package-db -package-db .cabal-sandbox/x86_64-osx-ghc-8.0.1-packages.conf.d
```

The actual ghc version number in the path might be different if I'm using a different version of ghc.

## Profiling

To run tests with profiling, first we need to rebuild everything with profiling:

First clean up existing build artifacts:

```
cabal clean
cabal sandbox delete
```

Then rebuild with profiling:

```
cabal sandbox init
cabal install --dependencies-only --enable-tests --enable-profling
cabal configure --enable-tests --enable-profiling
cabal build
```

Now run, passing profiling options to the executable.  For example, to generate heap
profiling info:

```
dist/build/chess-tests/chess-tests -m Chess.FEN +RTS -hd -RTS
```

For time profiling, pass `-p` instead of `-hd`.  See the 
[GHC Manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#profiling) 
for more info on compile time and runtime flags for profiling.

