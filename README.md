# GOLD level calculator

A command line programme to determine the GOLD level that users should be enrolled on.

## Technology

* [Stack](https://github.com/commercialhaskell/stack) is required to build the programme.
* It is currently built against [LTS Haskell 3.4](https://www.stackage.org/lts-3.4)
* Here is an [in-depth guide](https://www.fpcomplete.com/blog/2015/08/new-in-depth-guide-stack) to using Stack.

### Build

    stack build --pedantic

### Test

    stack test --pedantic

### Run (with Stack)

    stack exec gold-level-calculator -- --help

### Run (without Stack)

    build/gold-level-calculator --help
