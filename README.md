[H-99: Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems)

[![](https://github.com/asarkar/99-haskell/workflows/CI/badge.svg)](https://github.com/asarkar/99-haskell/actions)

## The problems

* Questions 1 to 10: [Lists](src/Lists.hs)

* Questions 11 to 20: [Lists, continued](src/Lists2.hs)

* Questions 21 to 30: [Lists again](src/Lists3.hs)

* Questions 31 to 45: [Arithmetic](src/Arithmetic.hs)

* Questions 46 to 53: [Logic and codes](src/Logic.hs)

* Questions 54A to 60: [Binary trees](src/BinaryTrees.hs)

* Questions 61 to 69: [Binary trees, continued](src/BinaryTrees2.hs)

* Questions 70B to 73: [Multiway trees](src/MultiwayTrees.hs)

* Questions 74 to 79: [Monads](src/Monads.hs)

* Questions 80 to 89: [Graphs](src/Graphs.hs)

* Questions 90 to 94: [Miscellaneous problems](src/Misc.hs)

* Questions 95 to 99: [Miscellaneous problems, continued](src/Misc2.hs)

## Running tests

```
./.github/run.sh
```

To run all matching tests:
```
./.github/run.sh -m <some_word>
```

To run exactly matching tests:
```
./.github/run.sh -m "/<some_word>/"
```

To run a _specific test_:
```
./.github/run.sh -m "/Ch11/evaluates expression/eval/"
```

To run a file containing a `main` method:
```
stack runhaskell app/Main.hs
```

To run an executable listed in `package.yaml`:
```
stack build
stack exec <name>
```

## License

Released under [Apache License v2.0](LICENSE).
