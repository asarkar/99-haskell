[H-99: Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems)

[![](https://github.com/asarkar/99-haskell/workflows/CI/badge.svg)](https://github.com/asarkar/99-haskell/actions)

## The problems

* Questions 1 to 10: [Lists](src/Lists.hs)

* Questions 11 to 20: [Lists, continued](src/Lists2.hs)

* Questions 21 to 28: [Lists again](src/Lists3.hs)

* Questions 31 to 41: Arithmetic

* Questions 46 to 50: Logic and codes

* Questions 54A to 60: [Binary trees](src/BinaryTrees.hs)

* Questions 61 to 69: [Binary trees, continued](src/BinaryTrees2.hs)

* Questions 70B to 73: Multiway trees

* Questions 80 to 89: Graphs

* Questions 90 to 94: Miscellaneous problems

* Questions 95 to 99: Miscellaneous problems, continued

(Though the problems number from 1 to 99, there are some gaps and some additions marked with letters. There are actually only 88 problems.)

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
