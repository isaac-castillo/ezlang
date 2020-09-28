Hi, this is an interpreter for a language implemented in Haskell.

You can use the repository in the following way:
This project uses Cabal as a package manager

```sh
$ stack build
$ stack run
$ stack test
```

This project is forked off of a school assignment.

```haskell
λ [0] let x = 100 in x + 3
103
λ [1] let a = [1,2,3,4] in head a
1
```