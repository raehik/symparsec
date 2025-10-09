# Symparsec to-dos
* `type TakeWhile chPred = While chPred TakeRest`, but a custom parser would be
  faster (we could track output `rem`)

## Pre-2.0 release
* finish `Expr` example (I didn't finish error cases, also fill out AST more)
* check the various `Run` functions available
* check my uses could move easily (binrep, gtvm-hs)

## Proofs (tests?)
* `Isolate n TakeRest` is equivalent to `Take n`

## Combinators
* `Choice :: [PParser a] -> PParser a`
* various from parser-combinators, megaparsec (e.g. `sepBy`)

## Example uses of Symparsec to write, present
### This StackOverflow one from 2022-06
https://stackoverflow.com/questions/72762890/simplest-way-to-do-type-level-symbol-formatting-in-haskell

### generic-data-functions
Fiddly and awfully abstract. WIP.

### aeson
Big but real world. Write some new aeson generics which do type-level
constructor name parsing, instead of using `constructorTagModifier`. Daunting
because aeson has complex generics.
