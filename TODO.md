# Symparsec to-dos
* check Haddocks, especially for overloaded binders like `<$>`
* `type TakeWhile chPred = While chPred TakeRest`, but a custom parser would be
  faster (we could track output `rem`)
* parser type binders: `res :: PResult r`? what about `UnconsState`? etc.
* consider binary combinator names...
  * now using infix as base, but the internals use non-infix because it's
    easier. but they won't display properly in type errors... meh
  * oh and the parse errors aren't superb here too. kinda meh though

## Proofs (tests?)
* `Isolate n TakeRest` is equivalent to `Take n`

## No instances on parsers due to "Illegal type synonym family application"
I had to work around this in the singletons. But it also impacts other design. I
wanted to write a special generic-data-functions sum tag handler that uses the
Symparsec parser you provide and fills out all the info. But we can't, because
the reifying requires a type family instance on that parser, which we can't do.
(I couldn't figure out a workaround here, but it's only a very minor loss of
simplicity.)

## Combinators
* `Choice :: [PParserSym a] -> PParserSym a`

### Various from parser-combinators, megaparsec
* Helpers for writing `Count` with separators
  * We can't do `sepBy` exactly because it uses backtracking.
  * Maybe we have to write another combinator here :(
  * We need to parse `p` once first, then repeatedly parse `sep :*>: p`. That's
    what parser-combinators does. Not hard to make a fresh combinator that does
    this, but a bit disappointing if I can't define it with other combinators.

## Examples
### This StackOverflow one from 2022-06
https://stackoverflow.com/questions/72762890/simplest-way-to-do-type-level-symbol-formatting-in-haskell

### generic-data-functions
Fiddly and awfully abstract. WIP.

### aeson
Big but real world. Write some new aeson generics which do type-level
constructor name parsing, instead of using `constructorTagModifier`. Daunting
because aeson has complex generics.
