# Symparsec to-dos
* consider binary combinator names...
  * now using infix as base, but the internals use non-infix because it's
    easier. but they won't display properly in type errors... meh
  * oh and the parse errors aren't superb here too. kinda meh though

## No instances on parsers due to "Illegal type synonym family application"
I had to work around this in the singletons. But it also impacts other design. I
wanted to write a special generic-data-functions sum tag handler that uses the
Symparsec parser you provide and fills out all the info. But we can't, because
the reifying requires a type family instance on that parser, which we can't do.
(I couldn't figure out a workaround here, but it's only a very minor loss of
simplicity.)

## Combinators
In general, we should be able to implement any parser we see that doesn't use
choice (`MonadPlus`/`Alternative`). Even those ones we might be able to define a
limited version, but only if it seems helpful.

### Various from parser-combinators, megaparsec
* `TakeWhile :: (Char ~> Bool) -> PParser [Char] Symbol`
  * `type TakeTill ch = TakeWhile (_ ch)`
  * (?) `While :: (Char ~> Bool) -> PParser s r -> PParser s r`
    * then seemingly `type TakeWhile p = While p TakeRest`...?
    * `While IsHex NatHex` seems obvious
    * similarly `While (IsNot "_") NatHex` (both useful, depends on your schema)
    * `Until` would just be an inverted character predicate so probs ignore
  * seems if we define this, we could implement a lot of megaparsec parsers
* `Count :: Natural -> PParser s r -> PParser _ [r]

## Examples
### This StackOverflow one from 2022-06
https://stackoverflow.com/questions/72762890/simplest-way-to-do-type-level-symbol-formatting-in-haskell

### generic-data-functions
Fiddly and awfully abstract. WIP.

### aeson
Big but real world. Write some new aeson generics which do type-level
constructor name parsing, instead of using `constructorTagModifier`. Daunting
because aeson has complex generics.
