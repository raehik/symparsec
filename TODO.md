# Symparsec to-dos
* consider binary combinator names...
  * now using infix as base, but the internals use non-infix because it's
    easier. but they won't display properly in type errors... meh
  * oh and the parse errors aren't superb here too. kinda meh though

## Is this quietly an LL(1) parser combinator library?
I think so. The change to `Done` and the choice operator limitation behaviour
seems to support this. I had a think about how a `SepBy` parser would work, and
I think I can only implement that if neither the separator nor the main parser
fail "mid parse", which again lends credence.

It would be good to know this for sure. Maybe I could reconsider the interface
to writing parsers? It's clear that the LL(1) approach is easier, as it means we
don't need backtracking. So perhaps we could exploit the fact that you can write
more efficient parsers, with rules and stuff.

...I'm not sure. It certainly feels easiest to design parsers with combinators.
If we have to hide errors inside them for invalid grammars... so be it?.....

## How to wrap complicated parser results into nicer ones??
Relevant for e.g. expression parsing.

`Apply` (or inline `:@@:`) does an OK job for this.

## No mutually recursive parsers??
I can't figure this out. It seems we can't, because that doesn't work with the
state type calculation. It seems you have to manually construct the LL(1)
grammar. But online, people gladly shove mutual recursion in their LL(1)
grammars...?

This is the last thing that's restricting me from writing complex parsers. I'm
disappointed. Maybe I can make combinators to handle certain mutual recursion,
like I need to for `Many`...?

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
