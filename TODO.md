# Symparsec to-dos
* consider binary combinator names...
  * now using infix as base, but the internals use non-infix because it's
    easier. but they won't display properly in type errors... meh

## Automatic reifying
I believe for this, I need full-blown singletons. `defun` supports that pattern
and has decent docs, but it's complex and not something I've done before. I
spent a good few days making various attempts to no avail.

The manual reification is an acceptable holdover to me, especially considering
combinator parsers work fine.

### Things I've tried that don't work
* Can't do parsers via class instances due to "conflicting family instance
  declarations". I suppose I utilize closed type families, but associated type
  families are open, which I can't have. I can't work around this without not
  pattern matching in my instance, which defeats the point.

## Combinators
None in particular. Probably a lot of easy ones though, please consider making
an issue or PR if you have one in mind.

## Examples
### aeson
Big but real world. Write some new aeson generics which do type-level
constructor name parsing, instead of using `constructorTagModifier`. Daunting
because aeson has complex generics.

### generic-data-functions
Fiddly and awfully abstract. WIP.

## Assorted
* TODO can we replace ParserEnd parts with an enforced Eot??!!! lmao maybe
  * hmmm idk. I think the ParserEnd separation just makes it safer, like we
    can't ever return a Cont at the end now which just makes things easier to
    track.
* implement this parser (2022-06)
  https://stackoverflow.com/questions/72762890/simplest-way-to-do-type-level-symbol-formatting-in-haskell
