# Symparsec: Design notes
## Re-thinking
Current design:

* A parser is the product of
  * a consuming character parser
  * an end handler
  * a raw state (no type family applications)
  * a state initializer

Aaaand scratch this. Upon checking, Parsec has a very similar design, with
parsers defining an initial state and a handful of inner parsers. With the extra
requirement of parsers being consuming, I'm not bothered that I need to do
manual state initialization (I mean, it's just more required
defunctionalization).

Time to rewrite the whole library, again. lol
