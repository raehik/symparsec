# Symparsec
[hackage-parsec]: https://hackage.haskell.org/package/parsec

Type level string (`Symbol`) parser combinators. A [Parsec][hackage-parsec]-like
for `Symbol`s; thus, Symparsec! With many of the features you'd expect:

* define parsers compositionally, largely as you would on the term level
* pretty, detailed parse errors
* decent performance (for simple parsers)

Parsers may also be reified and used at runtime with _guaranteed identical
behaviour_ via a healthy dose of singletons.

Requires GHC >= 9.6.

## Examples
Define a type-level parser:

```haskell
import Symparsec
type PExample = Skip 1 :*>: Isolate 2 NatHex :<*>: (Literal "_" :*>: TakeRest)
```

Use it to parse a type-level string (in a GHCi session):

```haskell
ghci> :k! Run PExample "xFF_"
Run ...
= Right '( '(255, "etc"), "")
```

Use it to parse a different, term-level string:

```haskell
ghci> import Singleraeh.Demote ( demote )
ghci> run' @PExample demote "abc_123"
Right ((188,"123"),"")
```

## Why?
Via `GHC.Generics`, we may inspect Haskell data types on the type level.
Constructor names are `Symbols`. Ever reify these, then perform some sort of
checking or parsing on the term level? Symparsec does the parsing on the type
level instead. Catch bugs earlier, get faster runtime.

Also type-level Haskell authors deserve fun libraries too!!

## Design
### The parser
A parser is a 3-tuple of:

* a character parser; given a character and a state, returns
  * `Cont s`: keep going, here's the next state `s`
  * `Done r`: parse successful with value `r`, _do not consume character_
  * `Err  E`: parse error, details in the `E` (a structured error)
* an end handler, which takes only a state, and can only return `Done` or `Err`
* an initial state

Running such a parser is very simple:

* initialize state
* parse character by character until end of input, or `Done`/`Err`

Parsers may not communicate with the runner any other way. This means no
backtracking, chunking etc. This is a conscious decision, made for simplicity.
We're still able to implement a good handful of parser combinators regardless,
including a limited form of backtracking.

This is a rough overview. See the code & Haddocks for precise details.

## Contributing
I would gladly accept further combinators or other suggestions. Please add an
issue or pull request, or contact me via email or whatever (I'm raehik
everywhere).

## License
Provided under the MIT license. See `LICENSE` for license text.
