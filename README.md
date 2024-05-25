# Symparsec
[hackage-parsec]: https://hackage.haskell.org/package/parsec

Type level string (`Symbol`) parser combinators. A [Parsec][hackage-parsec]-like
for `Symbol`s; thus, Symparsec! With all the features you'd expect:

* define parsers compositionally, largely as you would on the term level
* pretty, detailed parse errors
* decent performance (for simple parsers)

Parsers may also be reified and used at runtime with _guaranteed identical
behaviour_ via a healthy dose of singletons.

Requires GHC >= 9.6.

## Examples
Define a type-level parser:

```haskell
type PNumbers = Skip 1 :*>: Isolate 2 NatHex :<*>: (Literal "_" :*>: TakeRest))

ghci> :k! Run (Skip 3 :*>: Isolate 2 NatDec :<*>: (Skip 3 :*>: NatHex)) "10_FF"

```haskell
ghci> import Symparsec
ghci> :k! Run (Skip 3 :*>: Isolate 2 NatDec :<*>: (Skip 3 :*>: NatHex)) "___10___FF"
...
= Right '( '(10, 255), "")
```

## Why?
Via `GHC.Generics`, we may inspect Haskell data types on the type level.
Constructor names are `Symbols`. Ever reify these, then perform some sort of
checking or parsing on the term level? Symparsec does the parsing on the type
level instead. Catch bugs earlier, get faster runtime.

## Design
### The parser
TODO nope back to 3-tuple, but `Done` is non-consuming

A parser is a 4-tuple of:

* a consuming character parser; given a character and a state, returns
  * `Cont s`: keep going, here's the next state `s`
  * `Done r`: parse successful with value `r`
  * `Err  E`: parse error, details in the `E` (a structured error)
* an end handler, which takes only a state, and can only return `Done` or `Err`
* an initial "raw" state
* a state initializer, which turns the initial "raw" state into the first state
  value (the indirection here assists singling)

Running a parser is simple:

* initialize state
* parse character by character until end of input, or `Done`/`Err`

Parsers may not communicate with the runner any other way. This means no
backtracking, chunking etc. This is a conscious decision, made for simplicity.

Note that due to character parsers being consuming, we often need to do a bit of
"internal lookahead", where we check if we expect to consume any more
characters, and if not then emit a `Done`. It also means that non-consuming
parsers such as `Take 0` are invalid for non-empty strings. The state
initializer should be used to catch such cases.

This is a rough overview of parser design. See the code and/or Haddock
documentation for precise details.

### Pitfall: Character parsers always consume
There is no backtracking or lookahead, that you do not implement yourself. This
keeps the parser execution extremely simple, but breaks null parsers such as
`Drop 0`, so these must be handled specially (unless you don't getting mind
stuck type families on misuse).

For concrete examples, see the implementation of `Drop` and `Literal`.

### Pitfall: Not all parsers are definable
* No changing parser state. Thus, parsers such as `Try :: Parser s r -> Parser
  (s, [Char]) r` are not definable. _Parsers may not backtrack._
  * Combinators such as `<|>` can emulate backtracking, but they are complex and
    hard to reason about (they may have bugs!).

### Feature: Parsers may be reified to use at runtime, with guaranteed same behaviour
TODO

## Contributing
I would gladly accept further combinators or other suggestions. Please add an
issue or pull request, or contact me via email or whatever (I'm raehik
everywhere).

## License
Provided under the MIT license. See `LICENSE` for license text.
