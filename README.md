# Symparsec
Type level string (`Symbol`) parser combinators.

It's a Parsec for `Symbol`s; thus, Symparsec.

Previously named symbol-parser.

Requires GHC 9.6 for singling parsers.

## Features
* Define parsers compositionally, largely as you would on the term level.
* Pretty parse errors.
* Hopefully decent performance.
* No runtime cost (you shall find no term level code here).

## Examples
```haskell
ghci> import Symparsec
ghci> :k! Run (Drop 3 :*>: Isolate 2 NatDec :<*>: (Drop 3 :*>: NatHex)) "___10___FF"
...
= Right '( '(10, 255), "")
```

## Why?
Via `GHC.Generics`, we may inspect Haskell data types on the type level.
Constructor names are `Symbols`. Ever reify these, then perform some sort of
checking or parsing on the term level? Symparsec does the parsing on the type
level instead. Catch bugs earlier, get faster runtime.

## Design
### Parser basics
[hackage-defun-core]: https://hackage.haskell.org/package/defun-core
[hackage-symbols]: https://hackage.haskell.org/package/symbols

_(Note that I may omit "type-level" when referring to type-level operations.)_

Until GHC 9.2, `Symbol`s were largely opaque. You could get them, but you
couldn't _decompose_ them like you could `String`s with `uncons :: [a] -> Maybe
(a, [a])`. Some Haskellers took this as a challenge (see
[symbols][hackage-symbols]). But realistically, we needed a bit more compiler
support.

As of GHC 9.2, `Symbol`s may be decomposed via `UnconsSymbol :: Symbol -> Maybe
(Char, Symbol)`. We thus design a `Char`-based parser:

```haskell
type ParserCh s r = Char -> s -> Result s r
data Result   s r = Cont s | Done r | Err E
```

A parser is a function which takes a `Char`, the current state `s`, and returns
some result:

* `Cont s`: keep going, here's the next state `s`
* `Done r`: parse successful with value `r`
* `Err  E`: parse error, details in the `E` (a structured error)

`Run` handles calling the parser `Char` by `Char`, threading the state through,
and does some bookkeeping for nice errors.

This is a good first step, but we have some outstanding issues:

* What do we do when we reach the end of the string?
* How do we initialize parser state?
* How do we pass parsers around? (As of 2024-04-20, GHC HEAD does not support
  unsaturated type families.)

As always, types are our salvation.

```haskell
type ParserEnd s r = s -> Either E r
type Parser s r = (ParserChSym s r, ParserEndSym s r, s)

type ParserChSym s r = Char ~> s ~> Result s r
type ParserEndSym s r = s ~> Either E r
```

We define a parser as a tuple of a character parser, an end handler, and an
initial state. The types ending in `Sym` are _defunctionalization symbols_,
which enable us to pass our parsers around as type-level functions. The plumbing
is provided Oleg's fantastic library [defun-core][hackage-defun-core].

### Example: `NatDec`
Let's write a parser that parses a natural decimal.

```haskell
type NatDec = '(NatDecChSym, NatDecEndSym, 0)

type NatDecCh ch acc = NatDecCh' acc (ParseDecimalDigitSym @@ ch)
type family NatDecCh' acc mDigit where
    NatDecCh' acc (Just digit) = Cont (acc * 10 + digit)
    NatDecCh' acc Nothing      = Err -- ...

type NatDecEnd acc = Right acc

-- boring defunctionalization symbol definitions
```

At each `Char`, we attempt to parse as a digit. If it's valid, we multiply the
current accumulator by 10 (a left shift) and add the digit value. At the end of
the string, we simply emit the current accumulator.

It can be that easy to define a parser with Symparsec! But it isn't always.
Combinators get weird thanks to state handling. Take a look at `Isolate`, then
`Then`.

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

### Pitfall: Clumsy invalid parser handling
As mentioned above, it is possible to define invalid parsers. We can catch them
ad-hoc on the type-level, but not on the term-level when singling, and it
complicates reflecting.

We could defunctionalize a step further. Reflection would be cleaner. But parser
syntax would have to change. We'd need something like `Run Take 1 "input"`,
where `Take` is a defun symbol taking a single argument. More examples:

  * `Isolate 1 Take 1`: isolate takes 3 args: n, a -> p, a
  * `Then Isolate 5 Take 1`: `Isolate 5 :<|>: Take 1`
    * can't binop `:<|>:` anymore :(

It doesn't seem like a good idea at all. Instead, we must deal with some weird
reflecting, and wonky (type) errors when you define invalid parsers.

Thought: We could add another defun symbol to `Parser`, like
`parserCheck :: s -> Maybe (E, s)`. Even so, `Literal` would need reworking
(actually I think it needs it, I only wrote it like that to reduce equation
count, but it's silly).

That function would check that the initial state is OK. If it is, `Nothing`. If
not, it returns a default state to use _only_ for the end handler, and an error
to emit if it tries to consume a character.

I might actually need `parserInit :: s0 -> Either (E, s1) s1`? At least, it
would appear to help my reflecting. See `Literal''`, which is a very ugly way of
doing things. If I could do `type Literal str = 'Parser pCh pEnd str ...`...

OH do I simply replace the init state with a defun symbol?

I need to consider what this changes for combinator parsers. `Or` will be
interesting.

### Feature: Parsers may be reflected to use at runtime, with guaranteed same behaviour
TODO

## Contributing
I would gladly accept further combinators or other suggestions. Please add an
issue or pull request, or contact me via email or whatever (I'm raehik
everywhere).

## License
Provided under the MIT license. See `LICENSE` for license text.
