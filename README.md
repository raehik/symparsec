# symbol-parser
Type-level string (`Symbol`) parser combinators.

## Features
* Define parsers compositionally as you would on the term level.
* Pretty parse errors. _(And I can make them prettier later, when I get bored.)_
* Probably not _that_ slow.
* No runtime cost (you shall find no term-level code here).

## Examples
```haskell
ghci> import Data.Type.Symbol.Parser
ghci> :k! Run (Drop 3 :*>: Isolate 2 NatDec :<*>: (Drop 3 :*>: NatHex)) "___10___FF"
...
= Right '( '(10, 255), "")
```

## Why?
Via `GHC.Generics`, we may inspect Haskell data types on the type level.
Constructor names are `Symbols`. Ever reify these, then perform some sort of
checking or parsing on the term level? symbol-parser does the parsing on the
term level instead. Catch bugs earlier, get faster runtime.

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
data Result   s r = Cont s | Done r | Err EParser
```

A parser is a function which takes a `Char`, the current state `s`, and returns
some result:

* `Cont s`: keep going, here's the next state `s`
* `Done r`: parse successful with value `r`
* `Err  EParser`: parse error, details in the `EParser` (a structured error)

`Run` handles calling the parser `Char` by `Char`, threading the state
through, and does some bookkeeping for nice errors.

TODO:

* bit more to parsers, see the end parser
* also explain the consume rule, why it's kept (apparent simplicity) and how to
  work around it (do extra work at parser start)
* Parsers are type families, yet we pass them around unsaturated as type
  arguments. We do this via defunctionalization symbols, with plumbing provided
  by Oleg's fantastic [defun-core][hackage-defun-core].

## I want to help
I would gladly accept further combinators or other suggestions. Please add an
issue or pull request, or contact me via email or whatever (I'm raehik
everywhere).

## License
Provided under the MIT license. See `LICENSE` for license text.
