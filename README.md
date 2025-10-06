# Symparsec
[hackage-parsec]: https://hackage.haskell.org/package/parsec

Type level string parser combinators. A [Parsec][hackage-parsec]-like for
`Symbol`s; thus, Symparsec! With many of the features you'd expect:

* define parsers compositionally, largely as you would on the term level
* pretty, detailed parse errors
* good performance (probably-- please help me benchmark!)

Requires GHC >= 9.6.

## Examples
Define a type-level parser:

```haskell
import Symparsec
type PExample = Skip 1 *> Isolate 2 NatHex <*> (Literal "_" *> TakeRest)
```

Use it to parse a type-level string (in a GHCi session):

```haskell
ghci> :k! Run PExample "xFF_etc"
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

## Limitations
The key limitation Symparsec grapples with is having __no binders on the type
level.__ This means:

* no `let`s, no `where`s
* no `do` notation

The workaround is writing extra functions, and writing uglier functions.
Otherwise, I believe an average term-level Parsec-like parser should look
comparable to a type-level `Symparsec` one.

Writing complex type-level Haskell programs in 2025 is unintuitive. I intend to
provide guides on writing Symparsec parsers that walk through type-level
programming design patterns and solutions. Please ping me on the issues tab if
you read this, and can't find any such guides!

## Contributing
I would gladly accept further combinators or other suggestions. Please add an
issue or pull request, or contact me via email or whatever (I'm raehik
everywhere).

## License
Provided under the MIT license. See `LICENSE` for license text.
