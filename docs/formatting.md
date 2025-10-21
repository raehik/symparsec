Notes here are in general promotable i.e. if I talk about a return type, it goes
for return kind when used in type-level code.

## Types
### Promoting, singling
In general, types should be defined polymorphic enough to be used on both term
and type level (via `DataKinds` automatic promotion). Promoted type synonyms may
be used to fill out kinds for type-level computations (e.g. `Symbol` for
type-level strings). These should be prefixed with `P`.

Similarly, singled types should be prefixed with `S`.

Note that I don't believe one can be polymorphic over the function arrow i.e.
decide whether to use a `->` regular arrow (for term-level), or `~>`
defunctionalized arrow. This means some promoted types might not actually use
the term-level type directly. That's fine.

## Binders
* parser state string type: `str`
  * don't use `sym`.
* parser state numeric type: `n`
  * slightly preferred over `i` because it's a `Natural`
* parser return type: `a`
* parser reply: `rep`
  * ? `r` is fine, but it's often used for continuations. `rep` sounds like
    `representation` but that's it, so I consider it fairly unambiguous
* parser state: `ps`
* parser state custom state: `s`
* parser state index: `idx`
  * simple, obvious
* parser error: `e`
