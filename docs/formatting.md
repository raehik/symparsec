Notes here are in general promotable i.e. if I talk about a return type, it goes
for return kind when used in type-level code.

* parser string type binder: `str`
  * don't use `sym`.
* parser return type binder: `a`
* parser reply type binder: `rep`
  * ? `r` is fine, but it's often used for continuations. `rep` sounds like
    `representation` but that's it, so I consider it fairly unambiguous
* parser state type binder: `st`
  * ? reminds of `state token`, but otherwise sensible
* parser state index type binder: `idx`
  * simple, obvious
* parser state number kind binder: `n`
  * slightly preferred over `i` because it's a `Natural`
* parser error type binder: `e`
