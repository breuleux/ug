
Unholy Grail
============

Unholy Grail is a new programming language which compiles to Python.

It is quite alpha at the moment, but most of the features work.

You can get a feel for the language with the examples in the examples/
folder. They can be executed with the command: `ug eval
<filename>`. You can also execute bits of code with `ug eval -s
<code>`.

Here's what a recursive implementation of fibonacci looks like in UG:

    fib =
        [0] -> 0
        [1] -> 1
        [n] when (n > 1) ->
            fib[n - 1] + fib[n - 2]

    print["The 10th fibonaccy number is", fib[10]]



Current features
----------------

* No distinction between expression and statement.
* Featureful pattern matching. It works almost everywhere: variable
  declaration, parameter declaration, loop variables, exceptions,
  deconstruction into dictionary keys and as a standalone `match`
  construct.
* Optional "typing", which is in fact part of the pattern matching
  syntax.
* Ad hoc structures: `#point[1, 2]` instantiates a kind of tuple of type
  `#point`, `#point[x = 1, y = 2]` an object with named fields. They
  can be pattern matched.
* Exception handling operator: `result = (1/0 !! None)` or
  `result = (1/0 !! ZeroDivisionError e -> None)`. This uses the
  same pattern matcher as everything else.
* A very simple abstract syntax. The nodes produced by the parser are
  identifiers (strings), `#value`, `#begin`, `#square`, `#curly`,
  `#oper` and `#juxt`. That's it.
  * However, macros may return other kinds of nodes such as
    `#declare`, `#assign` or `#splice`.



Future features
---------------

* A well-integrated macro system over the aforementioned AST.
* Expression interpolation in strings.
* Expressions evaluated in macro scope.
* Syntax to tag expressions with their location in source code.



Differences with Python
-----------------------

Superficially, they look quite similar, but there are a few drastic
differences *beyond* what I just mentioned. You might not like them,
but I do, and I actually have reasons behind these changes.

The reasons pretty much all boil down to syntactic consistency: for
instance, function calls use `[]` and not `()` simply because `f(x)`
should be equivalent to `temp = (x), f temp`, and obviously in most
current languages that is not the case. If it was, their syntax would
have at least one less rule!

In UG, `f[x]` *is* equivalent to `args = [x], f args`, `f[x, *args, y
= z, **kwargs]` *is* equivalent to `args = [x, *args, y = z,
**kwargs], f args`, `f.x` *is* equivalent to `method = .x, f method`,
and so on.


### Function calls

* Function calls use `[]` and not `()`. You must therefore write
  `open["file"]` and not `open("file")`. `open("file")` is still
  legal in UG, but it parses the same as `open "file"`, which is
  equivalent to `open.file`.
  * The `!` operator can serve as shorthand for calls sometimes: `open !
    "file"` is like `open["file"]` and `f!` is like `f[]`, but it has
    very low priority, so it's not usable everywhere.
* Indexing is the `?` operator. You must therefore write `[1, 2, 3] ?
  0` and not `[1, 2, 3][0]`.


### Declaration and assignment

* `=` declares and sets variables. `:=` assigns to them.
* Indent and (...) both delimit fresh scopes.
* To minimize confusion, I am considering not allowing the
  redeclaration of a variable declared in an outer scope using `=`. It
  would be allowed using an as of yet hypothetical `let` construct.


### The comma

* `(1, 2, 3)` is not a tuple. It evaluates to 3.
* The comma (`,`) replaces `;` in all instances you might want to
  use it.
* Since we just freed up that character, comments start with `;`, not `#`.
* The comma has lowest priority in all situations. `a, b = 1, 2`
  therefore parses as `a, (b = 1), 2`. You must write `[a, b] = [1,
  2]`.
* Every line break is equivalent to a comma. Every single one (unless
  there is a line continuation).
  * Technically, it's an "indent comma" -- the only difference with a
    standard comma is that multiple consecutive commas insert `Void`
    objects in the holes, but indent commas do not.


### Indent and whitespace

* Tabs are not legal anywhere.
* Any indented block is equivalent to appending `(` to the line right
  before the block, and `)` to the last line of the block.  That's it.
  Any code using significant indent can thus be rewritten without
  any indent.
* *Unless* the block is located within brackets. All indent is ignored
  between `()`, `[]` or `{}`, but warnings may be given if it looks
  like they should be significant. I might allow tabs there.
* *Short* juxtaposition, e.g. `a[b]`, binds tighter than *wide*
  juxtaposition, e.g. `a [b]`. `a[b] c[d]` therefore parses as
  `(a[b])(c[d])`.
* Whitespace disambiguates fixity. `a - -b` is `(a - (-b))`, but `a-
  -b` is `(a-)(-b)`.


### Data structures

* This is not done yet, but data structures will follow a more
  consistent pattern than they do in Python:
  * Ordered collections (square brackets)
    * Immutable
      * tuple: `[1, 2, 3]`
      * immutable ordered dict: `[a = 1, b = 2]` or `["a" => 1, "b" => 2]`
    * Mutable (`mut` macro)
      * list: `mut [1, 2, 3]`
      * ordered dict: `mut [a = 1, b = 2]` or `mut ["a" => 1, "b" => 2]`
  * Unordered collections (curly brackets)
    * Immutable
      * frozenset: `{1, 2, 3}`
      * immutable dict: `{a = 1, b = 2}` or `{"a" => 1, "b" => 2}`
    * Mutable (`mut` macro)
      * set: `mut {1, 2, 3}`
      * dict: `mut {a = 1, b = 2}` or `mut {"a" => 1, "b" => 2}`
* You can create "hybrid" structures that have a tuple part and
  a dictionary part: `[1, a = 2]`.


### Function declaration

* There is no `def`. All functions are declared with the `->`
  operator. I have a plan to automatically name things when assigned
  to a variable with `=`.
* Arguments are either positional or by keyword, not both, though it
  may be possible to make a patterh matcher that honors Python's
  behavior (I'd only need to add a little thing to the protocol).


Notes
-----

Python's magical `__call__`, `__getattr__`, `__setattr__`,
`__getitem__` and `__setitem__` methods are all collapsed into a
single magic method in UG: `__recv__`. Defining a function with
pattern matching essentially implements `__recv__` in a singleton.

If you install [Terminus](https://github.com/breuleux/terminus), you
can look at pretty versions of the [tokenizer's
output](http://i.imgur.com/VH6dqRE.png) as well as the
[AST](http://i.imgur.com/V6nJZi5.png) before and after the application
of the operator macros. [Syntax
errors](http://i.imgur.com/qPcugrM.png) also require that terminal to
show up nicely at the moment (that'll be fixed in due time, of
course).

