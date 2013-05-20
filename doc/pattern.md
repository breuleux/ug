
Pattern matching
================

As the name aptly indicates, "pattern matching" is the process of
matching up a pattern with some value. The match can succeed, or it
can fail.

If a match succeeds, the matching value is put in a variable, where it
can be accessed. If it fails, then other patterns may be tried.

Pattern matching is of tremendous help if you have complex data
structures and you want to *extract* data from them. Consider the
following, for a taste:

    line = #line[#point[0, 0], #point[10, 10]]
    ;; Magic happens here:
    #line[#point[int x1, int y1], #point[int x2, int y2]] = line
    print["The line's length is", ((x1-x2)**2 + (y1-y2)**2)**0.5]

UG will do everything for you: it will test that `line` really is an
instance of `#line`, that it contains two `#point` structures, and
that these each contain two *integer* coordinates (note that typing is
optional). If all is well, then it will assign the values to the
variables.

It is also possible to use pattern matching as you would use
conditionals. This is invaluable if you want to have different
behavior depending on the structure of a value or argument and using a
series of `if` statements would be tedious or awkward.

For instance, imagine you want to define a `div` function that takes
either one or two arguments. If it gets one, it computes `1 /
argument`, otherwise it computes `argument1 / argument2`. You can do
this easily in UG:

    div =
        [nonzero argument] ->
            1 / argument
        [argument1, nonzero argument2] ->
            argument1 / argument2

    print["1/2 is", div[2], "and 3/2 is", div[3, 2]]

Here you have two patterns: `[nonzero argument]` and `[argument1,
nonzero argument2]`. Note how the use of `nonzero` will prevent
division from zero from occurring!

Each is associated, via the `->` operator, to an expression (which can
be a block). The expression associated to the *first* matching pattern
is the one that's executed.

The pattern matcher is then assigned to the `div` variable. You can
see that it is used in a very straightforward manner.



What is a pattern?
------------------


**Matching anything**

If the pattern is a plain identifier, it will match anything.

    x = y

Note: the identifier `_` is special, in the sense that it matches
anything, but discards the result afterwards. Use it as a placeholder
for any parts of a pattern you don't care about.


**Matching precise values**

You can use integers, floats or strings as patterns.

    0 = 1 ;; failure
    ["hello", name] = ["hello", "Peter"] ;; success


**Type check**

If the pattern is a type next to an identifier, it checks that the
value to match has the right type.

    int x = 1 ;; success!
    str x = 1 ;; TypeError


**Guard**

A guard is an expression that returns True or False. It is evaluated
*after* all the other matching and it has access to the values of all
variables declared by the pattern. If it evaluates to False, then the
match fails.

A guard is declared with the `when` operator.

    x when x.startswith["hello"] = "hello everyone" ;; success
    [x, y, z] when (x > 0) = [-7, -8, -9] ;; GuardError


**Matching a list**

    [x, y] = [1, 2]

If a `*variable` is found it is set to the remainder of the contents
of the list. It will match lists of arbitrary lengths.

    [x, *rest] = [1, 2, 3, 4] ;; rest = [2, 3, 4]
    [x, *rest, y] = [1, 2, 3, 4] ;; rest = [2, 3]

Variables may also be given *default values*. If the list to match is
too short, variables will match with their default values if they have
one.

    [x, y = None] = [1, 2] ;; x = 1, y = 2
    [x, y = None] = [1]    ;; x = 1, y = None


**Matching a dict**

Through pattern matching, it is possible to extract values from a
dictionary by key.

    [=> a] = ["a" => 1] ;; a = ["a" => 1] ? "a" = 1
    [=> b] = ["a" => 1] ;; failure: there is no key named "b"

The variable's name does not have to be the same as the key. The
pattern `=> a` is shorthand for `a => a`

    [a => x] = ["a" => 1] ;; x = ["a" => 1] ? "a" = 1
    [a => [x, y]] = ["a" => [1, 2]] ;; x = 1, y = 2

You can use `**variable` to mop up any extra keys.

    [=> a] = ["a" => 1, "b" => 2] ;; failure because the key "b" is not matched
    [=> a, **rest] = ["a" => 1, "b" => 2] ;; rest = ["b" => 2]

A keyword pattern can also be associated to a default value, which it
will match if there is nothing else.

    [=> a = None] = [1] ;; a = 1
    [=> a = None] = [] ;; a = None


**Transformers**

A "transformer" is similar to a Python decorator and the syntax is
also similar. However, they are part of the pattern matching syntax
and their application is unrestricted. Essentially, a transformer is a
function that takes some object as argument and wraps it or returns a
replacement.

For instance, let's say you want to set x to the absolute value of
y. You could do this:

    x = abs[y]

Or you could do this:

    @abs x = y

The latter way is quite useful in at least two situations. First, it
is useful if y is buried in some data structure. Compare:

    #point[x, y] = #point[1, -2]
    x := abs[x]
    y := abs[y]

    #point[@abs x, @abs y] = #point[1, -2]

Second, it is useful to wrap functions. Compare:

    f = classmethod[[cls] -> ...]

    @classmethod f = [cls] -> ...


**Deconstructors**

When a type is put before an identifier, it checks the value's type
before assigning it to the variable. If a type is put before a list or
dict pattern, however, the behavior is different.

    x [y, z] = ...

Is essentially equivalent to

    [y, z] = x.__deconstruct__[...]

That is to say, the type needs to implement a `__deconstruct__` method
which returns a list, dict or hybrid, which will be matched normally.

Likewise, what we had in an earlier example:

    #line[#point[int x1, int y1], #point[int x2, int y2]] = ...

Is equivalent to

    [#point[int x1, int y1], #point[int x2, int y2]] = #line.__deconstruct__[...]

And so on, recursively. So you can figure that this does not work:

    [x, y] = #point[1, 2] ;; TypeError

This does, though, because `struct` knows how to deconstruct instances
of `#point`:

    struct [x, y] = #point[1, 2]

You can of course define your own deconstructors. It only needs a
`__deconstruct__` method that takes one argument and returns a list,
dict or hybrid, and then you can use it like any other.




Where to use
------------

Here is a relatively complete account of all the situations where the
pattern matching engine can be used.


**Declaring variables**

    [x, y] = [1, 2]
    do_stuff_with[x, y]


*Declaring functions*

    add = [x, y] -> x + y
    print[add[1, 2]]


*match statement*

    match [1, 2, 3]:
        [] -> print["empty list"]
        [x] -> print["length is one"]
        [x, y] -> print["length is two"]
        [x, y, z] -> print["length is three"]


*Looping*

    (1 to 10) each i -> print[i]

    enumerate[["milk", "eggs", "bananas"]] each
        [0, food] -> print[food]
        [i, food] -> print[",", food]
    ;; ==> milk , eggs , bananas


*Exception handling*

    1 / 0 !!
        ZeroDivisionError e ->
            print["ZeroDivisionError:", e]


**Assignment**

Only **limited** pattern matching is allowed here. Types or
deconstructors are *not allowed* in the patterns on the left hand side
of `:=`: if present, they will be interpreted differently.

    d = #point[x = 1, y = 2]
    [d.x, d.y] := [3, 4] ;; d is now #point[x = 3, y = 4]
