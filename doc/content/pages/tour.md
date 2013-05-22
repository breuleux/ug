Title: Tour
Category: tutorial
Tags: tutorial
Slug: tour
Author: Olivier Breuleux
Summary: Quick tour




Literals and special values
===========================

    :::ug
    0, 1, 2613, 100000, 100_000          ;; Base 10 integers
    9.999, 4e10, .5, 5.                  ;; Base 10 floats
    2r1_100_100, 3r10_201, 16r64, 30r3A  ;; Other bases integers
    2r11.01, 36RZzZ.zZzZzZz              ;; Other bases floats

    "these", "are", "strings"
    .these, .are, .strings, .too

    True, False, None
    Void, ()                             ;; Void == ()




Data structures
===============

The basic data types in UG are *lists*:

    :::ug
    [], [1], ["a", "b", "c"]

And *dictionaries*:

    :::ug
    {"a" => 1}, {0 => "zero", 1 => "one"}
    ;; alternative syntax when keys are strings
    {a = 1, b = 2}
    ;; IMPORTANT: notation for empty dictionaries is as follows:
    {=}, {=>}

Both of them can be indexed using the `?` operator.

    :::ug
    xs = [1, 2, 3, 4]
    print[xs?0]        ;; 1
    xs?0 := 11         ;; xs is now [11, 2, 3, 4]

    d = {Balthazar = 27, Julie = 25}
    print["Balthazar is", d?"Balthazar", "years old."]

*Sets*:

    :::ug
    {1, 2, 3, 4}, {"set", "of", "words", "words"}
    {} ;; empty

Sets and dictionaries do not specify any iteration order. You can
however define *ordered dicts*, which do preserve the order of keys:

    :::ug
    ["a" => 1], [0 => "zero", 1 => "one"]
    [a = 1, b = 2]
    [=], [=>]

See Python's documentation for operations on these structures.


Ad hoc labeled structs
----------------------

Ever needed to pack together a few values, but did not care to make a
specific class for it? So you put them in a dictionary, or in a list,
but perhaps you lose some information in the process. UG's *ad hoc*
structs can help:

    :::ug
    p = #point[1, 2]
    print[p?0, p?1]

    p = #point[x = 1, y = 2]
    print[p.x, p.y]

`#label` defines a type named "label". These types are cached as they
are created, so each use of `#label` refers to the same type. They can
then be initiated with any sequence of arguments or keyword arguments
you like, keeping an internal vector for positional fields and an
internal dictionary for the named fields.


Frozen data structures
----------------------

In addition to all this, UG provides *immutable* versions of each
structure via the `frz` keyword (which means "frozen"):

    :::ug
    frz [1, 2]         ;; tuple (immutable list)
    frz [a = 1, b = 2] ;; immutable ordered dict
    frz {1, 2}         ;; frozenset (immutable set)
    frz {a = 1, b = 2} ;; immutable dict
    frz #point[1, 2]   ;; immutable struct



Variables and assignment
========================

Variables are declared with the **=** operator. They can be given an
initial value. However, from that moment, assignment must use the
**:=** operator. Assignment to properties or indices must also use
**:=**.

    :::ug
    x = None
    x = [1, 2, 3]  ;; NOT OK! x already exists
    x := [1, 2, 3] ;; ok
    x ? 0 = 111    ;; NOT OK!
    x ? 0 := 111   ;; ok

UG does not allow shadowing existing variables. If the variable `x`
exists in scope, then `x = y` is an error (you can however define the
same variable in multiple non overlapping scopes). (Note: if you
*really* want to do it, you can use the `let` statement).

You can declare or assign to multiple variables at once:

    ::ug
    [x, y, z] = [1, 2, 3]
    [obj.attr1, obj.attr2] := [True, False]

Mind that the priority of the comma operator is lower than that of
assignment. In fact, its priority is *dead last*. Always.

    :::ug
    x, y = 1, 2 ;; NOT OK: parses as x, (y = 1), z

The language will likely complain about `x` not being defined, so you
can just keep in mind that this might be the cause of the error.


Typing
------

You can *type* variables as you declare them.

    :::ug
    str s = 1 ;; NOT OK! 1 is not a string!
    int i = 1
    i := 80
    i := "hello" ;; NOT OK! UG remembers the type!

The type checks are **dynamic**: they are done at runtime and not at
compile time. While later optimizers may take advantage of the type
annotations, for the time being, mind that they will slow your code
down rather than speed it up. They will however help make your code
safer, more robust, and will give you nicer errors.


Indent rules
============

Like Python, UG has significant indent. However, it can be used much
more generally than in Python. Indent and line breaks obey two *very
simple* rules:

1. **Any** indented block is equivalent to putting **(** at the end of
   the line right before it, and **)** at the end of its last
   line.
     * **Unless** it is within any type of brackets. Inside `()`,
       `[]` and `{}` all indent is *ignored* (so it's even simpler).
2. There is an implicit **comma** (**,**) at the end of **every** line,
   unless it is continuated on the next line, or if the comma would come
   right after an opening bracket or right before a closing bracket.

Therefore, the following:

    :::ug
    if something:
        f[]
        g.bla["bla", "bla"]
    else:
        12345

Is equivalent to:

    :::ug
    if something: (
        f[] ,
        g.bla["bla", "bla"] ) ,
    else: (
        12345 ) ,

Using these rules, any UG code can be rewritten as a one-liner, though
doing so is not recommended.


Conditionals
============

The `if` statement looks the same in UG as it does in Python, the only
difference being that parentheses are required around the condition if
it involves an operator (comparison, `and`, etc.)

    :::ug
    if (x < 0):
        print["negative"]
    elif (x > 0):
        print["positive"]
    else:
        print["zero"]

There are a few ways to write a conditional expression:

    :::ug
    result =
        if (x < 0): "negative"
        else: "nonnegative"
    print[result]
    print[(if (x < 0): "negative", else: "nonnegative")]
    print[if[x < 0, "negative", "nonnegative"]]

Be careful, though: the first way will not work if you put `if` on the
same line as `result =`. The second way requires the parentheses,
because `if/else` must be comma-separated in a `(...)` block. The
third way will always work.


Looping
=======

While loops work like in Python. Parentheses are required around the
condition if it involves an operator (comparison, `and`, etc.)

    :::ug
    i = 10
    while (i > 0):
        print[i]
        i := i - 1
    print["Blast off!"]



Iteration
=========

There are two iteration operators: `each` and `map`. `each` executes a
function on every element of a collection, like Python's `for` loop:

    :::ug
    people = "Balthazar Peter Julie Olaf Eve Hildegarde".split[]
    people each person ->
        print["Hello,", person + "!"]

You can deconstruct the argument, and even use pattern matching on it:

    :::ug
    enumerate[people] each
        [0, person] -> print[person, "is 1st"]
        [1, person] -> print[person, "is 2nd"]
        [2, person] -> print[person, "is 3rd"]
        [n, person] when (n < 20) ->
            print[person, "is", str[i] + "th"]
        _ -> ValueError["I can't count that high!"]

`map` returns a *generator*: it does not do anything until it is asked
to. You can call the `list` function on it to get a list of results.

    :::ug
    numbers = 1 to 100
    squares = list! numbers map i -> i ** 2

Besides the fact it yields results only on demand, syntactically,
`map` works just like `each`.

The right hand side of `each` or `map` can be a function, but there is
a caveat: most functions with a single argument take a *tuple* of
length one. To illustrate:

    :::ug
    [1, 2, 3] each print       ;; FAILS: print 1, print 2, etc.
    [[1], [2], [3]] each print ;; WORKS: print[1], print[2], etc.

To solve this, you can use `wrap` as an intermediary step:

    :::ug
    [1, 2, 3] map wrap each print ;; WORKS!


Pattern matching
================

The `match` construct implements a powerful pattern matching language.
You can use it to implement a simple calculator, for instance:

    :::ug
    calc = [instruction] ->
        match instruction:
            int x -> x
            float x -> x
            #plus[x] -> +calc[x]
            #plus[x, y] -> calc[x] + calc[y]
            #minus[x] -> -calc[x]
            #minus[x, y] -> calc[x] - calc[y]
            #times[x, y] -> calc[x] * calc[y]
            #div[x, y] -> calc[x] / calc[y]
            #mod[x, y] -> calc[x] mod calc[y]
            _ -> raise TypeError["Unknown instruction", instruction]

    print ! calc[#plus[#times[5, 20], #div[77, 7]]]


Calling functions
=================

A function is called simply by putting the function and its argument
next to each other. Usually, you want to give the function a *list* of
arguments, so you just write a list next to it, using the list syntax.
Sometimes, you want to give it keyword arguments, so you write a
dictionary. And then sometimes you want to do both, which is fine too.

    :::ug
    f[]             ;; Calling f with no arguments
    f[x]            ;; Calling f with one argument
    f{a = x, b = y} ;; Calling f with two keyword arguments
    f[a = x, b = y] ;; That's fine too
    f[x, y, a = z]  ;; Calling f with two positional args and a keyword arg

But! You can also "call" a function on something that's neither a list
nor a dict. For instance, if you want to get an attribute, perhaps you
could simply call the function on the attribute's name!

    :::ug
    f "x"           ;; Calling f with a *string*
    f.x             ;; Exactly the same as the previous

    f (?0)          ;; Calling f on the *index* 0
    f?0             ;; Exactly the same as the previous

UG is quite free form. For instance, you can very well do this:

    :::ug
    obj = "hello"
    method = .replace
    arguments = ["e", "u"]
    print[obj method arguments]

In Python, the same thing would have to be written much less elegantly:

    :::python
    obj = "hello"
    method = "replace"
    arguments = ["e", "u"]
    print(getattr(obj, method)(*arguments))

Lastly, you can use the `!` operator to transform what follows into a
list. For instance:

    :::ug
    print! "hello"    ;; instead of print["hello"]
    "HELLO".lower!    ;; instead of "HELLO".lower[]

`!` has low priority and is right associative:

    :::ug
    list! reversed! sorted! [7, 3, 1, 7, 99, 12]
    ;; is equivalent to:
    list[reversed[sorted[[7, 3, 1, 7, 99, 12]]]]

If there is an indented block after `!`, it will be interpreted as a
list:

    :::ug
    print!
        "hello,"
        "friend"


Defining functions
==================

Functions are defined using the `->` operator. On the left: a
pattern. On the right: an expression. Usually the pattern will just
mirror the form of the expected arguments:

    :::ug
    add = [x, y] -> x + y
    print! add[11, 22]

Keyword arguments must be declared as such using `=>`

    :::ug
    div = [=> num, => denum] -> num / denum
    print! div[num = 1, denum = 2]

Default values for arguments with `=`

    :::ug
    increment = [x, inc = 1] -> x + inc
    print!
        increment[10]
        increment[10, 2]

It is possible to define *multiple clauses*, with one arrow per
clause. To illustrate, consider this recursive implementation of
fibonacci numbers:

    :::ug
    fib =
        [0] -> 0
        [1] -> 1
        [n] when (n > 1) ->
            fib[n - 1] + fib[n - 2]

In fact, any legal clause for `match` is a legal clause for a
function. The calculator example seen earlier can thus be rewritten
without `match`:

    :::ug
    calc =
        [int x] -> x
        [float x] -> x
        [#plus[x]] -> +calc[x]
        [#plus[x, y]] -> calc[x] + calc[y]
        [#minus[x]] -> -calc[x]
        [#minus[x, y]] -> calc[x] - calc[y]
        [#times[x, y]] -> calc[x] * calc[y]
        [#div[x, y]] -> calc[x] / calc[y]
        [_] -> raise TypeError["Unknown instruction", instruction]


Exceptions
==========

Exceptions are caught with the `!!` operator. On the left is the
expression that might throw an exception. On the right is either an
expression to evaluate if there is an error or a function.

For instance, this tries to fetch the key "Julie" from a dictionary,
but if there is no such key, it will return `None` instead:

    :::ug
    d = {Paul = "paul.txt", Jeanne = "jeanne.txt"}
    julie = d?.Julie !! None

If you want to print or manipulate the error, you can declare a
function that will be given the error as an argument:

    :::ug
    julie = d?.Julie !! e -> (print[e], None)

Perhaps more than one thing could go wrong! Just like you can define
functions with multiple clauses, you can define several clauses for
your exception handler, one for every kind of exception:

    :::ug
    open[d?.Julie] !!
        KeyError e ->
            print["Julie not found."]
        IOError e ->
            print["File not found."]

There are two kinds of special clauses you can add. The `success`
clause will be executed if there is no error, with the result of the
expression as its value (akin to the `else` clause in Python's
`try/except`). The `finally` clause will always be executed at the
end, no matter what happens, but its return value is irrelevant.

    :::ug
    ;; Prints "finally", then "abc"
    print! "a" + "b" !!
        e -> None
        success v -> v + "c"
        finally -> print["finally"]

