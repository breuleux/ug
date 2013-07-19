
{css}:
  .cmptable td, .cmptable th {width: 250px;}
  .cmptable td {padding: 5px;}
  .noequiv {color: #888;}
  .code {font-family: monospace; white-space: pre;}
  .dstable tr:first-child, .dstable td:first-child {
    background-color: #ccf;
    font-weight: bold;
  }
  .dstable tr:first-child {
    border-bottom: 2px solid black;
  }
  .dstable td:first-child {
    border-right: 2px solid black;
  }
  .dstable td {
    width: 250px;
    border-bottom: 1px solid black;
  }


{
  def wrap_code(lang, code):
    if code == "/nada":
      return wrap("n/a", classes = "noequiv")
    else:
      return wrap(Raw(codehl(lang, code)), "div", classes = "code")

  def header(s):
    r = Text(s)
    r.colspan = 3
    return r

  def make_table(table):
    table_entries = []
    for key, comparisons in sorted(table.items()):
      table_entries.append(TableHeader(header(key[1:])))
      table_entries.append(TableHeader("", "Python", "Unholy Grail"))
      for py, ug, comment in comparisons:
        if not isinstance(py, (list, tuple)):
          py = [py]
        pys = Gen(*[wrap_code("python", x) for x in py])
        if not isinstance(ug, (list, tuple)):
          ug = [ug]
        ugs = Gen(*[wrap_code("ug", x) for x in ug])
        table_entries.append((comment, pys, ugs))
    return Table(*table_entries)
}

Numbers and literals
====================

{yaml}:
  scalar_table:
    0Numerals:
      - ["1000", ["1000", "1_000"], "Base 10"]
      - ["1.234e56", "1.234e56", "Scientific notation"]
      - ["0xDEADBEEF", "16rDEADBEEF", "Base 16"]
      - ["0123", "8r123", "Base 8"]
      - ["0b111", "2r111", "Base 2"]
      - [/nada, "2r111.111", "Base 2 (floating point)"]
      - [/nada, "36rCOOL_NUMBER", "Base 36"]
    1Strings:
      - [["\"hello\"", "'hello'"], ["\"hello\"", .hello], "String"]
      - ['"a\"b"', "\"a\"\"b\"", "Escaping \""]

.cmptable ..
  {make_table(scalar_table)}


Data structures
===============

{yaml}:
  structure_table:
    0Lists:
      - ["[1, 2, 3]", ["[1, 2, 3]", "!(1, 2, 3)"], ""]
      - ["[1]", ["[1]", "!1"], "One element"]
      - ["[]", "[]", "Empty"]
    1Tuples:
      - [["(1, 2, 3)", "1, 2, 3"], ["frz [1, 2, 3]"], ""]
      - ["(1,)", ["frz [1]"], "One element"]
      - ["()", ["frz []"], "Empty"]
    2Sets:
      - ["{1, 2, 3}", "{1, 2, 3}", ""]
      - ["set()", "{}", "Empty"]
      - ["frozenset({1, 2, 3})", "frz {1, 2, 3}", "Immutable"]
    3Dictionaries:
      - ['{"a": 1, "b": 2}', ['{"a" => 1, "b" => 2}', '{a = 1, b = 2}'], ""]
      - ["{}", ["{=}", "{=>}"], "Empty"]
      - - |
          from collections import OrderedDict
          OrderedDict((("a", 1), ("b", 2)))
        - "[a = 1, b = 2]"
        - "Ordered"
    "6Frozen dictionaries":
      - [/nada, "frz {a = 1, b = 2}", ""]
      - [/nada, "frz [a = 1, b = 2]", "Ordered"]
    "7Structs":
      - [/nada, ["#abc[1, 2, 3]", "#point[x = 1, y = 2]"], ""]
      - [/nada, ["frz #abc[1, 2, 3]", "frz #point[x = 1, y = 2]"], "Immutable"]

.cmptable ..
  {make_table(structure_table)}

br ..

In order to better highlight the differences, here are the same data
structures, but organized with respect to whether they are ordered or
unordered, collections or associations, mutable or immutable:


=== Python

{yaml}:
  py_table:
    - ["", Collection, Associative]
    - [Ordered, "[1, 2]\n(1, 2)", "OrderedDict(((a, 1), (b, 2)))\nn/a"]
    - [Unordered, "{1, 2}\nfrozenset({1, 2})", "{a: 1, b: 2}\nn/a"]

{
  py_table = [py_table[0]] + [[a, wrap_code("python", b), wrap_code("python", c)]
                              for a, b, c in py_table[1:]]
}

.dstable ..
  {Table(*py_table)}


=== Unholy Grail

{
  results = [["", "Collection", "Associative"]]
  for open, close in ["[]", "{}"]:
    results.append(["Ordered" if open == "[" else "Unordered"])
    for assoc in [False, True]:
      inner = [('{} => {}'.format(a, x) if assoc else str(x))
               for a, x in [["a", 1], ["b", 2]]]
      s = open + ", ".join(inner) + close
      s = "    {s}\nfrz {s}".format(s = s)
      results[-1].append(wrap_code("ug", s))
}

.dstable ..
  {Table(*results)}



Operations
==========

{yaml}:
  operator_table:
    0Function calls:
      - [["f()", "", "f(x)", "", "f(x, y)"], ["f[]", "f!", "f[x]", "f!x", "f[x, y]"], ""]
      - ["f(x, y = 2)", "f[x, y = 2]", "Keyword arguments"]
      - ["f(*args)", ["f[*args]", "f args"], Rest argument]
      - ["f(**kwargs)", ["f[**kwargs]", "f{**kwargs}", "f kwargs"], Keyword rest argument]
    1Attributes:
      - ["a.x", ["a.x", 'a "x"'], ""]
      - ["getattr(a, attr)", "a attr", "By name"]    
    2Indexing:
      - ["a[x]", "a?x", ""]
      - ["a[x:y]", "a?(x..y)", "Slice"]
      - - ["a[x:]", "a[:y]", "a[:]"]
        - ["a?(x..)", "a?(..y)", "a?(..)"]
        - Open slice
      - ["a[x, y]", "a?[x, y]", "Multiple indices"]
    3Arithmetic:
      - [a + b, a + b, Addition]
      - [a * b, a * b, Multiplication]
      - [a / b, a / b, Division]
      - [a ** b, a ** b, Power]
      - [a % b, a mod b, Modulo]

.cmptable ..
  {make_table(operator_table)}


Control structures
==================

{yaml}:
  cs_table:
    0Conditionals:
      - - |
          if x < y:
              do_something
          elif x > y:
              do_something_else
              daydream_a_bit
          else:
              panic_utterly
        - |
          if (x < y):
              do_something
          elif (x > y):
              do_something_else
              daydream_a_bit
          else:
              panic_utterly
        - if
      - - "x if y else z"
        - "if[x, y, z]"
        - Inline if
      - - |
          while x < y:
              run_around
              y += 1
        - |
          while (x < y):
              run_around
              y += 1
        - while
    1Loops:
      - - |
          for i, x in enumerate(things):
              do_something
              work_hard
        - |
          things each [i, x] ->
              do_something
              work_hard
        - "for"
      - - - "f(x, y) for x in things"
          - "[f(x, y) for x in things]"
          - "{x: x * x for x in things}"
        - - "things map x -> f[x, y]"
          - "list! things map x -> f[x, y]"
          - "dict! things map x -> x => x * x"
        - "Comprehensions"
    2Functions:
      - - |
          def f(x, y):
              print(x)
              return x + y
        - |
          f = [x, y] ->
              print[x]
              x + y
        - Definitions
      - ["lambda x: f(x) + g(y)", "[x] -> f[x] + g[y]", Lambda]

.cmptable ..
  {make_table(cs_table)}



