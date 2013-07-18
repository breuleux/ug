
{meta}:
  title: Unholy Grail
  author: Olivier Breuleux

{use_asset}: script/tabs.js
{js}:
    convert_all_tabdivs("tabbed");

.tabbed ..
  ..
    .. A
    .. B
    .. C
    .. D
    .. E
    .. F
  ..
    ..
      ug %
        s = "hello"
        method = .replace
        arguments = ["ll", "y"]
        print[s method arguments]
      python %
        s = "hello"
        method = "replace"
        arguments = ["ll", "y"]
        print(getattr(s, method)(*arguments))
    ..
      ug %
        Point = [x, y] ->
            .x -> x
            .y -> y
            .abs -> (x**2 + y**2)**0.5
            .__add__ -> [p] -> Point[x + p.x, y + p.y]
      python %
        class Point:
            def __init__(self, x, y):
                self.x = x
                self.y = y
            abs = property(lambda self: (self.x**2 + self.y**2)**0.5
            def __add__(self, p):
                return Point(self.x + p.x, self.y + p.y)
    ..
      ug %
        greet =
            [str person] -> print["hello", person]
            [iterable people] -> people each p -> greet[p]
      python %
        def greet(people):
            if isinstance(people, str):
                print("hello", people)
            else:
                for p in people:
                    greet(p)
    ..
      ug %
        proxy = [obj] -> arg -> (print[arg], obj arg)
      python %
        class Proxy:
            def __init__(self, obj):
                self.__obj = obj
            def __getattr__(self, attr):
                print(attr)
                return getattr(self.__obj, attr)

    ;; ..

.warning ..
  Unholy Grail is not ready for distribution yet, so consider all the
  examples and snippets I am scattering around teasers for the near
  future.



Friends, _welcome.


