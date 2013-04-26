
from ..tools.unify import unify_walk, Unification, V, RestVariable
from ..parsing import parse, Source, ASTModifier, VOID
from ..lib import hashstruct as hs, ugstr

class ToPattern(ASTModifier):

    def __init__(self, *patterns):
        super().__init__()
        self.patterns = patterns
        self.variables = {}

    def visit(self, node):
        for pattern, namev, vmaker in self.patterns:
            U = unify_walk(pattern, node, Unification())
            if U:
                name = U[namev].value
                placeholder, var = vmaker(name)
                self.variables[name] = var
                return placeholder
        return super().visit(node)


class Matcher:

    def __init__(self, pattern, varmap):
        self.pattern = pattern
        self.variables = varmap

    def match(self, ast):
        if self.pattern is None:
            return False
        U = unify_walk(self.pattern, ast, Unification())
        if U:
            return {k: U[v].value for k, v in self.variables.items()}
        return False


class MatcherBuilder(Matcher):

    def __init__(self, patt_ast, var_op = "$", glom_op = "$*"):

        var_op = ugstr(var_op)
        glom_op = ugstr(glom_op)

        if patt_ast in (None, False):
            self.pattern = None
            return
            
        var_v, glom_v = V('var'), V('glom')

        pattern = hs.oper(var_op, hs.value(VOID), var_v)
        def makev(name):
            return (V(name),)*2

        v2 = V('variable')
        pattern2 = hs.oper(glom_op, hs.value(VOID), glom_v)
        def makev2(name):
            v = V(name)
            return (RestVariable(v), v)

        transformer = ToPattern([pattern, var_v, makev],
                                [pattern2, glom_v, makev2])

        super().__init__(transformer.visit(patt_ast),
                         transformer.variables)


def M(code):
    if code in (None, False):
        return Matcher(None, {})
    else:
        return MatcherBuilder(parse(Source(code)))

class PatternBank:

    def __init__(self, **patterns):
        self.patterns = {}
        for name, pattern in patterns.items():
            self.set_pattern(name, pattern)

    def set_pattern(self, name, pattern):
        m = M(pattern)
        self.patterns[name] = m

    def match(self, ast, candidates):
        for candidate in candidates:
            m = self[candidate].match(ast)
            if m:
                return (candidate, m)
        else:
            return (False, False)

    def matcher(self, *candidates):
        def match(ast):
            return self.match(ast, candidates)
        return match

    def __getitem__(self, name):
        return self.patterns[name]

    def __getattr__(self, name):
        return self.patterns[name]



