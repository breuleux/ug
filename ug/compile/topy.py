
from functools import reduce
import ast as pyast
pycompile = compile

from . import lib
from ..lib import hashstruct as hs, anonstruct, attrdict, hybrid, index, hastag, struct
from ..parsing.ug.ast import ASTVisitor, Void, transloc, getloc, hasloc
from .compile import UniqueVar, hs2


# ops = {
#     hs.special("+"): attrdict(unary = pyast.UAdd,
#                               binary = pyast.Add),
#     hs.special("-"): attrdict(unary = pyast.USub,
#                               binary = pyast.Sub),
#     hs.special("*"): attrdict(unary = None,
#                               binary = pyast.Mult),
#     hs.special("/"): attrdict(unary = None,
#                               binary = pyast.Div),

#     hs.special("<"): attrdict(unary = None,
#                               binary = pyast.Lt,
#                               compare = True),
#     hs.special(">"): attrdict(unary = None,
#                               binary = pyast.Gt,
#                               compare = True),
#     hs.special("=<"): attrdict(unary = None,
#                                binary = pyast.LtE,
#                                compare = True),
#     hs.special(">="): attrdict(unary = None,
#                                binary = pyast.GtE,
#                                compare = True),
#     hs.special("=="): attrdict(unary = None,
#                                binary = pyast.Eq,
#                                compare = True),
#     hs.special("!="): attrdict(unary = None,
#                                binary = pyast.NotEq,
#                                compare = True)

#     }


exprclasses = hs.Name, hs.Num, hs.Str, hs.Call

class UGToPy(ASTVisitor):

    def __init__(self, scope):
        self.scope = scope
        self.blocks = [[]]
        super().__init__(False)

    def add_declare(self, v):
        self.scope.declares.add(v)

    def add_assign(self, v):
        self.scope.assigns.add(v)

    def push(self):
        self.blocks.append([])

    def register(self, stmt, var, original = None):
        if original and hastag(original, "location"):
            stmt = transloc(stmt, original)
        if isinstance(stmt, exprclasses):
            if isinstance(stmt, (hs.Name, hs.Num, hs.Str)):
                self.blocks[-1].append((None, var))
                return var
            stmt = transloc(hs.Expr(stmt), stmt)
        self.blocks[-1].append((stmt, var))
        return var

    def assign_and_register(self, expr, node):
        if node and hastag(node, "location"):
            expr = transloc(expr, node)
        v = UniqueVar("temp")
        r = hs.Assign([transloc(hs.Name(str(v), hs.Store()), node)], expr)
        return self.register(r, transloc(hs.Name(str(v), hs.Load()), node), node)

    def finalize_statements(self, stmts, init, builder):
        last = init
        rval = []
        for stmt, var in stmts:
            if stmt:
                rval.append(stmt)
            last = var
        if builder is not None:
            rval.append(transloc(builder(last), last))
        return rval


    def visit_ugstr(self, node):
        r = hs.Name(str(node), hs.Load())
        return self.register(r, r, node)

    def visit_str(self, node):
        return self.visit_ugstr(node)

    def visit_UniqueVar(self, node):
        r = hs.Name(str(node), hs.Load())
        return self.register(r, r, node)

    def visit_value(self, node, value):
        if isinstance(value, (int, float)):
            r = hs.Num(value)
        elif isinstance(value, str):
            r = hs.Str(str(value))
        elif value in (None, True, False, Void):
            r = hs.Name(str(value), hs.Load())
        elif value in lib.rev_ug_library:
            r = hs.Name(lib.rev_ug_library[value], hs.Load())
        else:
            raise Exception("unknown value", value)
        return self.register(r, r, node)

    def visit_special(self, node, value):
        r = hs.Name('%%' + str(value), hs.Load())
        return self.register(r, r, node)

    def visit_tuple(self, node, *args):
        newargs = list(map(self.visit, args))
        c = hs.Call(transloc(hs.Name('%%tuple', hs.Load()), node),
                       newargs,
                       [],
                       None,
                       None)
        return self.assign_and_register(c, node)

    def visit_send(self, node, obj, msg):

        if False and isinstance(obj, (hs.special, hs.value)) and obj in ops:
            spec = ops[obj]
            if isinstance(msg, hs.tuple):
                left, right = msg[:]
                if left == hs.value(Void):
                    r = hs.UnaryOp(spec.unary(), self.visit(right))
                else:
                    if getattr(spec, 'compare', False):
                        r = hs.Compare(self.visit(left),
                                             [spec.binary()],
                                             [self.visit(right)])
                    else:
                        r = hs.BinOp(self.visit(left),
                                           spec.binary(),
                                           self.visit(right))
            else:
                raise Exception("Sending to strange thing:", node)
        elif isinstance(msg, hs.tuple):
            r = hs.Call(self.visit(obj),
                           list(map(self.visit, msg[:])),
                           [],
                           None,
                           None)
        else:
            r = hs.Call(hs.Name('send', hs.Load()),
                           [self.visit(obj), self.visit(msg)],
                           [],
                           None,
                           None)

        return self.assign_and_register(r, node)

    def visit_begin(self, node, *stmts):
        rval = hs.Name("None", hs.Load())
        for stmt in stmts:
            rval = self.visit(stmt)
        return rval

    def visit_declaring(self, node, variables, body):
        for v in variables:
            self.add_declare(v)
        return self.visit(body)

    def visit_assign(self, node, var, value):
        self.add_assign(var)
        pyvar = transloc(hs.Name(str(var), hs.Load()), var)
        stmt = transloc(hs.Assign([transloc(hs.Name(str(var), hs.Store()), var)],
                                     self.visit(value)), node)
        return self.register(stmt, pyvar, node)

    def visit_if(self, node, test, ift, iff):
        v = transloc(UniqueVar("if_result"), node)
        test = self.visit(test)
        _if = UGToPyIf(test, v, self.scope)
        _if.visit(ift)
        _if.push()
        _if.visit(iff)
        c = _if.create()
        return self.register(c, transloc(hs.Name(str(v), hs.Load()), v), node)

    def visit_catch(self, node, expr, handler):
        v = transloc(UniqueVar("catch_result"), node)
        _catch = UGToPyCatch(v, self.scope)
        _catch.visit(expr)
        _catch.push()
        _catch.visit(handler)
        c = _catch.create()
        return self.register(c, transloc(hs.Name(str(v), hs.Load()), v), node)

    def visit_return(self, node, v):
        r = hs.Return(self.visit(v))
        return self.register(r, hs.Name("unreachable", hs.Load()), node)

    def visit_lambda(self, node, arguments, body):
        name = transloc(UniqueVar("τ"), node)
        args = [hs.arg(str(arg), None) for arg in arguments]
        r = UGToPyDef(str(name),
                      hs.arguments(args,
                                      None, None, [], None, None, [], []))
        r.visit(body)
        r = r.create()
        self.register(r, hs.Name(str(name), hs.Load()), node)
        return self.visit(hs2.declaring([name], name))

    def visit_generic(self, node):
        raise Exception("Unknown node", node)



class UGToPyModule(UGToPy):

    def __init__(self):
        self.declares = set()
        self.assigns = set()
        super().__init__(self)

    def create(self):
        statements, = self.blocks[:]
        statements = self.finalize_statements(statements, None, None)
        m = hs.Module(statements)
        return m




class UGToPyDef(UGToPy):

    def __init__(self, name, args = None):
        self.name = name
        self.args = args or hs.arguments([], None, None, [], None, None, [], [])
        self.declares = set()
        self.assigns = set()
        super().__init__(self)

    def create(self):
        statements = [transloc(hs.Nonlocal([str(x)]), x)
                      for x in self.assigns if x not in self.declares]
        _statements, = self.blocks[:]

        statements += self.finalize_statements(_statements,
                                               hs.Name("None", hs.Load()),
                                               hs.Return)

        f = hs.FunctionDef(
            self.name,
            self.args,
            statements,
            [],
            None
            )

        return f



class UGToPyIf(UGToPy):

    def __init__(self, test, var, scope):
        super().__init__(scope)
        self.test = test
        self.var = var

    def create(self):
        tstmts, fstmts = self.blocks

        var = transloc(hs.Name(str(self.var), hs.Store()), self.var)

        tstmts = self.finalize_statements(tstmts, None, lambda x: hs.Assign([var], x))
        fstmts = self.finalize_statements(fstmts, None, lambda x: hs.Assign([var], x))

        return hs.If(self.test, tstmts, fstmts)



class UGToPyCatch(UGToPy):

    def __init__(self, var, scope):
        super().__init__(scope)
        self.var = var

    def create(self):
        tstmts, estmts = self.blocks

        var = transloc(hs.Name(str(self.var), hs.Store()), self.var)

        tstmts = self.finalize_statements(tstmts, None, lambda x: hs.Assign([var], x))
        estmts = self.finalize_statements(estmts, None, lambda x: hs.Assign([var], x))

        return hs.TryExcept(
                tstmts,
                [hs.ExceptHandler(None,
                                     None,
                                     estmts)],
                [])


def convert_to_py_ast(ast):
    """
    Convert the AST to Python's own AST objects and translate
    the locations represented by Location objects into Python's
    lineno and col_offset fields.
    """
    if isinstance(ast, struct):
        arguments = list(map(convert_to_py_ast, ast[:]))
        node = getattr(pyast, type(ast).__name__)(*arguments)
        if hasloc(ast):
            loc = getloc(ast)
            if loc.source:
                (line, col), end = getloc(ast).linecol()
                node.lineno = line
                node.col_offset = col
        return node
    elif isinstance(ast, (list, tuple)):
        return list(map(convert_to_py_ast, ast[:]))
    else:
        return ast



def evaluate(ast, source = None):

    ctor = UGToPyDef("F")

    ctor.visit(ast)
    py = ctor.create()
    py = convert_to_py_ast(py)

    if isinstance(py, pyast.expr):
        py = pyast.Expression(py)
    elif isinstance(py, pyast.stmt):
        py = pyast.Module([py])
    py = pyast.fix_missing_locations(py)

    # code = compile(py, source and source.url or "<string>", 'exec')
    code = compile(py, "<string>", 'exec')

    d = dict(lib.ug_library)

    exec(code, d)
    return d['F']()


def pprint(node, offset = 0):
    
    if isinstance(node, pyast.AST):
        print(" " * offset,
              type(node).__name__,
              "(%s,%s)" % (getattr(node, 'lineno', 'n/a'), getattr(node, 'col_offset', 'n/a')),
              ";".join("%s=%s" % (field, getattr(node, field)) for field in node._fields))
    else:
        raise Exception("Not an AST node", node)

    for child in pyast.iter_child_nodes(node):
        pprint(child, offset + 4)



# from descr import boxy_terminus
# pr = boxy_terminus()
# import ast
# from descr.registry import types_registry
# from descr.html import html_boxy, HTMLRuleBuilder

# class ASTDescriber(ast.NodeVisitor):

#     def __init__(self, recurse):
#         self.recurse = recurse

#     # def generic_visit(self, node):
#     #     if not isinstance(node, ast.AST):
#     #         return self.recurse(node)
#     #     name = type(node).__name__
#     #     classes = {"@ast.AST", "@AST."+name, "object", "+"+name}
#     #     results = [classes]
#     #     for fieldname, child in ast.iter_fields(node):
#     #         results.append(({"field", "+" + fieldname}, self.visit(child)))
#     #     return results

#     def generic_visit(self, node):
#         # if isinstance(node, (int, str, ast.Load, ast.Store)):
#         #     return []
#         if not isinstance(node, ast.AST):
#             return self.recurse(node)
#         name = type(node).__name__
#         results = [{"@list", "sequence"},
#                    name,
#                    self.recurse(getattr(node, 'lineno', None)),
#                    #self.recurse(getattr(node, 'col_offset', None))
#                    ]
#         for fieldname, child in ast.iter_fields(node):
#             results.append(self.visit(child))
#         return results

# def describe_ast_node(node, recurse):
#     return ASTDescriber(recurse).visit(node)


# def setup():
#     types_registry[ast.AST] = describe_ast_node

# setup()
