
from functools import reduce
from collections import OrderedDict
import ast as pyast
pycompile = compile

from . import lib
from ..lib import (hashstruct as hs, anonstruct, attrdict, hybrid, index,
                   hastag, struct, tag, gettag)
from ..parsing import parse, Source
from ..parsing.ug.ast import ASTVisitor, Void, transloc, transfer, getloc, hasloc, Char
from .compile import UniqueVar, hs2
from .main import compile as ugcompile
from .macros import macro


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
        self.values = {}
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
            else:
                rval.append(hs.Expr(var))
            last = var
        if builder is not None:
            rval.append(transloc(builder(last), last))
        # print("ok", rval)
        return rval

    def visit(self, node, name):

        if hastag(node, "tag_location"):
            thetag = gettag(node, "tag_location")
            if thetag:
                copy = tag(transfer(type(node)(*node[:]), node),
                           "tag_location",
                           False)
                node = hs.send(hs.value(lib.maytag),
                               hs.seq(copy,
                                        hs.value("location"),
                                        hs.value(thetag)))

        if hastag(node, 'name'):
            name2 = gettag(node, 'name')
            if name2 is not None:
                if name is not None:
                    name = name + ':' + name2
                else:
                    name = name2
                tag(node, 'name', None)

        if hastag(node, "set_name"):
            if gettag(node, "set_name"):
                copy = tag(transfer(type(node)(*node[:]), node),
                           "set_name",
                           False)
                node = hs.send(hs.value(lib.setattribute),
                               hs.seq(copy,
                                        hs.value("__name__"),
                                        hs.value(name or "?")))

        result = super().visit(node, name = name)
        return result

    def visit_ugstr(self, node, name):
        r = hs.Name(str(node), hs.Load())
        return self.register(r, r, node)

    def visit_str(self, node, name):
        return self.visit_ugstr(node, name = name)

    def visit_UniqueVar(self, node, name):
        r = hs.Name(str(node), hs.Load())
        return self.register(r, r, node)

    def visit_value(self, node, value, name):
        if isinstance(value, (int, float)):
            r = hs.Num(value)
        elif isinstance(value, str):
            r = hs.Str(str(value))
        elif value in (None, True, False, Void):
            r = hs.Name(str(value), hs.Load())
        else:
            try:
                if value in lib.rev_ug_library:
                    r = hs.Name(lib.rev_ug_library[value], hs.Load())
                else:
                    r = None
            except TypeError:
                r = None

            if r is None:
                v = transloc(UniqueVar("v"), node)
                self.values[str(v)] = value
                r = hs.Name(str(v), hs.Load())
                return self.register(r, r, node)
        # else:
        #     raise Exception("unknown value", value)
        return self.register(r, r, node)

    # def visit_special(self, node, value, name):
    #     r = hs.Name('%%' + str(value), hs.Load())
    #     return self.register(r, r, node)

    def visit_seq(self, node, *args, name = None):
        newargs = [self.visit(arg, name) for arg in args]
        c = hs.Call(transloc(hs.Name('%%list', hs.Load()), node),
                       newargs,
                       [],
                       None,
                       None)
        return self.assign_and_register(c, node)

    def visit_pair(self, node, x, y, name = None):
        return self.visit(hs.send(hs.value(lib.pair), hs.seq(x, y)), name = name)

    def visit_send(self, node, obj, msg, name):

        if False and isinstance(obj, (hs.special, hs.value)) and obj in ops:
            spec = ops[obj]
            if isinstance(msg, hs.seq):
                left, right = msg[:]
                if left == hs.value(Void):
                    r = hs.UnaryOp(spec.unary(), self.visit(right, name))
                else:
                    if getattr(spec, 'compare', False):
                        r = hs.Compare(self.visit(left, name),
                                             [spec.binary()],
                                             [self.visit(right, name)])
                    else:
                        r = hs.BinOp(self.visit(left, name),
                                           spec.binary(),
                                           self.visit(right, name))
            else:
                raise Exception("Sending to strange thing:", node)
        elif isinstance(msg, hs.seq):
            r = hs.Call(self.visit(obj, name),
                           [self.visit(m, name) for m in msg[:]],
                           [],
                           None,
                           None)
        else:
            r = hs.Call(hs.Name('send', hs.Load()),
                           [self.visit(obj, name), self.visit(msg, name)],
                           [],
                           None,
                           None)

        return self.assign_and_register(r, node)

    def visit_begin(self, node, *stmts, name = None):
        rval = hs.Name("None", hs.Load())
        for stmt in stmts:
            rval = self.visit(stmt, name)
        return rval

    def visit_declaring(self, node, variables, body, name):
        for v in variables:
            self.add_declare(v)
        return self.visit(body, name)

    def visit_assign(self, node, var, value, name):
        self.add_assign(var)
        pyvar = transloc(hs.Name(str(var), hs.Load()), var)
        stmt = transloc(hs.Assign([transloc(hs.Name(str(var), hs.Store()), var)],
                                     self.visit(value, name)), node)
        return self.register(stmt, pyvar, node)

    def visit_if(self, node, test, ift, iff, name):
        v = transloc(UniqueVar("if_result"), node)
        test = self.visit(test, name)
        _if = UGToPyIf(test, v, self.scope)
        _if.visit(ift, name)
        _if.push()
        _if.visit(iff, name)
        c = _if.create()
        self.values.update(_if.values)
        return self.register(c, transloc(hs.Name(str(v), hs.Load()), v), node)

    def visit_catch(self, node, expr, handler, name):
        v = transloc(UniqueVar("catch_result"), node)
        _catch = UGToPyCatch(v, self.scope)
        _catch.visit(expr, name)
        _catch.push()
        _catch.visit(handler, name)
        c = _catch.create()
        self.values.update(_catch.values)
        return self.register(c, transloc(hs.Name(str(v), hs.Load()), v), node)

    def visit_return(self, node, v, name):
        r = hs.Return(self.visit(v, name))
        return self.register(r, hs.Name("unreachable", hs.Load()), node)

    def visit_lambda(self, node, arguments, body, name):
        # name = transloc(UniqueVar("τ"), node)
        if name is None:
            name = "<lambda>"
        args = [hs.arg(str(arg), None) for arg in arguments]
        r = UGToPyDef(str(name),
                      hs.arguments(args,
                                      None, None, [], None, None, [], []))
        r.visit(body, name + "->")
        fn = r.create()
        self.values.update(r.values)
        self.register(fn, hs.Name(str(name), hs.Load()), node)
        temp = UniqueVar("temp")
        return self.visit(hs2.declaring([name, temp],
                                        hs2.begin(hs2.assign(temp, name),
                                                  temp)),
                          name)

    def visit_generic(self, node, name):
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



class UGToPyStatements(UGToPy):

    def __init__(self):
        self.declares = set()
        self.assigns = set()
        super().__init__(self)

    def create(self):
        statements, = self.blocks[:]
        statements = self.finalize_statements(statements,
                                              None,
                                              lambda x: x)
        return statements




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


def order_monotonic(nodes):
    minl, minc = 0, 0
    candidates = list(nodes)
    while candidates:
        node = candidates.pop(0)
        if isinstance(node, (list, tuple)):
            candidates = list(node) + candidates
            continue
        if isinstance(node, pyast.AST):
            if hasattr(node, 'lineno'):
                if node.lineno < minl:
                    node.lineno = minl
                if node.lineno == minl and node.col_offset < minc:
                    node.col_offset = minc
                else:
                    minl, minc = node.lineno, node.col_offset
            else:
                node.lineno = minl
                node.col_offset = minc
            candidates = [getattr(node, f)
                          for f in node._fields] + candidates


def convert_to_py_ast(ast):
    """
    Convert the AST to Python's own AST objects and translate
    the locations represented by Location objects into Python's
    lineno and col_offset fields.
    """
    if isinstance(ast, struct):
        arguments = list(map(convert_to_py_ast, ast[:]))
        # order_monotonic(arguments)
        node = getattr(pyast, type(ast).__name__)(*arguments)
        if hasloc(ast):
            loc = getloc(ast)
            if loc.source:
                (line, col), end = getloc(ast).linecol()
                node.lineno = line
                node.col_offset = col
        return node
    elif isinstance(ast, (list, tuple)):
        results = list(map(convert_to_py_ast, ast[:]))
        # order_monotonic(results)
        return results
    else:
        return ast



# def evaluate(ast, source = None, d = None):

#     ctor = UGToPyDef("F")

#     ctor.visit(ast, None)
#     py = ctor.create()
#     py = convert_to_py_ast(py)

#     if isinstance(py, pyast.expr):
#         py = pyast.Expression(py)
#     elif isinstance(py, pyast.stmt):
#         py = pyast.Module([py])


#     order_monotonic([py])
#     py = pyast.fix_missing_locations(py)
#     # pr(py)


#     code = compile(py, source and source.url or "<string>", 'exec')
#     # code = compile(py, "<string>", 'exec')

#     d = d or {}
#     d.update(lib.ug_library)
#     d.update(ctor.values)

#     exec(code, d)
#     return d['F']()


def evaluate(ast, source = None, d = None):

    ctor = UGToPyStatements()
    ctor.visit(ast, None)

    *spy, epy = ctor.create()
    spy = convert_to_py_ast(hs.Module(spy))
    epy = convert_to_py_ast(hs.Expression(epy))

    # pprint(py)

    # if isinstance(py, pyast.expr):
    #     py = pyast.Expression(py)
    # elif isinstance(py, pyast.stmt):
    #     py = pyast.Module([py])


    order_monotonic([spy, epy])
    spy = pyast.fix_missing_locations(spy)
    epy = pyast.fix_missing_locations(epy)
    # pr(py)


    code1 = compile(spy, source and source.url or "<string>", 'exec')
    code2 = compile(epy, source and source.url or "<string>", 'eval')

    # code = compile(py, "<string>", 'exec')

    if d is None:
        d = {}
    d.update(lib.ug_library)
    d.update(ctor.values)

    exec(code1, d)
    return eval(code2, d)
    # return d['F']()


def ugeval(x):
    if isinstance(x, str):
        src = Source(x, url = None)
        ast = parse(src)
    else:
        src = None
        ast = x
    ast2 = ugcompile(ast)
    return evaluate(ast2, source = src)

def ugeval_direct(x):
    return evaluate(x)

lib.ug_library['Char'] = Char
lib.ug_library['symbol'] = lib.symbol
lib.ug_library['delay'] = lib.delay
lib.ug_library['Source'] = Source
lib.ug_library['UniqueVar'] = UniqueVar
lib.ug_library['ugparse'] = parse
lib.ug_library['ugcompile'] = ugcompile
lib.ug_library['ugeval'] = ugeval
lib.ug_library['ugeval_direct'] = ugeval_direct
lib.ug_library['addmacro'] = lambda name, f: macro(name)(f)
lib.ug_library['dynlet'] = lib.dynlet
lib.ug_library['ugtranslations'] = {
    None: '#f',
    True: '#t',
    False: '#f',
    Void: '__hole',

    lib.symbol: '__symbol',
    lib.pair: 'cons',
    lib.delay: 'lazy',

    lib.patch_list: '__patch_vector',

    OrderedDict: '__assoc',

    set: '__set',

    dict: '__table',
    lib.patch_dict: '__patch_table',

    hybrid: '__hybrid',

    hs: 'Struct',
    hs.index: 'vector-immutable',

    lib.Deconstructor: 'spec->deconstructor',
    lib.make_object: '__make_object',

    lib.struct_project: 'p-project',
    lib.struct_deconstruct: 'p-deconstruct',
    lib.struct_star: 'p-star',
    lib.struct_dstar: 'p-dstar',
    lib.struct_assoc: 'p-assoc',
    lib.struct_default: 'p-default',

    lib.trycatch: '__catch',
    lib.escape: '__escape',

    lib.check_equal: '__check_equal',
    lib.ugwhile: '__while',

    lib.raiser: '__raise',
    lib.ContinueException: '__continue',
    lib.BreakException: '__break',
}



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



from descr import boxy_terminus
pr = boxy_terminus()
import ast
from descr.registry import types_registry
from descr.html import html_boxy, HTMLRuleBuilder

class ASTDescriber(ast.NodeVisitor):

    def __init__(self, recurse):
        self.recurse = recurse

    # def generic_visit(self, node):
    #     if not isinstance(node, ast.AST):
    #         return self.recurse(node)
    #     name = type(node).__name__
    #     classes = {"@ast.AST", "@AST."+name, "object", "+"+name}
    #     results = [classes]
    #     for fieldname, child in ast.iter_fields(node):
    #         results.append(({"field", "+" + fieldname}, self.visit(child)))
    #     return results

    def generic_visit(self, node):
        # if isinstance(node, (int, str, ast.Load, ast.Store)):
        #     return []
        if not isinstance(node, ast.AST):
            return self.recurse(node)
        name = type(node).__name__
        results = [{"@list", "sequence"},
                   name,
                   self.recurse(getattr(node, 'lineno', None)),
                   #self.recurse(getattr(node, 'col_offset', None))
                   ]
        for fieldname, child in ast.iter_fields(node):
            results.append(self.visit(child))
        return results

def describe_ast_node(node, recurse):
    return ASTDescriber(recurse).visit(node)


def setup():
    types_registry[ast.AST] = describe_ast_node

setup()
