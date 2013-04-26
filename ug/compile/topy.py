
from functools import reduce
import ast as pyast
pycompile = compile

from ..lib import hashstruct as hs, anonstruct, attrdict, hybrid, index, hastag
from ..parsing.ug.ast import ASTVisitor, VOID, transloc
from .compile import UniqueVar, hs2


ops = {
    hs.special("+"): attrdict(unary = pyast.UAdd,
                              binary = pyast.Add),
    hs.special("-"): attrdict(unary = pyast.USub,
                              binary = pyast.Sub),
    hs.special("*"): attrdict(unary = None,
                              binary = pyast.Mult),
    hs.special("/"): attrdict(unary = None,
                              binary = pyast.Div),

    hs.special("<"): attrdict(unary = None,
                              binary = pyast.Lt,
                              compare = True),
    hs.special(">"): attrdict(unary = None,
                              binary = pyast.Gt,
                              compare = True),
    hs.special("=<"): attrdict(unary = None,
                               binary = pyast.LtE,
                               compare = True),
    hs.special(">="): attrdict(unary = None,
                               binary = pyast.GtE,
                               compare = True),
    hs.special("=="): attrdict(unary = None,
                               binary = pyast.Eq,
                               compare = True),
    hs.special("!="): attrdict(unary = None,
                               binary = pyast.NotEq,
                               compare = True)

    }



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
        if isinstance(stmt, pyast.expr):
            if isinstance(stmt, (pyast.Name, pyast.Num, pyast.Str)):
                self.blocks[-1].append((None, var))
                return var
            stmt = transloc(pyast.Expr(stmt), stmt)
        self.blocks[-1].append((stmt, var))
        return var

    def assign_and_register(self, expr, node):
        if node and hastag(node, "location"):
            expr = transloc(expr, node)
        v = UniqueVar("temp")
        r = pyast.Assign([transloc(pyast.Name(str(v), pyast.Store()), node)], expr)
        return self.register(r, transloc(pyast.Name(str(v), pyast.Load()), node), node)

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
        r = pyast.Name(str(node), pyast.Load())
        return self.register(r, r, node)

    def visit_UniqueVar(self, node):
        r = pyast.Name(str(node), pyast.Load())
        return self.register(r, r, node)

    def visit_value(self, node, value):
        if isinstance(value, (int, float)):
            r = pyast.Num(value)
        elif isinstance(value, str):
            r = pyast.Str(str(value))
        elif value in (None, True, False):
            r = pyast.Name(str(value), pyast.Load())
        else:
            raise Exception("unknown value", value)
        return self.register(r, r, node)

    def visit_special(self, node, value):
        r = pyast.Name('%%' + str(value), pyast.Load())
        return self.register(r, r, node)

    def visit_tuple(self, node, *args):
        newargs = list(map(self.visit, args))
        c = pyast.Call(transloc(pyast.Name('%%tuple', pyast.Load()), node),
                       newargs,
                       [],
                       None,
                       None)
        return self.assign_and_register(c, node)

    def visit_send(self, node, obj, msg):

        if isinstance(obj, (hs.special, hs.value)) and obj in ops:
            spec = ops[obj]
            if isinstance(msg, hs.tuple):
                left, right = msg[:]
                if left == hs.value(VOID):
                    r = pyast.UnaryOp(spec.unary(), self.visit(right))
                else:
                    if getattr(spec, 'compare', False):
                        r = pyast.Compare(self.visit(left),
                                             [spec.binary()],
                                             [self.visit(right)])
                    else:
                        r = pyast.BinOp(self.visit(left),
                                           spec.binary(),
                                           self.visit(right))
            else:
                raise Exception("Sending to strange thing:", node)
        elif isinstance(msg, hs.tuple):
            r = pyast.Call(self.visit(obj),
                           list(map(self.visit, msg[:])),
                           [],
                           None,
                           None)
        else:
            r = pyast.Call(pyast.Name('%%send', pyast.Load()),
                           [self.visit(obj), self.visit(msg)],
                           [],
                           None,
                           None)

        return self.assign_and_register(r, node)

    def visit_begin(self, node, *stmts):
        rval = pyast.Name("None", pyast.Load())
        for stmt in stmts:
            rval = self.visit(stmt)
        return rval

    def visit_declaring(self, node, variables, body):
        for v in variables:
            self.add_declare(v)
        return self.visit(body)

    def visit_assign(self, node, var, value):
        self.add_assign(var)
        pyvar = transloc(pyast.Name(str(var), pyast.Load()), var)
        stmt = transloc(pyast.Assign([transloc(pyast.Name(str(var), pyast.Store()), var)],
                                     self.visit(value)), node)
        return self.register(stmt, pyvar, node)

    def visit_if(self, node, test, ift, iff):
        v = transloc(UniqueVar("if_result"), node)
        test = self.visit(test)
        _if = UGToPyIf2(test, v, self.scope)
        _if.visit(ift)
        _if.push()
        _if.visit(iff)
        c = _if.create()
        return self.register(c, transloc(pyast.Name(str(v), pyast.Load()), v), node)

    def visit_catch(self, node, expr, handler):
        v = transloc(UniqueVar("catch_result"), node)
        _catch = UGToPyCatch2(v, self.scope)
        _catch.visit(expr)
        _catch.push()
        _catch.visit(handler)
        c = _catch.create()
        return self.register(c, transloc(pyast.Name(str(v), pyast.Load()), v), node)

    def visit_return(self, node, v):
        r = pyast.Return(self.visit(v))
        return self.register(r, pyast.Name("unreachable", pyast.Load()), node)
        # self.statements.append(r)
        # return self.visit(hs.value("unreachable"))

    def visit_object(self, node, v, *specs):

        def build_expr(specs):
            decl, body = specs[0]
            if isinstance(decl, hs.declaring):
                vs, decl_block = decl[:]
            else:
                vs, decl_block = [], decl
                
            if len(specs) == 1:
                expr = hs2.begin(decl_block, body)
            else:
                expr = hs2.begin(hs2.catch(decl_block,
                                           hs2["return"](build_expr(specs[1:]))),
                                 body)
            return hs2.declaring(vs, expr)


        name = transloc(UniqueVar("class"), node)

        r = UGToPyDef2('__recv__',
                       pyast.arguments([pyast.arg(str(UniqueVar("_")), None),
                                        pyast.arg(str(v), None)],
                                       None, None, [], None, None, [], []))

        r.visit(build_expr(specs))

        c = transloc(
            pyast.ClassDef(
                str(name),
                [pyast.Name("%%ugobj", pyast.Load())],
                [],
                None,
                None,
                [r.create()],
                []),
            node)

        self.register(c, pyast.Name(str(name), pyast.Load()), node)

        result = transloc(UniqueVar("result"), node)

        return self.visit(hs2.declaring([result],
                                        hs2.begin(hs2.assign(result,
                                                             hs2.send(name, hs.tuple())),
                                                  result)))


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
        m = pyast.Module(statements)
        return m




class UGToPyDef(UGToPy):

    def __init__(self, name, args = None):
        self.name = name
        self.args = args or pyast.arguments([], None, None, [], None, None, [], [])
        self.declares = set()
        self.assigns = set()
        super().__init__(self)

    def create(self):
        statements = [transloc(pyast.Nonlocal([str(x)]), x)
                      for x in self.assigns if x not in self.declares]
        _statements, = self.blocks[:]

        statements += self.finalize_statements(_statements,
                                               pyast.Name("None", pyast.Load()),
                                               pyast.Return)

        f = pyast.FunctionDef(
            self.name,
            self.args,
            statements,
            [],
            None
            )

        # transloc(f, _statements[0][1])

        return f



class UGToPyIf(UGToPy):

    def __init__(self, test, var, scope):
        super().__init__(scope)
        self.test = test
        self.var = var

    def create(self):
        tstmts, fstmts = self.blocks

        var = transloc(pyast.Name(str(self.var), pyast.Store()), self.var)

        tstmts = self.finalize_statements(tstmts, None, lambda x: pyast.Assign([var], x))
        fstmts = self.finalize_statements(fstmts, None, lambda x: pyast.Assign([var], x))

        return pyast.If(self.test, tstmts, fstmts)



class UGToPyCatch(UGToPy):

    def __init__(self, var, scope):
        super().__init__(scope)
        self.var = var

    def create(self):
        tstmts, estmts = self.blocks

        var = transloc(pyast.Name(str(self.var), pyast.Store()), self.var)

        tstmts = self.finalize_statements(tstmts, None, lambda x: pyast.Assign([var], x))
        estmts = self.finalize_statements(estmts, None, lambda x: pyast.Assign([var], x))

        return pyast.TryExcept(
                tstmts,
                [pyast.ExceptHandler(None,
                                     None,
                                     estmts)],
                [])



class ugobj:

    def __call__(self, *args, **kwargs):
        if kwargs:
            if args:
                return self.__recv__(hybrid(args, kwargs))
            else:
                return self.__recv__(kwargs)
        else:
            return self.__recv__(args)

    def __getattr__(self, attr):
        if attr.startswith('__'):
            return getattr(super(), attr)
        else:
            return self.__recv__(attr)

    def __getitem__(self, item):
        return self.__recv__(index(item))



def send(obj, msg):
    if isinstance(msg, tuple):
        return obj(*msg)
    elif isinstance(msg, dict):
        return obj(**msg)
    elif isinstance(msg, hybrid):
        return obj(*msg.tuple, **msg.dict)
    elif isinstance(msg, str):
        return getattr(obj, msg)
    elif isinstance(msg, index):
        return obj[msg.item]
    else:
        return obj.__recv__(msg)

def patch_tuple(*args):
    if len(args) == 0:
        return ()
    elif len(args) == 1:
        return tuple(args[0])
    else:
        return reduce(lambda x, y: tuple(x) + tuple(y), args)

def make_dict(*args):
    return dict(args)

def patch_dict(*args):
    if len(args) == 0:
        return {}
    else:
        rval = {}
        for entry in args:
            rval.update(entry)
        return rval

# hybrid = anonstruct['']

# def make_hybrid(l, d):
#     h = hybrid()
#     h.__l__ = l
#     h.__d__ = d
#     return h


def _assign(obj, item, value):
    if isinstance(item, index):
        obj[item.item] = value
    elif isinstance(item, str):
        setattr(obj, item, value)
    else:
        obj.__recv__(hs.assign(item, value))

def _check(checker, value):
    if hasattr(checker, '__check__'):
        return checker.__check__(value)
    elif isinstance(checker, type):
        if isinstance(value, checker):
            return value
        else:
            raise TypeError("Expected %s but got %s" % (checker, type(value)))

class _check_equal:
    def __init__(self, value):
        self.value = value
    def __check__(self, other):
        if self.value == other:
            return other
        raise TypeError("Expected == %s but got %s" % (self.value, other))


def _deconstruct(dctor, value):
    if dctor is None:
        if isinstance(value, (tuple, list, dict, hybrid)):
            return value
        else:
            raise Exception("Not deconstructible")
    else:
        return dctor.__deconstruct__(value)

def _extract_tuple(value, minlen, maxlen):
    # print(value, minlen, maxlen)
    if isinstance(value, (tuple, list)):
        tup = value
    elif isinstance(value, hybrid):
        tup = value.tuple
    elif isinstance(value, (dict)):
        tup = ()
    else:
        raise Exception("No tuple")
    if len(tup) < minlen:
        raise Exception("Too small")
    elif maxlen is not None and len(tup) > maxlen:
        raise Exception("Too large")
    else:
        return tup

def _tuple_index(tup, idx):
    return tup[idx]

def _tuple_range(tup, start, end):
    return tup[start:end]

def _extract_dict(value):
    if isinstance(value, (dict)):
        dic = value
    elif isinstance(value, hybrid):
        dic = value.dict
    elif isinstance(value, (tuple, list)):
        dic = {}
    else:
        raise Exception("No dict")
    return dic

def _extract_dict_copy(value):
    return dict(_extract_dict(value))

def _dict_index(dic, idx):
    return dic[idx]

def _dict_pop(dic, idx):
    return dic.pop(idx)

def _dict_empty(dic):
    if dic:
        raise Exception("Remaining entries", dic)
    return True



def evaluate(ast, source = None):
    # print(ast)
    # py = UGToPyExpr(None).visit(ast)

    ctor = UGToPyDef2("wackadoodle")
    # ctor = UGToPyModule2()

    ctor.visit(ast)
    py = ctor.create()
    # print(py.args)
    if isinstance(py, pyast.expr):
        py = pyast.Expression(py)
    elif isinstance(py, pyast.stmt):
        py = pyast.Module([py])
    py = pyast.fix_missing_locations(py)
    # pprint(py)
    # py.lineno = 1
    # py.col_offset = 1
    # pprint(py)
    code = compile(py, # source and source.url or
                   "<string>", 'exec')

    d = {'f': lambda x, y: x + y,
         'l': [1, 2],

         '%%hashstruct': hs,
         '%%index': index,
         '%%tuple': lambda *args: args,
         '%%send': send,
         '%%make_hybrid': hybrid,
         '%%make_dict': make_dict,
         '%%patch_dict': patch_dict,
         '%%patch_tuple': patch_tuple,

         '%%assign': _assign,
         '%%deconstruct': _deconstruct,
         '%%check': _check,
         '%%check_equal': _check_equal,
         '%%extract_tuple': _extract_tuple,
         '%%tuple_index': _tuple_index,
         '%%tuple_range': _tuple_range,
         # '%%extract_dict': _extract_dict,
         '%%extract_dict_copy': _extract_dict_copy,
         # '%%dict_index': _dict_index,
         '%%dict_pop': _dict_pop,
         '%%dict_empty': _dict_empty,

         '%%ugobj': ugobj,
         }
    exec(code, d)
    return d['wackadoodle']()


def pprint(node, offset = 0):

    # if isinstance(node, pyast.Name):
    #     print(" " * offset, "Name:" + node.id, getattr(node, 'lineno', 'n/a'), getattr(node, 'col_offset', 'n/a'))
    # elif isinstance(node, pyast.Num):
    #     print(" " * offset, "Num:" + str(node.n), getattr(node, 'lineno', 'n/a'), getattr(node, 'col_offset', 'n/a'))
    # else:
    #     print(" " * offset, type(node).__name__, getattr(node, 'lineno', 'n/a'), getattr(node, 'col_offset', 'n/a'))
    
    if isinstance(node, pyast.AST):
        print(" " * offset,
              type(node).__name__,
              "(%s,%s)" % (getattr(node, 'lineno', 'n/a'), getattr(node, 'col_offset', 'n/a')),
              ";".join("%s=%s" % (field, getattr(node, field)) for field in node._fields))
    else:
        raise Exception("Not an AST node", node)
        # print(" " * offset,
        #       node)



    for child in pyast.iter_child_nodes(node):
        pprint(child, offset + 4)


# S = """1 + 2"""

# py = pyast.parse(S, "meh")
# pprint(py)
# code = compile(py, "<string>", 'exec')
# print(code)
# print("----------------------")



