
import ast as pyast
import exc

from ...lib import hashstruct as hs, struct, ugstr, tag, gettag, hastag
from ..generic.parse import Token, NodeToken, Bracket, Operator, VOID, SyntaxError
from functools import reduce


class ASTException(exc.Exception):
    def __init__(self, **keywords):
        self.__dict__.update(keywords)
    def __str__(self):
        return "[%s]" % (", ".join("%s = %s" % (k, v)
                                   for k, v in self.__dict__.items()))
    def __repr__(self):
        return str(self)


value_void = hs.value(VOID)

def mkv(x):
    return hs.value(x)

def getloc(x):
    if isinstance(x, pyast.AST):
        return (x.lineno, x.col_offset)
    else:
        return gettag(x, "location")

def setloc(x, loc):
    if isinstance(x, pyast.AST):
        if (getattr(x, 'lineno', None) is None
            and getattr(x, 'col_offset', None) is None):
            if isinstance(loc, tuple):
                line, col = loc
            else:
                (line, col), end = loc.linecol()
            x.lineno = line
            x.col_offset = col
        return x
    elif hastag(x, "location"):
        return x
    else:
        return tag(x, "location", loc)

def transloc(x, y):
    try:
        return setloc(x, getloc(y))
    except (KeyError, AttributeError):
        # print("NO LOCATION:", y)
        return x


def make_ast(node):

    def helper():

        if isinstance(node, Token):
            cmd, *args = node.args
            if cmd == 'id':
                return ugstr(args[0])
            elif cmd == 'num10':
                n, dec, exp = args
                if dec is not None or exp is not None:
                    return mkv(float("%s.%se%s" % (n or '0', dec or '0', exp or '0')))
                else:
                    return mkv(int(n))
            elif cmd == 'numR':
                radix, n, dec = args
                radix = int(radix)
                if dec is not None:
                    return mkv(int(n + dec, radix) / radix**len(dec))
                else:
                    return mkv(int(n, radix))
            elif cmd == 'str':
                return hs.string(args[0])
            elif cmd == VOID:
                return mkv(VOID)
            else:
                raise ASTException['unsupported_token'](node = node)

        elif isinstance(node, NodeToken):
            op = node.op
            if isinstance(op, Bracket):
                args = list(map(make_ast, node.args))
                if args[0] != value_void or args[1] != value_void:
                    raise SyntaxError['defective_bracketing'](
                        left = args[0],
                        right = args[1])
                    # raise Exception("Left and right of bracket should be VOID")
                return getattr(hs, op.type)(args[2])
            elif isinstance(op, Operator):
                op = node.op.name
                args = list(map(make_ast, node.args))
                if op == '':
                    if node.op.width == 'wide':
                        return hs.wjuxt(*args)
                    else:
                        return hs.sjuxt(*args)
                elif op == ',':
                    return hs.seq(*args)
                elif op == 'NL':
                    if args[0] == value_void:
                        return args[1]
                    if args[1] == value_void:
                        return args[0]
                    return hs.seq(*args)
                else:
                    return hs.oper(transloc(ugstr(op), node.op), *args)
            else:
                raise SyntaxError['what'](node)

    return tag(helper(), 'location', node.loc)


class ASTVisitor:

    def __init__(self, transfer_locations = True):
        self.transfer_locations = transfer_locations

    def visit(self, node):
        try:
            f = getattr(self, "visit_" + type(node).__name__)
            generic = False
        except AttributeError:
            f = self.visit_generic
            generic = True
        if isinstance(node, struct) and not generic:
            rval = f(node, *node.__l__)
        else:
            rval = f(node)
        if self.transfer_locations:
            try:
                rval = transloc(rval, node)
            except KeyError:
                pass
                # print("NO LOCATION", node, node.__tags__)
        return rval

    def visit_generic(self, node):
        return node



t_wjuxt = hs["wjuxt"]
t_sjuxt = hs["sjuxt"]
t_seq = hs["seq"]
t_string = hs["string"]


def collapse(visit, node, cls, name):
    a, b = node[:]
    arguments = [visit(b)]
    while isinstance(a, cls):
        arguments.append(visit(a[1]))
        a = a[0]
    arguments.append(visit(a))
    arguments = [a for a in reversed(arguments) if a is not VOID and a != hs.value(VOID)]
    if len(arguments) == 0:
        raise exc.Exception['mystery']("Investigate how this happened.")
    elif len(arguments) == 1:
        return arguments[0]
    else:
        return getattr(hs, name)(*arguments)
        # loc = reduce(lambda x, y: x + y,
        #              [getloc(arg) for arg in arguments])
        # return setloc(getattr(hs, name)(*arguments), loc)


class ASTCollapse(ASTVisitor):

    def visit_ugstr(self, node):
        return node

    def visit_value(self, node, v):
        return node

    def visit_wjuxt(self, node, a, b):
        return collapse(self.visit, node, t_wjuxt, "juxt")

    def visit_sjuxt(self, node, a, b):
        return collapse(self.visit, node, t_sjuxt, "juxt")

    def visit_seq(self, node, a, b):
        a = self.visit(a)
        b = self.visit(b)
        arguments = []
        if isinstance(a, t_seq):
            arguments += a[:]
        else:
            arguments.append(a)
        if isinstance(b, t_seq):
            arguments += b[:]
        else:
            arguments.append(b)
        return hs.seq(*arguments)

    def visit_round(self, node, expr):
        expr = self.visit(expr)
        if isinstance(expr, t_seq):
            # return transloc(hs.begin(*expr[:]), node)
            return hs.begin(*expr[:])
        else:
            return expr

    def visit_square(self, node, expr):
        if expr == hs.value(VOID):
            return hs.square()
        expr = self.visit(expr)
        if isinstance(expr, t_seq):
            rval = hs.square(*expr[:])
        else:
            rval = hs.square(expr)
        return rval
        # return transloc(rval, node)

    def visit_curly(self, node, expr):
        if expr == hs.value(VOID):
            return hs.curly()
        expr = self.visit(expr)
        if isinstance(expr, t_seq):
            rval = hs.curly(*expr[:])
        else:
            rval = hs.curly(expr)
        return rval
        # return transloc(rval, node)

    def visit_quote(self, node, expr):
        if isinstance(expr, t_string):
            return mkv(expr[0])
            # return transloc(mkv(expr[0]), node)
        elif expr == hs.value(VOID):
            return mkv("")
        else:
            raise SyntaxError['illegal_in_quote'](
                quote = node,
                expr = expr)
            # raise Exception("Illegal node in quote.")

    def visit_oper(self, node, op, a, b):
        # return transloc(hs.oper(self.visit(op), self.visit(a), self.visit(b)), node)
        return hs.oper(self.visit(op), self.visit(a), self.visit(b))

    def visit_generic(self, node):
        raise ASTException['collapse/unknown'](node = node)



class ASTModifier(ASTVisitor):

    def visit_ugstr(self, node):
        return node

    def visit_value(self, node, v):
        return node

    def visit_juxt(self, node, *args):
        return hs.juxt(*map(self.visit, args))

    def visit_oper(self, node, *args):
        return hs.oper(*map(self.visit, args))

    def visit_begin(self, node, *args):
        return hs.begin(*map(self.visit, args))

    def visit_square(self, node, *args):
        return hs.square(*map(self.visit, args))

    def visit_curly(self, node, *args):
        return hs.curly(*map(self.visit, args))

