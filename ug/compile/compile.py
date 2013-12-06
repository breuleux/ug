
import ast as pyast
from functools import reduce

from . import lib

from ..parsing import decode as de, Void, Location, SyntaxError
from ..parsing.ug.ast import ASTVisitor, transfer, getloc, setloc, hasloc 
from ..lib import hashstruct as hs, ugstr, hastag, tag, struct
pycompile = compile


def revisit(x):
    return hs.revisit(x)


#############
# UniqueVar #
#############

class UniqueVar:
    __id__ = 0
    def __init__(self, name):
        self.name = name
        self.id = UniqueVar.__id__
        self.__tags__ = {}
        UniqueVar.__id__ += 1
    def __str__(self):
        return self.name + "#" + str(self.id)
    def __repr__(self):
        return str(self)
    def __descr__(self, descr):
        return ({"@UniqueVar"}, descr(self.name), descr(self.id))


###########
# helpers #
###########

def build_fcall(obj, *args):
    return hs2.send(obj, hs2.seq(*args))

def build_scall(obj, *args):
    if isinstance(obj, str):
        raise Exception
        caller = hs.special(obj)
    else:
        caller = hs.value(obj)
    return build_fcall(caller, *args)


#####################
# Location handling #
#####################

def fix_locations(nodes, above = None):

    nn = len(nodes)
    loc = None
    for i, node in enumerate(nodes):
        if hasloc(node):
            if loc is None:
                loc = getloc(node)
            else:
                loc += getloc(node)
        elif isinstance(node, struct):
            start, end = None, None
            src = None
            for j in range(i - 1, -1, -1):
                node2 = nodes[j]
                if hasloc(node2):
                    l = getloc(node2)
                    src = l.source
                    start = l.end
                    break
            for j in range(i + 1, nn):
                node2 = nodes[j]
                if hasloc(node2):
                    l = getloc(node2)
                    src = l.source
                    end = l.start
                    break
            if start is None:
                if end is None:
                    if above is None:
                        return None
                    start, end = above.span
                start = end
            if end is None:
                end = start
            # print("SETTING:", node, start, end)
            newloc = Location(src, (start, end))
            setloc(node, newloc)
            fix_locations(node[:], newloc)

    return loc

class HS2:
    def __getattr__(self, attr):
        f = getattr(hs, attr)
        def f2(*args):
            loc = fix_locations(args)
            result = f(*args)
            if loc is None:
                return result
            else:
                return setloc(result, loc)
        return f2
    def __getitem__(self, item):
        return getattr(self, item)

hs2 = HS2()



def accumulate_sequence(compiler, entries, type, expand_eqassoc = True):
    results = []
    for i, entry in enumerate(entries):
        newentry = compiler.svisit(entry)
        if isinstance(newentry, hs.splice):
            if type in getattr(newentry, "expand_in", ["begin"]):
                results += accumulate_sequence(compiler, newentry[:], type, expand_eqassoc)
            else:
                results.append(compiler.xvisit(newentry))
        elif isinstance(newentry, hs.restmacro):
            f = newentry[0]
            results += accumulate_sequence(compiler, f(newentry, hs[type](*entries[i+1:])), type, expand_eqassoc)
            break
        elif expand_eqassoc and isinstance(newentry, hs.eqassoc):

            lhs, rhs = newentry[:]

            p = ParseLHS(lhs)
            instructions = p.instructions

            if p.simple and p.variables:
                [[variable, handler]] = p.variables
                instructions += [hs2.declare(variable, handler),
                                 hs2.assign(variable, tag(rhs, "name", variable))]
            else:
                rv = transfer(UniqueVar("α"), p.deconstructor)
                instructions += [hs2.declare(rv, None),
                                 hs2.assign(rv, build_fcall(p.deconstructor, rhs))]

                for i, (variable, handler) in enumerate(p.variables):
                    entry = hs2.send(rv, build_scall(hs.index, hs.value(i)))
                    instructions += \
                        [hs2.declare(variable, handler),
                         hs2.assign(variable, entry)]

            if p.guard is not None:
                lbda = hs2["lambda"]([], p.guard)
                tag(lbda, "tag_location", getloc(lbda))
                g = build_scall(lib.apply_guard, lbda)
                instructions += [g]

            instructions.append(hs.value(None))

            results += accumulate_sequence(compiler, instructions, type, False)

        else:
            results.append(newentry)

    return results


def parse_sequence(compiler, seq):

    lparts = [[]]
    dparts = [[]]

    holes = False
    holen = 0

    def gethole(x, star = ""):
        nonlocal holes, holen
        if not isinstance(x, str):
            return False
        elif x == "_":
            holen += 1
            holes = True
            return build_scall(lib.Hole, hs.value(holen - 1), hs.value(star))
        elif x.startswith("_"):
            if holen:
                raise Exception
            else:
                holes = True
                x = x[1:]
                if x.isdigit():
                    return build_scall(lib.Hole, hs.value(int(x)), hs.value(star))
                else:
                    return build_scall(lib.Hole, hs.value(x), hs.value(star))
        return False
        

    for arg in seq:
        if isinstance(arg, str) and arg.startswith("_"):
            hole = gethole(arg)
            lparts[-1].append(hole)
        elif isinstance(arg, hs.star):
            hole = gethole(arg[0], "*")
            if hole:
                lparts[-1].append(hole)
            else:
                lparts.append(compiler.xvisit(arg[0]))
                lparts.append([])
        elif isinstance(arg, hs.dstar):
            hole = gethole(arg[0], "**")
            if hole:
                lparts[-1].append(hole)
            else:
                dparts.append(compiler.xvisit(arg[0]))
                dparts.append([])
        elif isinstance(arg, hs.assoc):
            k = compiler.xvisit(arg[0])
            v = compiler.xvisit(arg[1])
            dparts[-1].append((k, v))

        elif isinstance(arg, hs.eqassoc):

            lhs, rhs = arg[:]

            if isinstance(lhs, (str, ugstr)):
                hole = gethole(rhs, "")
                if hole:
                    dparts[-1].append((hs2.value(lhs), hole))
                else:
                    new_rhs = compiler.xvisit(rhs)
                    tag(new_rhs, "name", lhs)
                    dparts[-1].append((build_scall(lib.symbol, hs2.value(lhs)), new_rhs))

            else:
                p = ParseLHS(lhs, allow_guard = False)
                variables = p.variables
                deconstructor = p.deconstructor
                rv = transfer(UniqueVar("α"), deconstructor)
                instructions = p.instructions
                instructions.append(hs2.declare(rv, None))
                vnames = []
                for v, h in variables:
                    # if h is not None:
                    #     raise Exception("cannot type variable here", h)
                    vnames.append(hs.value(v))
                
                instructions += [hs2.assign(rv, build_fcall(deconstructor, rhs)),
                                 build_scall(dict,
                                             build_scall(zip,
                                                         hs2.seq(*vnames),
                                                         rv))]
                expr = hs2.begin(*instructions)
                dparts.append(compiler.xvisit(expr))
                dparts.append([])

        else:
            lparts[-1].append(arg)

    return (holes,
            [x for x in lparts if x],
            [x for x in dparts if x])


class Compiler(ASTVisitor):

    def __init__(self, glob, cenv):
        super().__init__()
        self.glob = glob
        self.cenv = cenv

    def visit(self, node):
        res = super().visit(node)
        if isinstance(res, hs.revisit):
            try:
                res = transfer(res[0], res)
            except KeyError:
                print("WHAT")
            return self.visit(res)
        else:
            return res

    def visit_revisit(self, node, expr):
        return self.visit(expr)

    def xvisit(self, node):
        if isinstance(self, ExprCompiler):
            return self.visit(node)
        else:
            return ExprCompiler(self.glob, self.cenv).visit(node)

    def svisit(self, node):
        if isinstance(self, InSeqCompiler):
            return self.visit(node)
        else:
            return InSeqCompiler(self.glob, self.cenv).visit(node)

    # Use `return revisit(x)` instead of `return self.visit(x)`
    # because the former will automatically annotate x with the
    # visited node's location information before calling visit()
    # again, whereas the latter will not unless you do it explicitly.

    def visit_ugstr(self, node):
        if node in self.cenv:
            return revisit(self.cenv[node])
        return node

    def visit_str(self, node):
        return self.visit_ugstr(node)

    def visit_processed(self, node, s):
        return s

    def visit_UniqueVar(self, node):
        return node

    def visit_value(self, node, v):
        return node

    def visit_juxt(self, node, *args):
        if len(args) == 0:
            return transfer(hs.value(Void), node)
        if len(args) == 1:
            return self.xvisit(args[0])
        f, arg, *rest = args
        if rest:
            return revisit(hs.juxt(hs2.juxt(f, arg), *rest))
        f = self.xvisit(f)
        if isinstance(f, hs.macro):
            return revisit(f[0](self, node, arg))
        else:
            arg = self.xvisit(arg)
            if isinstance(arg, hs.macro):
                arg = arg[0](self, arg, hs.void())
            elif isinstance(arg, hs.partial):
                arg = build_scall(lib.Partial, arg[0])
            return hs.send(f, arg)

    def visit_oper(self, node, op, a, b):
        return revisit(hs.juxt(op, hs2.seq(a, b)))

    def visit_begin(self, node, *args):
        newargs = accumulate_sequence(self, args, "begin")

        declarations = []
        arguments = []
        for arg in newargs:
            if isinstance(arg, hs.declare):
                declarations.append((arg[0], arg[1]))
            else:
                arguments.append(arg)

        objs = []
        loc = None

        stmts = hs2.begin(*arguments)
        if declarations:
            return hs2.declaring(tuple(declarations), stmts)
        elif len(arguments) == 1:
            return stmts[0]
        else:
            return stmts


    def visit_square(self, node, *args):
        if len(args) == 1 and args[0] in ('=', '=>'):
            return build_scall(lib.OrderedDict)

        newargs = accumulate_sequence(self, args, "square", False)
        holen, t, d = parse_sequence(self, newargs)
        if t:
            if len(t) == 1 and isinstance(t[0], list):
                t = hs2.seq(*t[0])
            else:
                t = build_scall(lib.patch_list,
                                *[hs2.seq(*x) if isinstance(x, list) else x
                                  for x in t])
        if d:
            def mkdict(kvs):
                return build_scall(lib.OrderedDict,
                                   # hs2.seq(*[hs2.seq(build_scall(lib.symbol, x[0]), x[1]) for x in kvs]))
                                   hs2.seq(*[hs2.pair(x[0], x[1]) for x in kvs]))
            if len(d) == 1 and isinstance(d[0], list):
                d = mkdict(d[0])
            else:
                d = build_scall(lib.patch_odict,
                                *[mkdict(x) if isinstance(x, list) else x
                                  for x in d])

        if t and d:
            rval = build_scall(lib.hybrid, t, d)
        elif t:
            rval = t
        elif d:
            rval = d
        else:
            rval = hs.seq()

        if holen:
            return hs.partial(rval)
        else:
            return rval

    def visit_curly(self, node, *args):
        if len(args) == 1 and args[0] in ('=', '=>'):
            return build_scall(dict)

        newargs = accumulate_sequence(self, args, "curly", False)
        holen, t, d = parse_sequence(self, newargs)
        if t:
            if len(t) == 1 and isinstance(t[0], list):
                t = build_scall(set, hs2.seq(*t[0]))
            else:
                t = build_scall(lib.patch_set,
                                *[hs2.seq(*x) if isinstance(x, list) else x
                                  for x in t])
        if d:
            def mkdict(kvs):
                return build_scall(dict,
                                   # hs2.seq(*[hs2.seq(build_scall(lib.symbol, x[0]), x[1]) for x in kvs]))
                                   hs2.seq(*[hs2.pair(x[0], x[1]) for x in kvs]))
            if len(d) == 1 and isinstance(d[0], list):
                d = mkdict(d[0])
            else:
                d = build_scall(lib.patch_dict,
                                *[mkdict(x) if isinstance(x, list) else x
                                  for x in d])
        if t and d:
            return build_scall(lib.hybrid, t, d)
        elif t:
            return t
        elif d:
            return d
        else:
            return build_scall(set)


    # def visit_curly(self, node, *args):
    #     newargs = accumulate_sequence(self, args, "curly")
    #     return hs.curly(*newargs)

    def visit_send(self, node, obj, msg):
        return hs.send(self.xvisit(obj), self.xvisit(msg))

    def visit_lambda(self, node, variables, body):
        newbody = self.xvisit(body)
        return hs["lambda"](variables, newbody)

    def visit_seq(self, node, *args):
        return hs.seq(*map(self.xvisit, args))

    def visit_if(self, node, cond, iftrue, iffalse):
        return hs["if"](self.xvisit(cond), self.xvisit(iftrue), self.xvisit(iffalse))

    def visit_assign(self, node, var, value):
        return self.svisit(node)

    def visit_colonargs(self, node, *args):
        raise Exception("#colonargs not valid here")

    def visit_generic(self, node):
        raise Exception("Unknown node", node)

    def visit_extra(self, node, x):
        return node




class ExprCompiler(Compiler):

    def visit_splice(self, node, *args):
        return revisit(hs.begin(*args))

    def visit_star(self, node, arg):
        raise SyntaxError['bad_context'](
            operator = "*",
            node = node)

    def visit_dstar(self, node, arg):
        raise SyntaxError['bad_context'](
            operator = "**",
            node = node)

    def visit_assoc(self, node, k, v):
        raise SyntaxError['bad_context'](
            operator = "=>",
            node = node)

    def visit_eqassoc(self, node, lhs, rhs):
        raise SyntaxError['bad_context'](
            operator = "=",
            node = node)

    def visit_macro(self, node, value):
        return node

    def visit_restmacro(self, node, f):
        return revisit(hs.begin(*f(node, hs.begin())))





class InSeqCompiler(Compiler):

    def visit_splice(self, node, *args):
        return node

    def visit_star(self, node, arg):
        return node

    def visit_dstar(self, node, arg):
        return node

    def visit_assoc(self, node, k, v):
        return node

    def visit_eqassoc(self, node, lhs, rhs):
        return node

    def visit_macro(self, node, f):
        return revisit(f(self, node, hs.value(Void)))

    def visit_restmacro(self, node, value):
        return node

    def visit_declare(self, node, var, constructor):
        return hs2.declare(var, (constructor
                                 and self.xvisit(constructor)))
    
    def visit_assign(self, node, var, value):
        if var is None:
            return hs2.assign(None, self.xvisit(value))
        elif isinstance(var, list):
            raise Exception
            return hs2.assign(list(map(self.xvisit, var)),
                              self.xvisit(value))
        else:
            return hs2.assign(self.svisit(hs.begin(var)),
                              self.xvisit(value))







class ParseLHS(ASTVisitor):

    def __init__(self, lhs, allow_guard = True):
        super().__init__()
        self.allow_guard = allow_guard
        self.guard = None
        self.variables = []
        self.instructions = []
        self.simple = True
        dctor = self.visit(lhs, d = None)
        self.deconstructor_arg = dctor
        self.deconstructor = build_scall(lib.Deconstructor, dctor)

    def visit(self, node, d):
        result = super().visit(node, d = d)
        try:
            tag(result, "tag_location", getloc(node))
        except:
            pass
        return result

    def register_variable(self, node, d):
        if node == "_":
            return hs.value(None), d
        else:
            var = hs.value(node)
            if d is not None:
                dv = transfer(UniqueVar(node + "&"), d)
                self.instructions.append(hs.declare(dv, None))
                self.instructions.append(hs.assign(dv, d))
            else:
                dv = None
            self.variables.append((node, dv))
            return build_scall(lib.symbol, var), dv

    def hash(self, name):
        return hs.send(hs.value(lib.hashstruct), hs.value(name))

    def make_check(self, expr, variable):
        return build_scall(lib.struct_project, expr, variable)
        # return build_fcall(self.hash("check"), expr, variable)

    def make_deconstruct(self, expr, variables):
        return build_scall(lib.struct_deconstruct, expr, hs.seq(*variables))
        # return build_fcall(self.hash("deconstruct"), expr, *variables)

    def visit_generic(self, node, d = None):
        raise SyntaxError['lhs/invalid'](
            message = ("This expression is invalid on the left hand side of "
                       "an assignment or declaration"),
            node = node)

    def visit_ugstr(self, node, d):
        var, d = self.register_variable(node, d)
        if d is None:
            return var
        else:
            return self.make_check(d, var)

    def visit_str(self, node, d):
        return self.visit_ugstr(node, d)

    def visit_value(self, node, value, d):
        self.simple = False
        if d is not None:
            raise SyntaxError['lhs/checker'](
                message = "You cannot give a type or wrapper to a literal left hand side.",
                nodes = [node, d])
        # if isinstance(node[0], str):
        #     node = build_scall(lib.symbol, node)
        expr = build_scall(lib.check_equal, node)
        return self.make_check(expr, hs.value(None))

    def visit_send(self, node, f, arg, d):
        self.simple = False
        if d is not None:
            raise SyntaxError['lhs/checker'](
                message = "You cannot give a type or wrapper to a literal left hand side.",
                nodes = [node, d])
        expr = build_scall(lib.check_equal, node)
        return self.make_check(expr, hs.value(None))

    def visit_juxt(self, node, *args, d):
        if len(args) == 1:
            return self.visit(args[0], d = d)
        if d is not None:
            raise SyntaxError['lhs/checker'](
                message = "You cannot give a type or wrapper to a left hand side that already has one.",
                nodes = [node, d])
        *dctor, var = args
        dctor = hs2.juxt(*dctor)
        return self.visit(var, d = dctor)

    def visit_square(self, node, *args, d):
        self.simple = False
        newargs = [self.visit(x, d = None) for x in args]
        if d is None:
            return hs2.seq(*newargs)
        else:
            return self.make_deconstruct(d, newargs)

    def visit_oper(self, node, op, x, y, d):
        was_simple = self.simple
        self.simple = False

        if op == "-" and x == hs.value(Void) and isinstance(y, hs.value):
            return self.visit(hs.value(-y[0]), d = d)

        elif op == "=":
            if d is not None:
                raise SyntaxError['lhs/checker'](
                    message = "You cannot give a type or wrapper to a whole default expression (put it on the left hand side of = instead).",
                    nodes = [node, d])
            return build_scall(lib.struct_default, #self.hash("default"),
                               self.visit(x, d = d), y)

        elif op == "when":
            if not was_simple:
                raise SyntaxError['lhs/when'](
                    message = "when clause must be top level, not nested in other expressions.",
                    node = node)
            if not self.allow_guard:
                raise SyntaxError['lhs/when'](
                    message = "when clause is not allowed here.",
                    node = node)
            self.guard = y
            if x == hs.value(Void):
                return hs.value(None)
            else:
                return self.visit(x, d = d)

        elif op == "*" and x == hs.value(Void):
            if not isinstance(y, ugstr):
                raise SyntaxError['lhs/star'](
                    message = "* must qualify a variable -- not an expression.",
                    node = y)
            return build_scall(lib.struct_star, self.visit(y, d = d))

        elif op == "**" and x == hs.value(Void):
            if not isinstance(y, ugstr):
                raise SyntaxError['lhs/dstar'](
                    message = "** must qualify a variable -- not an expression.",
                    node = y)
            return build_scall(lib.struct_dstar, self.visit(y, d = None))

        elif op == "=>":
            if d is not None:
                raise SyntaxError['lhs/checker'](
                    message = "You cannot give a type or wrapper to a whole => expression (put it on the right hand side of => instead).",
                    nodes = [node, d])
            if x == hs.value(Void):
                if isinstance(y, hs.juxt):
                    x = y[-1]
                else:
                    x = y
            if not isinstance(x, (ugstr, hs.value)):
                raise SyntaxError['lhs/checker'](
                    message = "The left hand side of => must be a single variable and not an expression",
                    node = x)
            return build_scall(lib.struct_assoc, #self.hash("assoc"),
                               build_scall(lib.symbol,
                                           x if isinstance(x, hs.value) else hs2.value(x)),
                               self.visit(y, d = None))

        elif op == "." and x == hs.value(Void):
            if not isinstance(y, ugstr):
                raise SyntaxError['lhs/dot'](
                    message = "For the time being, it must be a plain variable on the right hand side of the dot and not an expression.",
                    node = y)
            return self.visit(build_scall(lib.symbol, hs.value(y)), d = d)
            # return self.visit(hs.value(y, "symbol"), d = d)

        else:
            raise SyntaxError['lhs/invalid'](
                message = ("This operator expression is invalid on the left hand side of "
                           "an assignment or declaration."),
                node = node)



class ASTRename(ASTVisitor):

    def __init__(self, env = None, rename = True):
        self.env = env or []
        self.depth = 0
        self.rename = rename
        super().__init__()

    def push(self, variables):
        self.env.append(variables)

    def pop(self):
        self.env.pop()

    def resolve(self, v):
        for vs in reversed(self.env):
            if v in vs:
                return vs[v]
        return (v, None)
        # raise Exception("Could not resolve variable", v)

    def visit_str(self, node):
        r = self.resolve(node)[0]
        if isinstance(r, str) and not isinstance(r, ugstr):
            r = ugstr(r)
        r = transfer(r, node)
        return r

    def visit_ugstr(self, node):
        r = self.resolve(node)[0]
        if isinstance(r, str) and not isinstance(r, ugstr):
            r = ugstr(r)
        r = transfer(r, node)
        return r

    def visit_UniqueVar(self, node):
        return node

    def visit_value(self, node, v):
        return node

    def visit_macro(self, node, f):
        raise SyntaxError(message = "macro in rename phase: {f}".format(f = f),
                          node = node)

    def visit_begin(self, node, *stmts):
        self.depth += 1
        changed = True
        while changed:
            changed = False
            new_stmts = []
            value = None
            for stmt in stmts:
                if isinstance(stmt, hs.begin):
                    new_stmts += stmt[:]
                    changed = True
                    value = None
                elif isinstance(stmt, hs.value):
                    value = stmt
                else:
                    new_stmts.append(stmt)
                    value = None
            if value is not None:
                new_stmts.append(value)
            stmts = new_stmts

        rval = hs.begin(*list(map(self.visit, stmts)))
        self.depth -= 1
        return rval

    def visit_seq(self, node, *stmts):
        return hs.seq(*list(map(self.visit, stmts)))

    def visit_pair(self, node, a, b):
        return hs.pair(self.visit(a), self.visit(b))

    def visit_send(self, node, obj, msg):
        return hs.send(self.visit(obj), self.visit(msg))

    def visit_if(self, node, test, ift, iff):
        return hs["if"](self.visit(test),
                        self.visit(ift),
                        self.visit(iff))

    def visit_lambda(self, node, variables, body):
        variables, body = self.with_declarations(variables, body)
        return hs2["lambda"](variables, body)

    def visit_assign(self, node, var, value):
        v, handle = self.resolve(var)
        if handle is None:
            return hs.assign(v, self.visit(value))
        else:
            return hs.assign(v, hs2.send(hs.value(lib.check),
                                         hs2.seq(self.visit(handle),
                                                 self.visit(value))))

    def visit_declaring(self, node, variables, body):
        variables, body = self.with_declarations(variables, body)
        return hs2.declaring(variables, body)

    def with_declarations(self, variables, body):
        declarations = \
            [(name, ((name if (isinstance(name, UniqueVar)
                               or (self.depth == 0 and not self.rename))
                      else transfer(UniqueVar(name), name)),
                     handler))
             for name, handler in variables]
        self.push(dict(declarations))
        rval = ([v for _, (v, h) in declarations],
                self.visit(body))
        self.pop()
        return rval

