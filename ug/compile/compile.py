
import ast as pyast
from functools import reduce

from .pattern import PatternBank
from ..parsing import decode as de, VOID
from ..parsing.ug.ast import ASTVisitor, transloc, getloc, setloc
from ..lib import hashstruct as hs, ugstr, hastag
pycompile = compile

# t_juxt = hs["juxt"]
# t_macro = hs["macro"]
# t_splice = hs["splice"]

# t_revisit = hs["revisit"]

def revisit(x):
    return hs.revisit(x)

patterns = PatternBank(
    star = "*($x)",
    dstar = "**($x)",
    assoc = "($key) => ($value)",
    declare = "($dest) = ($source)",
    assoc_default = "(($key) => ($value)) = ($default)",
    default = "($x) = ($default)",

    dot = ".($x)",
    arrow = "($lhs) -> ($rhs)",

    elsif = "elif ($cond): $body",
    els = "else: $body"
)

def apply_patterns(matcher, node):
    name, keys = matcher(node)
    if name:
        return hs[name](**keys)
    else:
        return node


class HS2:
    def __getattr__(self, attr):
        f = getattr(hs, attr)
        def f2(*args):
            loc = reduce(lambda x, y: x + y,
                         [getloc(arg) for arg in args if hastag(arg, "location")],
                         None)
            result = f(*args)
            if loc is None:
                return result
            else:
                return setloc(result, loc)
        return f2
    def __getitem__(self, item):
        return getattr(self, item)

hs2 = HS2()


class Recipes:

    def r_check_equal(self, value):
        return hs2.send(hs.special("check_equal"),
                        hs2.tuple(value))

    def r_deconstruct(self, deconstructor, value):
        if deconstructor is None:
            deconstructor = hs.value(None)
        return hs2.send(hs.special("deconstruct"),
                        hs2.tuple(deconstructor, value))

    def r_extract_tuple(self, v, minlen, maxlen):
        if isinstance(minlen, (int, type(None))):
            minlen = hs.value(minlen)
        if isinstance(maxlen, (int, type(None))):
            maxlen = hs.value(maxlen)
        return hs2.send(hs.special("extract_tuple"),
                        hs2.tuple(v, minlen, maxlen))

    def r_tuple_index(self, v, idx):
        if isinstance(idx, int):
            idx = hs.value(idx)
        return hs2.send(hs.special("tuple_index"),
                        hs2.tuple(v, idx))

    def r_extract_dict(self, v):
        return hs2.send(hs.special("extract_dict"),
                        hs2.tuple(v))

    def r_extract_dict_copy(self, v):
        return hs2.send(hs.special("extract_dict_copy"),
                        hs2.tuple(v))

    def r_dict_index(self, v, idx):
        if isinstance(idx, str):
            idx = hs.value(idx)
        return hs2.send(hs.special("dict_index"),
                        hs2.tuple(v, idx))

    def r_dict_pop(self, v, idx):
        if isinstance(idx, str):
            idx = hs.value(idx)
        return hs2.send(hs.special("dict_pop"),
                        hs2.tuple(v, idx))

    def r_dict_empty(self, v):
        return hs2.send(hs.special("dict_empty"),
                        hs2.tuple(v))

    def r_tuple_range(self, v, start, end):
        if isinstance(start, int) or start is None:
            start = hs.value(start)
        if isinstance(end, int) or end is None:
            end = hs.value(end)
        return hs2.send(hs.special("tuple_range"),
                        hs2.tuple(v, start, end))


recipes = Recipes()
def recipe(name, *args):
    return getattr(recipes, 'r_'+name)(*args)



def accumulate_sequence(compiler, entries, type, expand_eqassoc = True):
    results = []
    for i, entry in enumerate(entries):
        newentry = compiler.svisit(entry)
        if isinstance(newentry, hs.splice):
            if type in getattr(newentry, "expand_in", ["begin"]):
                # print(newentry[:])
                # print(accumulate_sequence(compiler, newentry[:], type))
                results += accumulate_sequence(compiler, newentry[:], type, expand_eqassoc)
            else:
                results.append(compiler.xvisit(newentry))
        elif isinstance(newentry, hs.restmacro):
            f = newentry[0]
            results += accumulate_sequence(compiler, f(newentry, hs[type](*entries[i+1:])), type, expand_eqassoc)
            break
        elif expand_eqassoc and isinstance(newentry, hs.eqassoc):
            lhs, rhs = newentry[:]

            instructions = []
            # print([len(x) for x in parse_lhs(None, lhs, rhs)])

            for deconstructor, deconstruct, var, value in parse_lhs(None, lhs, rhs):

                if deconstruct:
                    instructions.append(hs2.declare(var, None))
                    instructions.append(hs2.assign(var, recipe("deconstruct", deconstructor, value)))
                elif var:
                    instructions.append(hs2.declare(var, deconstructor))
                    instructions.append(hs2.assign(var, value))


                elif deconstructor:
                    instructions.append(hs2.send(hs.special("check"),
                                                 hs2.tuple(deconstructor,
                                                           value)))
                else:
                    instructions.append(value)

            results += accumulate_sequence(compiler, instructions, type, False)

        else:
            results.append(newentry)
    return results

def parse_sequence(compiler, seq):

    lparts = [[]]
    dparts = [[]]

    for arg in seq:
        if isinstance(arg, hs.star):
            lparts.append(compiler.xvisit(arg[0]))
            lparts.append([])
        elif isinstance(arg, hs.dstar):
            dparts.append(compiler.xvisit(arg[0]))
            dparts.append([])
        elif isinstance(arg, hs.assoc):
            k = compiler.xvisit(arg[0])
            v = compiler.xvisit(arg[1])
            dparts[-1].append((k, v))
        elif isinstance(arg, hs.eqassoc):
            lhs, rhs = arg[:]

            if isinstance(lhs, ugstr):
                dparts[-1].append((hs2.value(lhs),
                                   compiler.xvisit(rhs)))
            else:
                instructions = []
                variables = []
                # print([len(x) for x in parse_lhs(None, lhs, rhs)])

                # Use rhsv instead of rhs to avoid name capture
                rhsv = transloc(UniqueVar("temp"), rhs)

                for deconstructor, deconstruct, var, value in parse_lhs(None, lhs, rhsv):
                    if isinstance(var, ugstr):
                        variables.append(var)

                    if deconstruct:
                        instructions.append(hs2.declare(var, None))
                        instructions.append(hs2.assign(var, recipe("deconstruct", deconstructor, value)))
                    elif var:
                        instructions.append(hs2.declare(var, deconstructor))
                        instructions.append(hs2.assign(var, value))

                    elif deconstructor:
                        instructions.append(hs2.send(hs.special("check"),
                                                     hs2.tuple(deconstructor,
                                                               value)))
                    else:
                        instructions.append(value)


                # print(variables)

                instructions.append(hs2.square(*[hs.assoc(hs2.value(variable), variable)
                                                 for variable in variables]))

                dparts.append(compiler.xvisit(
                        hs2.begin(hs2.eqassoc(rhsv, rhs), # rhsv <- rhs
                                  hs2.begin(*instructions))))
                dparts.append([])

        else:
            lparts[-1].append(arg)

    return ([x for x in lparts if x],
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
                res = transloc(res[0], res)
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

    def visit_UniqueVar(self, node):
        return node

    def visit_value(self, node, v):
        return node

    def visit_juxt(self, node, *args):
        if len(args) == 0:
            return transloc(hs.value(VOID), node)
        if len(args) == 1:
            return self.xvisit(args[0])
        f, arg, *rest = args
        if rest:
            return revisit(hs.juxt(hs2.juxt(f, arg), *rest))
        f = self.xvisit(f)
        if isinstance(f, hs.macro):
            return revisit(f[0](node, arg))
        else:
            arg = self.xvisit(arg)
            return hs.send(f, arg)

    def visit_oper(self, node, op, a, b):
        return revisit(hs.juxt(op, hs2.square(a, b)))

    def visit_begin(self, node, *args):
        # print("a", node)
        newargs = accumulate_sequence(self, args, "begin")
        # print("b")
        # print(newargs)

        declarations = []
        arguments = []
        for arg in newargs:
            if isinstance(arg, hs.declare):
                if arg[1] is not None:
                    temp = transloc(UniqueVar(arg[0]+".t"), arg[1])
                    declarations.append((temp, None))
                    declarations.append((arg[0], temp))
                    arguments.append(hs2.assign(temp, arg[1]))
                else:
                    declarations.append((arg[0], None))
            else:
                arguments.append(arg)

        objs = []
        loc = None
        # while arguments and isinstance(arguments[-1], hs.object):
        #     o = arguments.pop()
        #     if loc:
        #         loc += getloc(o)
        #     else:
        #         loc = getloc(o)
        #     if o[0] is not message_var:
        #         raise Exception("object variable should be %s" % message_var)
        #     objs = list(o[1:]) + objs
        # if objs:
        #     arguments.append(setloc(hs.object(message_var, *objs), loc))

        stmts = hs.begin(*arguments)
        if declarations:
            return hs2.declaring(tuple(declarations), stmts)
        else:
            return stmts

    def visit_square(self, node, *args):
        newargs = accumulate_sequence(self, args, "square", False)
        t, d = parse_sequence(self, newargs)
        if t:
            if len(t) == 1 and isinstance(t[0], list):
                t = hs2.tuple(*t[0])
            else:
                t = hs.send(hs.special("patch_tuple"),
                            hs2.tuple(*[hs2.tuple(*x) if isinstance(x, list) else x
                                        for x in t]))
        if d:
            def mkdict(kvs):
                return hs.send(hs.special("make_dict"),
                               hs2.tuple(*[hs2.tuple(x[0], x[1]) for x in kvs]))
            if len(d) == 1 and isinstance(d[0], list):
                d = mkdict(d[0])
            else:
                d = hs.send(hs.special("patch_dict"),
                            hs2.tuple(*[mkdict(x) if isinstance(x, list) else x
                                        for x in d]))
        if t and d:
            return hs.send(hs.special("make_hybrid"),
                           hs2.tuple(t, d))
        elif t:
            return t
        elif d:
            return d
        else:
            return hs.tuple()


        # parts = [[]]
        # for arg in newargs:
        #     if isinstance(arg, hs.star):
        #         parts.append(self.xvisit(arg[0]))
        #         parts.append([])
        #     else:
        #         parts[-1].append(arg)
        # if len(parts) == 1:
        #     return hs.tuple(*parts[0])
        # else:
        #     return hs.send(hs.special("make_tuple"),
        #                    hs.tuple(*[hs2.tuple(*x) if isinstance(x, list) else x
        #                               for x in parts if x]))

    def visit_curly(self, node, *args):
        newargs = accumulate_sequence(self, args, "curly")
        return hs.curly(*newargs)

    def visit_send(self, node, obj, msg):
        return hs.send(self.xvisit(obj), self.xvisit(msg))

    def visit_object(self, node, v, *specs):
        return hs.object(v, *[[self.xvisit(dec),
                               self.xvisit(body)]
                              for dec, body in specs])

    def visit_tuple(self, node, *args):
        return hs.tuple(*map(self.xvisit, args))

    def visit_if(self, node, cond, iftrue, iffalse):
        return hs["if"](self.xvisit(cond), self.xvisit(iftrue), self.xvisit(iffalse))

    def visit_assign(self, node, var, value):
        return self.svisit(node)

    def visit_special(self, node, value):
        return node

    def visit_colonargs(self, node, *args):
        raise Exception("#colonargs not valid here")

    def visit_generic(self, node):
        raise Exception("Unknown node", node)




class ExprCompiler(Compiler):

    def visit_splice(self, node, *args):
        return revisit(hs.begin(*args))

    def visit_star(self, node, arg):
        raise Exception("Bad * context")

    def visit_dstar(self, node, arg):
        raise Exception("Bad ** context")

    def visit_assoc(self, node, k, v):
        raise Exception("Bad => context")

    def visit_eqassoc(self, node, lhs, rhs):
        raise Exception("Bad = context")

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
        return revisit(f(node, hs.value(VOID)))

    def visit_restmacro(self, node, value):
        return node

    def visit_declare(self, node, var, constructor):
        return hs2.declare(var, (constructor
                                 and self.xvisit(constructor)))
    
    def visit_assign(self, node, var, value):
        return hs2.assign(var and self.xvisit(var),
                          self.xvisit(value))




def mac_star(node, args):
    if isinstance(args, hs.square):
        if args[0] == hs.value(VOID):
            return hs.star(args[1])
        else:
            return hs.send(hs.special("*"), args)
    else:
        raise Exception("Bad args for macro *")

def mac_dstar(node, args):
    if isinstance(args, hs.square):
        if args[0] == hs.value(VOID):
            return hs.dstar(args[1])
        else:
            return hs.send(hs.special("**"), args)
    else:
        raise Exception("Bad args for macro **")

def mac_dot(node, args):
    if args[0] == hs.value(VOID):
        return hs.value(args[1])
    else:
        raise Exception("Bad args for macro .")

def mac_hash(node, args):
    if args[0] == hs.value(VOID):
        arg = args[1]
        if isinstance(arg, ugstr):
            arg = hs2.value(arg)
        return hs.send(hs.special("hashstruct"), arg)
    else:
        raise Exception("Bad args for macro #")

def mac_bang(node, args):
    if args[0] == hs.value(VOID):
        return hs.send(hs.special("index"), hs2.tuple(args[1]))
    else:
        raise Exception("Bad args for macro !")

# def mac_bangdot(node, args):
#     if args[0] == hs.value(VOID):
#         return hs.send(hs.special("index"), hs2.tuple(hs2.value(args[1])))
#     else:
#         raise Exception("Bad args for macro !")

def mac_assoc(node, args):
    if isinstance(args, hs.square):
        return hs.assoc(args[0], args[1])
    else:
        raise Exception("Bad args for macro =>")



def mac_trivial(node, args):
    return hs.value("Macro worked")

def mac_test(node, args):
    # args = [ugstr("a"), ugstr("b"), hs.splice(ugstr("c"), ugstr("d"), expand_in = ["begin", "square"]), ugstr("e")]
    # rval = hs.splice(*args)
    # return rval

    # def f(node):
    #     print(node)
    #     return map(hs.value, node[:])
    # return hs.restmacro(f)

    return args



# _gensym_idx = 0
# def gensym(name):
#     _gensym_idx += 1
#     return "___" + name + _gensym_idx

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

message_var = UniqueVar("message")


# class Categorizer:
#     def __init__(self, **rules):
#         pass


# Categorizer(
#     star = hs.rule(lambda x: isinstance(x, hs.star))


def partition(f, l):
    a = []
    b = []
    for entry in l:
        if f(entry):
            a.append(entry)
        else:
            b.append(entry)
    return (a, b)


lhs_matcher = patterns.matcher('star', 'dstar', 'assoc_default', 'default', 'assoc')
dot_matcher = patterns.matcher('dot')

def parse_lhs(deconstructor, lhs, rhs):

    if isinstance(lhs, (ugstr, UniqueVar)):
        if lhs == '_':
            return [(deconstructor, False, None, rhs)]
        else:
            return [(deconstructor, False, lhs, rhs)]

    elif isinstance(lhs, hs.value):
        return [(recipe("check_equal", lhs), False, None, rhs)]

    elif isinstance(lhs, hs.oper):
        lhs2 = apply_patterns(dot_matcher, lhs)
        return [(recipe("check_equal", hs.value(lhs2.x)), False, None, rhs)]

    elif isinstance(lhs, hs.square):

        lhs2 = [apply_patterns(lhs_matcher, x) for x in lhs[:]]

        d, t = partition(lambda x: (isinstance(x, hs.dstar)
                                    or isinstance(x, hs.assoc)
                                    or isinstance(x, hs.assoc_default)),
                         lhs2)

        temp = transloc(UniqueVar("temp"), lhs)
        results = [(deconstructor, True, temp, rhs)]

        if t:
            ttemp = transloc(UniqueVar("ttemp"), temp)
            lt = len(t)
            # print([bool(x) for x in t if isinstance(x, hs.star)])
            if any(isinstance(x, hs.star) for x in t):
                minlen, maxlen = lt - 1, None
            else:
                minlen, maxlen = lt, lt
            # results.append((hs.special("extract_tuple"), False, ttemp, temp, lt_nostar))
            results.append((None, False, ttemp, recipe("extract_tuple", temp, minlen, maxlen)))
            star = False
            for i, entry in enumerate(t):
                # print(i, entry)
                if isinstance(entry, hs.star):
                    if star:
                        raise Exception("Should be no more than one *")
                    star = True
                    if not isinstance(entry.x, ugstr):
                        raise Exception("Invalid * variable", entry.x)
                    results.append((None,
                                    False,
                                    None if entry.x == '_' else entry.x,
                                    recipe("tuple_range",
                                           ttemp,
                                           hs.value(i),
                                           hs.value(-lt+i+1) if i < lt-1 else None)))
                elif isinstance(entry, hs.default):
                    if star:
                        raise Exception("No default values after *")
                    results += parse_lhs(None, entry.x, recipe("tuple_index",
                                                               ttemp,
                                                               hs.value(i),
                                                               entry.default))
                else:
                    index = hs.value(-lt+i) if star else hs.value(i)
                    results += parse_lhs(None, entry, recipe("tuple_index",
                                                             ttemp,
                                                             index))


        if d:
            dtemp = transloc(UniqueVar("dtemp"), temp)
            # results.append((hs.special("extract_dict"), False, dtemp, temp))
            is_dstar = any(isinstance(x, hs.dstar) for x in d)
            # if is_dstar:
            results.append((None, False, dtemp, recipe("extract_dict_copy", temp)))
            # else:
            #     results.append((None, False, dtemp, recipe("extract_dict", temp)))
            dstar = False
            for entry in d:
                if isinstance(entry, hs.dstar):
                    if dstar:
                        raise Exception("Should be no more than one **")
                    if not isinstance(entry.x, ugstr):
                        raise Exception("Invalid * variable", entry.x)
                    dstar = True
                    results.append((None,
                                    False,
                                    None if entry.x == '_' else entry.x,
                                    dtemp))
                elif isinstance(entry, hs.assoc):
                    k, v = entry.key, entry.value
                    if k == hs.value(VOID):
                        k = v
                    if not isinstance(k, ugstr):
                        raise Exception("Invalid key")
                    # if is_dstar:
                    results += parse_lhs(None, v, recipe("dict_pop", dtemp, hs.value(k)))
                    # else:
                    #     results += parse_lhs(None, v, recipe("dict_index", dtemp, hs.value(k)))
                elif isinstance(entry, hs.assoc_default):
                    k, v, default = entry.key, entry.value, entry.default
                    if k == hs.value(VOID):
                        k = v
                    if not isinstance(k, ugstr):
                        raise Exception("Invalid key")
                    # if is_dstar:
                    results += parse_lhs(None, v, recipe("dict_pop", dtemp, hs2.value(k), default))
                        # results += parse_lhs(None, v, recipe("dict_index", dtemp, hs2.value(k), default))
                else:
                    raise Exception("Invalid dict entry", entry)

            if not is_dstar:
                results.append((None, False, None, recipe("dict_empty", dtemp)))

        return results

    elif isinstance(lhs, hs.juxt):
        *typ, var = lhs[:]
        return parse_lhs(hs2.juxt(*typ), var, rhs)

    else:
        raise Exception("invalid lhs", lhs)


def mac_assign(node, args):
    lhs, rhs = args[:]
    return hs2.assign(lhs, rhs)


# def mac_lambda(node, args):
#     lhs, rhs = args[:]
#     # v = UniqueVar("message")
#     v = message_var
#     return hs2.object(v, [hs2.begin(hs2.eqassoc(lhs, v)), rhs])


def mac_equal(node, args):
    lhs, rhs = args[:]
    return hs2.eqassoc(lhs, rhs)

    #     instructions.append(hs.declare(var, deconstructor))


    #     # if deconstructor and deconstruct:
    #     #     value = hs.recipe("deconstruct", deconstructor, value)
    #     # elif deconstructor and not deconstruct:
    #     #     value = hs.recipe("typetest", deconstructor, value)

    # for entry in parse_lhs(None, lhs, rhs):
    #     print(entry)
    # return hs.value(123)



def mac_bangbang(node, args):
    expr, handler = args[:]
    return hs.catch(expr, handler, None, None)


def mac_colon(node, args):
    arg, body = args[:]
    if isinstance(arg, hs.juxt):
        f = arg[0]
        arg = hs2.juxt(*arg[1:])
    else:
        f = arg
        arg = setloc(hs.value(VOID), getloc(f).at_start())
    return hs.juxt(f, hs2.colonargs(arg, body))


arrow_matcher = patterns.matcher('arrow')
def mac_lambda(node, args):
    lhs, rhs = args[:]
    pairs = [(lhs, rhs)]
    v = message_var
    def find_others(node2, rest):
        stmts = list(rest[:])
        if isinstance(rest, hs.begin):
            while stmts:
                stmt = stmts[0]
                m = apply_patterns(arrow_matcher, stmt)
                if isinstance(m, hs.arrow):
                    stmts = stmts[1:]
                    pairs.append((m.lhs, m.rhs))
                else:
                    break
        rval = transloc(hs2.object(v, *[(hs2.begin(hs2.eqassoc(lhs, v)), rhs)
                                        for lhs, rhs in pairs]),
                        node)
        return [rval] + stmts

    return hs.restmacro(find_others)


def mac_if(node, args):

    if isinstance(args, hs.colonargs):
        cond, iftrue = args[:]

        def find_else(node2, rest):
            stmts = list(rest[:])
            if isinstance(rest, hs.begin):
                conditions = [(cond, iftrue)]
                while stmts:
                    stmt = stmts[0]
                    m = patterns["els"].match(stmt)
                    if m:
                        conditions.append((None, m["body"]))
                        stmts = stmts[1:]
                        break
                    m = patterns["elsif"].match(stmt)
                    if m:
                        conditions.append((m["cond"], m["body"]))
                        stmts = stmts[1:]
                        continue
                    break
                def process_conditions(conditions):
                    (cond, body), *rest = conditions
                    if not rest:
                        return hs["if"](cond, body, transloc(hs.value(None), node))
                    elif rest[0][0] is None:
                        return hs["if"](cond, body, rest[0][1])
                    else:
                        return hs["if"](cond, body, process_conditions(rest))
                return [transloc(process_conditions(conditions), node)] + stmts
            else:
                return [hs["if"](cond, iftrue, transloc(hs.value(None), node))] + stmts

        return hs.restmacro(find_else)

    elif isinstance(args, hs.square):
        args = list(args[:])
        if len(args) == 2:
            args.append(hs.value(None))
        return hs["if"](*args)

    else:
        raise Exception("bad if")





class ASTRename(ASTVisitor):

    def __init__(self, env = None):
        self.env = env or []
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

    def visit_ugstr(self, node):
        return self.resolve(node)[0]

    def visit_UniqueVar(self, node):
        return node

    def visit_value(self, node, v):
        return node

    def visit_begin(self, node, *stmts):
        return hs.begin(*list(map(self.visit, stmts)))

    def visit_tuple(self, node, *stmts):
        return hs.tuple(*list(map(self.visit, stmts)))

    def visit_send(self, node, obj, msg):
        return hs.send(self.visit(obj), self.visit(msg))

    def visit_if(self, node, test, ift, iff):
        return hs["if"](self.visit(test),
                        self.visit(ift),
                        self.visit(iff))

    def visit_object(self, node, v, *specs):
        new = []
        for dec, body in specs:
            if isinstance(dec, hs.declaring):
                vs, assigns = dec[:]
                merger = self.visit(hs.declaring(vs, hs.begin(assigns, body)))
                newvs, temp = merger[:]
                newassigns, newbody = temp[:]
                new.append([hs2.declaring(newvs, newassigns), newbody])
            else:
                new.append([self.visit(dec), self.visit(body)])
        return hs.object(v, *new)

    def visit_assign(self, node, var, value):
        if isinstance(var, (ugstr, UniqueVar)):
            v, handle = self.resolve(var)
            if handle is None:
                return hs.assign(v, self.visit(value))
            else:
                return hs.assign(v, hs2.send(hs.special("check"),
                                             hs2.tuple(self.visit(handle),
                                                       self.visit(value))))
        else:
            var = self.visit(var)
            if isinstance(var, hs.send):
                var, item = var[:]
                return hs2.send(hs.special("assign"),
                                hs2.tuple(self.visit(var),
                                          self.visit(item),
                                          self.visit(value)))
            else:
                raise Exception("Invalid lhs", var)

    def visit_declaring(self, node, variables, body):
        declarations = \
            [(name, ((name if isinstance(name, UniqueVar) else transloc(UniqueVar(name), name)),
                     handler))
             for name, handler in variables]
        self.push(dict(declarations))
        rval = hs.declaring([v for _, (v, h) in declarations],
                            self.visit(body))
        self.pop()
        return rval


def compile(ast):
    c = ExprCompiler(
        {},
        {".": hs.macro(mac_dot),
         "#": hs.macro(mac_hash),
         "!": hs.macro(mac_bang),
         ">": hs.special(">"),
         "<": hs.special("<"),
         ">=": hs.special(">="),
         "=<": hs.special("=<"),
         "==": hs.special("=="),
         "!=": hs.special("!="),
         "+": hs.special("+"),
         "-": hs.special("-"),
         "/": hs.special("/"),
         "*": hs.macro(mac_star),
         "**": hs.macro(mac_dstar),
         "=>": hs.macro(mac_assoc),
         "=": hs.macro(mac_equal),
         ":=": hs.macro(mac_assign),
         ":": hs.macro(mac_colon),
         "->": hs.macro(mac_lambda),
         "!!": hs.macro(mac_bangbang),
         "id": hs.macro(mac_trivial),
         "test": hs.macro(mac_test),
         "if": hs.macro(mac_if)
         })
    if not isinstance(ast, hs.begin):
        ast = hs.begin(ast)
    res = c.visit(ast)
    # print(res)
    res = ASTRename().visit(res)
    return res


#lambda[v, #declaring[variables, expr], body]
#lambdas[#lambda...]


# print(dir(pyast))

# # ast = compile("1 + 2 + 3", "w/e", "eval", pyast.PyCF_ONLY_AST)

# # print(ast)

# ast = pyast.Expression(pyast.BinOp(pyast.Num(1.7), pyast.Add(), pyast.Num(2)))
# ast = pyast.fix_missing_locations(ast)

# code = compile(ast, "w/e", "eval")
# print(eval(code))

