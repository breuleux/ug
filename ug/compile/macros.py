
from ..parsing import decode as de, VOID, Location, SyntaxError
from ..parsing.ug.ast import ASTVisitor, transfer, getloc, setloc, hasloc
from ..lib import hashstruct as hs, ugstr, hastag, tag, struct
from .pattern import PatternBank
from .compile import hs2, ParseLHS, UniqueVar, build_scall, build_fcall
from . import lib


macro_registry = {}

def macro(name):
    def wrap(f):
        macro_registry[name] = hs.macro(f)
        return f
    return wrap


patterns = PatternBank(
    star = "*($x)",
    dstar = "**($x)",
    assoc = "($key) => ($value)",
    declare = "($dest) = ($source)",
    assoc_default = "(($key) => ($value)) = ($default)",
    default = "($x) = ($default)",

    dot = ".($x)",
    arrow = "($lhs) -> ($rhs)",
    success_arrow1 = "success -> ($rhs)",
    success_arrow2 = "success ($*lhs) -> ($rhs)",
    finally_arrow = "finally -> ($rhs)",

    compr = "($vars) in ($iter)",
    callcompr = "($fn) (($vars) in ($iter))",
    fullcompr = "(($vars) in ($iter)) -> ($body)",
    fullcallcompr = "(($fn) (($vars) in ($iter))) -> ($body)",

    elsif = "elif ($cond): $body",
    els = "else: $body"
)

def apply_patterns(matcher, node):
    name, keys = matcher(node)
    if name:
        return hs[name](**keys)
    else:
        return node


lhs_matcher = patterns.matcher('star', 'dstar', 'assoc_default', 'default', 'assoc')
dot_matcher = patterns.matcher('dot')
arrow_matcher = patterns.matcher('success_arrow1',
                                 'success_arrow2',
                                 'finally_arrow',
                                 'fullcompr',
                                 'fullcallcompr',
                                 'arrow')
compr_matcher = patterns.matcher('compr', 'callcompr')



@macro("*")
def star(self, node, args):
    if isinstance(args, hs.square):
        if args[0] == hs.value(VOID):
            return hs.star(args[1])
        else:
            return hs.send(hs.processed("*"), args)
    else:
        raise Exception("Bad args for macro *")

@macro("**")
def dstar(self, node, args):
    if isinstance(args, hs.square):
        if args[0] == hs.value(VOID):
            return hs.dstar(args[1])
        else:
            return hs.send(hs.processed("**"), args)
    else:
        raise Exception("Bad args for macro **")

@macro(".")
def dot(self, node, args):
    if args[0] == hs.value(VOID):
        return hs.value(args[1])
    else:
        raise Exception("Bad args for macro .")

@macro("#")
def hash(self, node, args):
    if args[0] == hs.value(VOID):
        arg = args[1]
        if isinstance(arg, ugstr):
            arg = hs2.value(arg)
        return hs.send(hs.value(lib.hashstruct), arg)
    else:
        raise Exception("Bad args for macro #")

@macro("?")
def index(self, node, args):
    if args[0] == hs.value(VOID):
        return hs.send(hs.value(lib.index), hs2.tuple(args[1]))
    else:
        raise Exception("Bad args for macro ?")

@macro("!")
def index(self, node, args):
    if args[0] == hs.value(VOID):
        arg = args[1]
        if isinstance(arg, hs.begin):
            return hs.square(*arg[:])
        else:
            return hs.square(arg)
    else:
        raise Exception("Bad args for macro !")

@macro("=>")
def assoc(self, node, args):
    if isinstance(args, hs.square):
        return hs.assoc(args[0], args[1])
    else:
        raise Exception("Bad args for macro =>")

@macro("test")
def test(self, node, args):

    p = ParseLHS(args)
    print(p.variables)
    print(p.deconstructor)
    print("======")

    return p.deconstructor


class ParseLHSAssign(ParseLHS):

    def __init__(self, compiler, lhs):
        self.compiler = compiler
        super().__init__(lhs)

    def visit_juxt(self, node, *args, d):
        node = self.compiler.visit(node)
        if isinstance(node, hs.send):
            self.register_variable(node, None)
            return hs.value("_")
        else:
            raise Exception("wrong lhs", node)

@macro(":=")
def assign(self, node, args):
    lhs, rhs = args[:]

    p = ParseLHSAssign(self, lhs)

    rv = transfer(UniqueVar("α"), p.deconstructor)
    instructions = [hs2.declare(rv, None),
                    hs2.assign(rv, build_fcall(p.deconstructor, rhs))]

    for i, (v, handler) in enumerate(p.variables):
        value = hs.send(rv, build_scall(lib.index, hs.value(i)))
        if isinstance(v, hs.send):
            target, message = v[:]
            instr = hs2.send(target,
                             build_fcall(hs.send(hs.value(lib.hashstruct),
                                                 hs.value("assign")),
                                         message,
                                         value))
        else:
            instr = hs2.assign(v, value)
            i += 1

        instructions.append(instr)

    return hs2.begin(*instructions)

    # return hs2.assign(lhs, rhs)

@macro("=")
def equal(self, node, args):
    lhs, rhs = args[:]
    return hs2.eqassoc(lhs, rhs)

@macro("!!")
def bangbang(self, node, args):
    expr, handler = args[:]

    arrows = []
    finally_ = None
    else_ = None

    if not isinstance(handler, hs.begin):
        instructions = [handler]
    else:
        instructions = handler[:]

    for entry in instructions:
        m = apply_patterns(arrow_matcher, entry)
        if isinstance(m, hs.arrow):
            if else_ or finally_:
                raise Exception("bad !! A")
            arrows.append(entry)
        elif isinstance(m, hs.success_arrow1):
            if else_ or finally_:
                raise Exception("bad !! B")
            else_ = ("_", m.rhs)
        elif isinstance(m, hs.success_arrow2):
            if else_ or finally_:
                raise Exception("bad !! B")
            lhs = hs.juxt(*m.lhs)
            else_ = (lhs, m.rhs)
        elif isinstance(m, hs.finally_arrow):
            if finally_:
                raise Exception("bad !! C")
            finally_ = m.rhs
        elif else_ or finally_:
            raise Exception("bad !! D")
        else:
            arrows = [hs.oper("->", "_", handler)]
            break

    rval = build_scall(lib.trycatch,
                       hs.oper("->", hs.square(), expr),
                       hs.begin(*arrows) if arrows else hs.value(None),
                       hs.oper("->",  else_[0], else_[1]) if else_ else hs.value(None),
                       hs.oper("->",  hs.square(), finally_) if finally_ else hs.value(None))
    return rval

@macro(":")
def colon(self, node, args):
    arg, body = args[:]
    if isinstance(arg, hs.juxt):
        f = arg[0]
        arg = hs2.juxt(*arg[1:])
    else:
        f = arg
        arg = setloc(hs.value(VOID), getloc(f).at_start())
    return hs.juxt(f, hs2.colonargs(arg, body))

@macro("->")
def mac_lambda(self, node, args):
    lhs, rhs = args[:]
    dispatch = apply_patterns(compr_matcher, lhs)

    if isinstance(dispatch, hs.compr):
        rval = build_scall(map,
                           hs.oper(ugstr("->"), hs.square(dispatch.vars), rhs),
                           dispatch.iter)
        return rval

    elif isinstance(dispatch, hs.callcompr):
        rval = build_fcall(dispatch.fn,
                           build_scall(map,
                                       hs.oper(ugstr("->"), hs.square(dispatch.vars), rhs),
                                       dispatch.iter))
        return rval

    else:
        pairs = [(lhs, rhs)]
        # v = message_var
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

            instructions = []
            functions = []
            for decl, body in pairs:
                p = ParseLHS(decl)
                instructions += p.instructions
                dv = UniqueVar("δ")
                instructions.append(hs2.declare(dv, None))
                instructions.append(hs2.assign(dv, p.deconstructor))
                functions.append(hs2.tuple(dv, hs2["lambda"](p.variables, #[v for v, t in p.variables],
                                                             body)))
            instructions.append(build_scall(lib.make_object, *functions))
            rval = hs2.begin(*instructions)

            return [rval] + stmts

        return hs.restmacro(find_others)


def invalid_here(**kwargs):
    def mac(self, node, args):
        raise SyntaxError['invalid_here'](
            node = node,
            args = args,
            **kwargs)
    return mac

macro("elif")(invalid_here(
        keyword = "elif",
        requires = ["if", "elif"]))

macro("else")(invalid_here(
        keyword = "else",
        requires = ["if", "elif"]))


@macro("not")
def mac_not(self, node, cond):
    return hs["if"](cond, hs.value(False), hs.value(True))

@macro("and")
def mac_and(self, node, args):
    if isinstance(args, hs.square):
        cond, rest = args[:]
        v = UniqueVar("temp")
        return hs.begin(
            hs.declare(v, None),
            hs.assign(v, cond),
            hs["if"](v, rest, v))
    else:
        raise Exception("Bad and")

@macro("or")
def mac_or(self, node, args):
    if isinstance(args, hs.square):
        cond, rest = args[:]
        v = UniqueVar("temp")
        return hs.begin(
            hs.declare(v, None),
            hs.assign(v, cond),
            hs["if"](v, v, rest))
    else:
        raise Exception("Bad or")


@macro("if")
def mac_if(self, node, args):

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
                        return hs["if"](cond, body, transfer(hs.value(None), node))
                    elif rest[0][0] is None:
                        return hs["if"](cond, body, rest[0][1])
                    else:
                        return hs["if"](cond, body, process_conditions(rest))
                return [transfer(process_conditions(conditions), node)] + stmts
            else:
                return [hs["if"](cond, iftrue, transfer(hs.value(None), node))] + stmts

        return hs.restmacro(find_else)

    elif isinstance(args, hs.square):
        args = list(args[:])
        if len(args) == 2:
            args.append(hs.value(None))
        return hs["if"](*args)

    else:
        raise Exception("bad if")


@macro("match")
def match(self, node, args):
    obj, patterns = args[:]
    return hs2.send(patterns, obj)



# [x ** x for x in [1, 2, 3, 4, 5] if x > 2]
# list x in [1, 2, 3, 4, 5] | (x > 2) -> x ** x
# list for x in [1, 2, 3, 4, 5] | (x > 2) -> x ** x

# [1, 2, 3, 4, 5].map!
#     [int x] -> x
#     [str x] -> int(x)

# l = map [1, 2, 3, 4, 5] ! [x] -> x ** 2

# m = map m ! [row] ->
#         map row ! [entry] -> entry * entry

# m = [[entry * entry for entry in row] for row in m]
# m = [row in m] -> [entry in row] -> entry * entry
# m = [m :> row] -> [row :> entry] -> entry * entry

# m = m >> row -> row >> entry -> entry * entry
# m = m map row -> row map entry -> entry * entry

# for stmt in statements:
#     print(stmt)

# statements map stmt ->
#     print[stmt]

# list ! [1, 2, 3, 4, 5] map
#     int x -> x * x
#     str x -> ...
