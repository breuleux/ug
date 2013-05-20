
from .. import SyntaxError, Locations
from ..parsing.ug.ast import getloc as getloc_
from ..parsing.generic.parse import Operator
from ..compile.lib import DeconstructError, GuardError, UGTypeError, MatchError
from descr import descr

error_repository = {}


def getloc(x):
    try:
        return getloc_(x)
    except:
        return None



def process_error(error):
    for t in type(error).__mro__:
        f = error_repository.get(t, None)
        if f:
            break 
    else:
        return None
    return f(error)


def associate_to(exc):
    def wrap(f):
        error_repository[exc] = f
        return f
    return wrap


class mkmsg:
    def __init__(self, s = ""):
        self.descr = [{"text"}, s]
    def t(self, s, ws = True):
        if ws:
            self.descr.append(" ")
        self.descr.append(s)
        return self
    def add(self, cls, s, ws = True):
        if ws:
            self.descr.append(" ")
        self.descr.append([{cls}, s])
        return self
    def hl(self, s, ws = True):
        return self.add("hl", s, ws)
    def hl1(self, s, ws = True):
        return self.add("hl1", s, ws)
    def hl2(self, s, ws = True):
        return self.add("hl2", s, ws)
    def hl3(self, s, ws = True):
        return self.add("hl3", s, ws)
    def hlE(self, s, ws = True):
        return self.add("hlE", s, ws)
    def __iter__(self):
        return iter(self.descr)
    def __descr__(self, recurse):
        return self.descr

def serr(s = ""):
    return mkmsg().hlE("Syntax Error:").t(s)


class ErrorPrinter(Exception):
    def __init__(self, message, *locations, exc = None):
        self.message = message
        self.locations = locations and Locations([l for l in locations if l])
        if exc:
            self.__traceback__ = exc.__traceback__
    def __exc_descr__(self, recurse):
        if self.locations:
            return ({"assoc", "@ErrorPrinter"},
                    self.message,
                    [recurse(self.locations)])
        else:
            return ({"assoc", "@ErrorPrinter"},
                    self.message)


@associate_to(SyntaxError["priority"])
def err_priority(exc):
    l, r = exc.left, exc.right

    l_wideness_matters = False
    r_wideness_matters = False
    if exc.order(l.other_width(), r) != 0:
        l_wideness_matters = True
    if exc.order(l, r.other_width()) != 0:
        r_wideness_matters = True
    if (not l_wideness_matters and not r_wideness_matters
        and exc.order(l.other_width(), r.other_width())):
        l_wideness_matters = r_wideness_matters = True

    left = (l.fixity
            + (" " + l.width if l_wideness_matters else "")
            + " " + (l.name or "juxtaposition"))
    right = (r.fixity
             + (" " + r.width if r_wideness_matters else "")
             + " " + (r.name or "juxtaposition"))

    message = (
        serr().t("No priority is defined between operators")
        .hl1(left).t("and").hl2(right)
        .t(". You may use parentheses to disambiguate.", False))

    locations = exc.locations
    return ErrorPrinter(message, *locations)


@associate_to(SyntaxError["missing_bracket"])
def err_priority(exc):
    message = serr()

    if exc.close is None:
        message.t("Could not find a closing bracket for").hl1(exc.open.name)

    elif exc.open is None:
        message.t("Could not find an opening bracket for").hl1(exc.close.name)
               
    return ErrorPrinter(message, getloc(exc.node.op))


@associate_to(SyntaxError["bracket_mismatch"])
def err_priority(exc):
    message = (serr().t("Mismatching brackets:")
               .hl1(exc.open.name).t("and")
               .hl2(exc.close.name).t(".", False))
    return ErrorPrinter(message,
                        exc.nodes[0].loc,
                        exc.nodes[1].loc)


@associate_to(SyntaxError["invalid_here"])
def err_priority(exc):
    message = (serr().t("The keyword").hl1(exc.keyword)
               .t("is only valid when it defines a block immediately following")
               .hl2(", ".join(exc.requires))
               .t("blocks. Perhaps there is something in the way, "
                  "or the parts are in a [] or {} block."))
               
    return ErrorPrinter(message,
                        getloc(exc.node))


@associate_to(SyntaxError["prefix_macro/invalid_argument"])
def pfxm_invalid(exc):
    message = (serr().t("The prefix operator macro").hl1(exc.macro_name)
               .t("requires a list of a single argument, or "
                  "two arguments the first of which is").hl("Void").t(".", False)
               .t("For instance,").hl1(exc.macro_name + " x")
               .t("or").hl1("("+exc.macro_name+")[(), x]").t(".", False))
    return ErrorPrinter(message,
                        getloc(exc.node),
                        getloc(exc.argument))


@associate_to(SyntaxError["prefix_macro/wrong_arity"])
def pfxm_invalid(exc):
    message = (serr().t("The prefix operator macro").hl1(exc.macro_name)
               .t("requires a list of a single argument, or "
                  "two arguments the first of which is").hl("Void").t(".", False)
               .t("For instance,").hl1(exc.macro_name + " x")
               .t("or").hl1("("+exc.macro_name+")[(), x]").t(".", False))
    if exc.num == "void":
        message.hl2("2").t("arguments were given but the first is not Void.")
    else:
        message.hl2(str(exc.num)).t("arguments were given.")

    return ErrorPrinter(message,
                        getloc(exc.node[0]),
                        getloc(exc.argument))


@associate_to(SyntaxError["infix_macro/invalid_argument"])
def pfxm_invalid(exc):
    message = (serr().t("The infix operator macro").hl1(exc.macro_name)
               .t("requires two arguments neither of which are Void.")
               .t("For instance,").hl1("x " + exc.macro_name + " y")
               .t("or").hl1("("+exc.macro_name+")[x, y]").t(".", False))
    return ErrorPrinter(message,
                        getloc(exc.node),
                        getloc(exc.argument))


@associate_to(SyntaxError["infix_macro/wrong_arity"])
def pfxm_invalid(exc):
    message = (serr().t("The infix operator macro").hl1(exc.macro_name)
               .t("requires two arguments neither of which are Void.")
               .t("For instance,").hl1("x " + exc.macro_name + " y")
               .t("or").hl1("("+exc.macro_name+")[x, y]").t(".", False))
    if exc.num == "void":
        message.hl2("2").t("arguments were given but one or both is Void.")
    else:
        message.hl2(str(exc.num)).t("arguments were given.")

    return ErrorPrinter(message,
                        getloc(exc.node[0]),
                        getloc(exc.argument))


@associate_to(SyntaxError['wrong_order'])
def wrong_order(exc):
    m = (serr("In the context of").hl(exc.context)
         .t("the following order must be respected:"))
    for i, x in enumerate(exc.expected):
        if i > 0:
            m.t(",", False)
        m.add("hl", x)
    m.t(". Instead, we find:", False)
    for i, x in enumerate(exc.found):
        if i > 0:
            m.t(",", False)
        m.add("hl%s"%(i+1), x)
    m.t(".", False)

    return ErrorPrinter(m, *list(map(getloc, exc.nodes)))


@associate_to(SyntaxError)
def syntax_error(exc):
    if hasattr(exc, "location"):
        loc = exc.location
        if not isinstance(loc, (tuple, list)):
            loc = [loc]
    elif hasattr(exc, "nodes"):
        loc = list(map(getloc, exc.nodes))
    elif hasattr(exc, "node"):
        loc = [getloc(exc.node)]
    else:
        loc = []

    if hasattr(exc, "message"):
        m = exc.message
    else:
        m = str(type(exc)) + str(exc)

    return ErrorPrinter(serr().t(m), *loc)



def derr(s = ""):
    return mkmsg().hlE("Deconstruction Error:").t(s)


@associate_to(DeconstructError['not_deconstructor'])
def dec_missing_key(exc):
    m = derr().hl1(repr(exc.deconstructor))
    m.t("is not a deconstructor.")
    return ErrorPrinter(m, getloc(exc.pattern), exc = exc)

@associate_to(DeconstructError['not_deconstructible'])
def dec_missing_key(exc):
    m = derr("Could not deconstruct").hl2(repr(exc.value))
    m.t("into a tuple, dict or hybrid.")
    return ErrorPrinter(m, getloc(exc.pattern), exc = exc)

@associate_to(DeconstructError['wrong_length'])
def dec_wrong_length(exc):
    m = derr()
    e = exc.expected

    if isinstance(e, int):
        low, high = e, e
    else:
        low, high = e

    if low is None:
        m.t("At most").hl1(str(high))
    elif high is None:
        m.t("At least").hl1(str(low))
    elif low == high:
        m.t("Exactly").hl1(str(low))
    else:
        m.t("From").hl1(str(low)).t("to").hl1(str(high))

    r = exc.received
    m.t("positional argument").t(" is" if (high or low) == 1 else "s are", False)
    m.t("required, but").hl2(str(r)).t("was" if r == 1 else "were").t("found.")

    return ErrorPrinter(m, getloc(exc.pattern), exc = exc)


@associate_to(DeconstructError['missing_key'])
def dec_missing_key(exc):
    m = derr("The key")
    m.hl1(exc.key).t("was not found.")

    return ErrorPrinter(m, getloc(exc.pattern), exc = exc)


@associate_to(DeconstructError['extra_keys'])
def dec_extra_keys(exc):
    m = derr("Unexpected key/value pairs were found:")
    m.t(str(exc.keys))

    return ErrorPrinter(m, getloc(exc.pattern), exc = exc)


@associate_to(GuardError)
def guard_error(exc):
    m = mkmsg().hlE("Guard Error:").t("Guard failed.")
    return ErrorPrinter(m, getloc(exc.guard), exc = exc)




def terr(s = ""):
    return mkmsg().hlE("Type Error:").t(s)

@associate_to(UGTypeError['bad_check'])
def bad_check(exc):
    m = terr().hl1(repr(exc.checker))
    m.t("is not a type or wrapper.")
    return ErrorPrinter(m, getloc(exc.pattern), exc = exc)

@associate_to(UGTypeError['bad_type'])
def type_error(exc):
    m = terr("Expected").hl1(repr(exc.expected))
    m.t("but received").hl1(repr(exc.received))
    return ErrorPrinter(m, getloc(exc.pattern), exc = exc)

@associate_to(UGTypeError['bad_value'])
def type_error(exc):
    m = terr("Expected the value").hl1(repr(exc.expected))
    m.t("but received the value").hl1(repr(exc.received))
    return ErrorPrinter(m, exc = exc)



@associate_to(MatchError)
def type_error(exc):
    m = mkmsg().hlE("Match Error:").t("Nothing matched.")
    printers = []
    for error in exc.errors:
        error.__traceback__ = None
        error.__cause__ = None
        error.__context__ = None
        printers.append(process_error(error) or error)

    return ErrorPrinter([m] + list(map(descr, printers)),
                        exc = exc)
