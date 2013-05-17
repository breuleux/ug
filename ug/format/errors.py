
from .. import SyntaxError, Locations
from ..parsing.ug.ast import getloc
from ..parsing.generic.parse import Operator

error_repository = {}



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


class ErrorPrinter:
    def __init__(self, message, *locations):
        self.message = message
        self.locations = Locations(locations)
    def __descr__(self, recurse):
        return ({"assoc"},
                self.message,
                [recurse(self.locations)])


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

    message = [{"text"},
               [{"hlE"}, "Syntax Error:"],
               " No priority is defined between operators ",
               [{"hl1"}, left],
               " and ",
               [{"hl2"}, right],
               ". You may use parentheses to disambiguate."]
    locations = exc.locations
    return ErrorPrinter(message, *locations)


@associate_to(SyntaxError["missing_bracket"])
def err_priority(exc):
    message = [{"text"},
               [{"hlE"}, "Syntax Error:"]]

    if exc.close is None:
        message += [" Could not find a closing bracket for ",
                    [{"hl1"}, exc.open.name]]

    elif exc.open is None:
        message += [" Could not find an opening bracket for ",
                    [{"hl1"}, exc.close.name]]
               
    return ErrorPrinter(message, getloc(exc.node.op))


@associate_to(SyntaxError["bracket_mismatch"])
def err_priority(exc):
    message = [{"text"},
               [{"hlE"}, "Syntax Error:"],
               " mismatching brackets: ",
               [{"hl1"}, exc.open.name],
               " and ",
               [{"hl2"}, exc.close.name],
               "."]
    return ErrorPrinter(message,
                        exc.nodes[0].loc,
                        exc.nodes[1].loc)


@associate_to(SyntaxError["invalid_here"])
def err_priority(exc):
    message = [{"text"},
               [{"hlE"}, "Syntax Error:"],
               " The keyword ",
               [{"hl1"}, exc.keyword],
               " is only valid when it defines a block immediately following ",
               [{"hl2"}, ", ".join(exc.requires)],
               " blocks. Perhaps there is something in the way?"]
    return ErrorPrinter(message,
                        getloc(exc.node))

