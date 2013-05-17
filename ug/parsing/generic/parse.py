
import re

from .location import Location, Source
from ...lib import struct, tag, ugstr
from ...tools import exc


__all__ = [
    "VOID",
    "Token", "NodeToken",
    "SubTokenizer", "subtok_rule", "Tokenizer",
    "parse_op_description", "OperatorGroup", "OperatorGroups",
    "OperatorMatrix",
    "Operator", "Bracket", "Partial", "Forward", "OpMerge", "BracketMerge",
    "OperatorParse"
    ]


class SyntaxError(exc.RichException, exc.SyntaxError):
    pass


#################
### TOKENIZER ###
#################

class VOID:
    def __str__(self):
        return "VOID"
    def __sub__(self, other):
        return -other
    def __add__(self, other):
        return +other
    def __repr__(self):
        return "VOID"
    def __descr__(self, descr):
        return ({"@VOID"}, '∅')

VOID = VOID()


class Token:
    def __init__(self, kind, args, ws, loc, **kw):
        self.kind = kind
        self.args = args
        self.ws = ws
        self.loc = loc
        self.all = [kind, args, ws]
        self.__dict__.update(kw)
    def __getitem__(self, i):
        return self.all[i]
    def __iter__(self):
        return iter(self.all)
    def __str__(self):
        return "Token[%s]" % ", ".join(map(str, self.all))
    def __repr__(self):
        return str(self)



class SubTokenizer:
    """
    SubTokenizer(rules) creates a tokenizer from various rules. Each
    rule is of the form:

        [chrs, regexp, spangroup, [take_wsb, take_wsa], description, action]

        chrs: a list of characters that trigger the rule (whitespace skipped); if True
            then all characters trigger the rule.
        regexp: a regular expression that will extract the token
        spangroup: the group number representing the token's "extent"; the length
            of the string corresponding to that group, plus the length of any whitespace
            skipped before it, will be returned as the number of characters to skip
            to get past the token.
        take_wsb: boolean; if True, then any whitespace characters will be skipped,
            and their number will be stored in the return Token. Otherwise whitespace
            is not skipped.
        take_wsa: same as take_wsb, but for the whitespace after the token.
        description: a list of things to include as the token's arguments; if
            a number is given, the string for the corresponding group in the regexp
            will be inserted. Anything else will be inserted verbatim.
        action: some action to take when reading the token. It can be either:
            ["push", subtokenizer_name] to instruct a Tokenizer to switch to some other
                SubTokenizer.
            ["pop"] to instruct a Tokenizer to switch back to the SubTokenizer it was
                using right before this one.
            These actions are usually needed to pop in and out of a string context.

        Example:
        >>> st = SubTokenizer([["abc", re.compile("((a)b)(c*)"), 0, [True, True], ["id", "hello", 1, 2], None]])
        >>> st.read(Source("  abccccc def"), 0)
        (Token[id, ['hello', 'a', 'ccccc'], [2, 1]], 4)

        i.e. a token of kind "id" with arguments "hello", "a" and
        "ccccc" (the latter two are the strings associated to groups 1
        and 2 in the regular expression "((a)b)(c*)"), which has 2
        whitespaces before (which we skipped because take_wsb is True)
        and 1 whitespace after. The number 4 corresponds to the length
        of group 0, i.e. the group "((a)b)" in the regular expression,
        plus the whitespace before the token, so after reading this
        token we will have to position ourselves on the first c before
        reading the next.

    Rules are tried in order. The first to match is returned. If pos is
    at the end of the string, [False, 0] is returned. If pos is *not* at
    the end of the string, and yet no match is found, an exception is
    raised, so the rules should cover all expected inputs.

    Provides the `read(source, pos)` method which, given a Source
    object and an integer position, returns a Token beginning at that
    position and the number of characters to skip to get past the
    token.
    """

    def __init__(self, rules, ws_re):
        self.ws_re = ws_re
        self.rules = rules
        self.rulemap = [[] for i in range(129)]
        for rule in rules:
            chars, *rest = rule
            if chars is True:
                for i in range(129):
                    self.rulemap[i].append(rest)
            else:
                for c in chars:
                    i = min(ord(c), 128)
                    self.rulemap[i].append(rest)

    def ws(self, text, pos):
        ws = self.ws_re.match(text, pos)
        s = ws.span()
        return s[1] - s[0]

    def read(self, source, pos):
        def compute(descr, groups):
            if isinstance(descr, int):
                return groups[descr]
            elif callable(descr):
                return descr(*groups)
            else:
                return descr

        text = source.text
        if pos >= len(text):
            return [False, 0]
        wsb = self.ws(text, pos)
        pos2 = pos + wsb
        rules = self.rulemap[min(ord(text[pos2]), 128)]
        for rxp, spangroup, (take_wsb, take_wsa), descr, action in rules:
            match = rxp.match(text, pos2 if take_wsb else pos)
            if match:
                groups = match.groups()
                span = match.regs[spangroup + 1]
                descr = [compute(x, groups) for x in descr]
                return (Token(descr[0], descr[1:],
                              (wsb if take_wsb else 0,
                               self.ws(text, span[1]) if take_wsa else 0),
                              loc = Location(source, span),
                              action = action),
                        (span[1] - pos))
        if pos + wsb >= len(text):
            return [False, 0]
        raise SyntaxError['no_token'](source = source,
                                      pos = pos,
                                      subtokenizer = self)


def subtok_rule(chrs, rxp, fields, span = 0, ws = (True, True), action = None):
    if isinstance(rxp, str):
        rxp = re.compile("(" + rxp + ")")
    return (chrs, rxp, span, ws, fields, action)



class Tokenizer:

    def __init__(self, source, subtok, initial_state = 'normal'):
        self.subtok = subtok
        self.source = source

        self.buffer = []
        self.buffer_pfx = True
        self.last = Token("start", (), (0, 0), loc = Location(source, (0, 0)))
        self.mark = 0
        self.stack = []
        self.st = None

        self.push_state(initial_state)

    def install_state(self, state):
        self.st = self.subtok[state]

    def push_state(self, state):
        self.stack.append(state)
        self.install_state(state)

    def pop_state(self):
        if len(self.stack) > 1:
            self.stack.pop()
            self.install_state(self.stack[-1])

    def dump_buffer(self):
        b = self.buffer
        def helper(last, i):
            if i >= len(b):
                return []
            else:
                current = b[i]
                last = last or Token("n/a", (), (0, 0), loc = Location(self.source, (0, 0)))
                t1, _, (_, wsr) = last
                t2, _, (wsl, _) = current
                pl = last.loc.end
                pr = current.loc.start
                ws = Token("infix", ("",), (wsl, wsr), loc = Location(self.source, (pl, pr)))
                void = Token("id", (VOID,), (wsl, wsr), loc = Location(self.source, (pl, pr)))
                t = t1 + "/" + t2
                if t in ["id/id"]:
                    return [ws] + helper(None, i)
                elif t in ["prefix/infix",
                           "infix/infix",
                           "infix/suffix",
                           "start/prefix",
                           "start/infix",
                           "start/suffix",
                           "infix/prefix",
                           "suffix/infix",
                           "prefix/prefix",
                           "prefix/suffix",
                           "suffix/suffix"]:
                    return [void] + helper(None, i)
                elif t in ["id/prefix"]:
                    return [ws, void] + helper(None, i)
                elif t in ["suffix/id"]:
                    return [void, ws] + helper(None, i)
                elif t in ["suffix/prefix"]:
                    return [void, ws, void] + helper(None, i)
                else:
                    return [current] + helper(current, i + 1)

        results = helper(self.last, 0)

        actions = [getattr(tok, "action", None) for tok in results]
        if [a for a in actions[:-1] if a]:
            raise SyntaxError['lookahead_action'](tokens = results,
                                                  actions = actions)
            # raise Exception("Actions associated to operators should not require lookahead")
        if actions and actions[-1]:
            action = actions[-1]
            command, *args = action
            if command == 'pop':
                self.pop_state()
            elif command == 'push':
                self.push_state(args[0])
            else:
                raise SyntaxError["unknown_action"](token = results[-1],
                                                    action = action)
                # raise Exception("Unknown command: " + str(action))

        self.buffer = []
        if results:
            self.last = results[-1]
        return results

    def process_buffer(self, pfx, sfx, start):
        n = len(self.buffer) - start
        if n == 0:
            return
        elif pfx and sfx:
            if n > 1:
                raise SyntaxError["ambiguous_nullary"](operators = self.buffer[start:])
                # raise Exception("Cannot have more than one operator in this situation.")
            elif n == 1:
                tok = self.buffer[0]
                self.buffer[0] = Token("id", ["id"] + tok.args, tok.ws, loc = tok.loc)
        elif pfx:
            for i in range(start, len(self.buffer)):
                tok = self.buffer[i]
                self.buffer[i] = Token("prefix", tok.args, tok.ws, loc = tok.loc)
        elif sfx:
            for i in range(start, len(self.buffer)):
                tok = self.buffer[i]
                self.buffer[i] = Token("suffix", tok.args, tok.ws, loc = tok.loc)
        else:
            tok = self.buffer[start]
            wsl, wsr = tok.ws
            if (wsl == wsr == 0) or (wsl > 0 and wsr > 0):
                self.buffer[start] = Token("infix", tok.args, tok.ws, loc = tok.loc)
                self.process_buffer(True, sfx, start + 1)
            elif wsl > 0:
                self.buffer[start] = Token("prefix", tok.args, tok.ws, loc = tok.loc)
                self.process_buffer(True, sfx, start + 1)
            elif wsr > 0:
                self.buffer[start] = Token("suffix", tok.args, tok.ws, loc = tok.loc)
                self.process_buffer(False, sfx, start + 1)

    def next_batch(self):
        tok, skip = self.st.read(self.source, self.mark)
        if skip:
            self.mark += skip

        assoc = {"id": (False, False),
                 "infix": (True, True),
                 "prefix": (False, True),
                 "suffix": (True, False)}

        if tok:
            if tok.kind in assoc:
                sfx, newpfx = assoc[tok.kind]
                self.process_buffer(self.buffer_pfx, sfx, 0)
                self.buffer.append(tok)
                self.buffer_pfx = newpfx
                return self.dump_buffer()

            elif tok.kind == "?fix":
                self.buffer.append(tok)
                return self.next_batch()

        elif self.buffer:
            self.process_buffer(self.pfx, True, 0)
            return self.dump_buffer()
        elif self.last and self.last.kind != "id":
            self.last = None
            return [Token("id", (VOID,), (0, 0), loc = Location(self.source, (0, 0)))]
        else:
            return False

    def __iter__(self):
        while True:
            batch = self.next_batch()
            if batch is False:
                return
            for tok in batch:
                yield tok


###########################
### OPERATOR DEFINITION ###
###########################

def parse_op_description(descr):
    m = re.match("^(X?)([ _]*)([^ _Y]*)([ _]*)(Y?)$", descr)
    x, w1, op, w2, y = m.groups()
    if op == "W":
        op = ""

    if x and y:
        fixity = "infix"
        wideness = bool(w1 and w2)

    elif x and not y:
        fixity = "suffix"
        wideness = bool(w1)

    elif not x and y:
        fixity = "prefix"
        wideness = bool(w2)

    if w1 == "_" or w2 == "_":
        return [Operator(fixity, "short", op),
                Operator(fixity, "wide", op)]
    else:
        return [Operator(fixity, "wide" if wideness else "short", op)]


class OperatorGroup:

    def __init__(self, name, *operators):
        self.name = name
        self.operators = []
        for operator in operators:
            if isinstance(operator, str):
                self.operators += parse_op_description(operator)
            else:
                self.operators.append(operator)

    def __str__(self):
        def op_to_s(o):
            if isinstance(o, tuple):
                return (o[2] or "W") + " (" + o[0][0] + o[1][0] + ")"
            else:
                return str(o)

        return "<%s: %s>" % (self.name,
                             ", ".join(map(op_to_s, self.operators)))

    def __repr__(self):
        return str(self)


class OperatorGroups:

    def __init__(self, *groups):
        self.groups = groups
        self.gnames = [g.name for g in groups]
        self.gmap = {}
        self.fns = []
        for i, g in enumerate(groups):
            for o in g.operators:
                if callable(o):
                    self.fns.append((o, i))
                else:
                    self.gmap[o] = i

    def __getitem__(self, operator):
        try:
            return self.gmap[operator]
        except KeyError:
            for f, i in self.fns:
                if f(operator):
                    return i
            raise SyntaxError['unknown_operator'](operator = operator,
                                                  groups = self)

    def __str__(self):
        return "OperatorGroups[%s]" % ", ".join(map(str, self.groups))

    def __repr__(self):
        return str(self)


class OperatorMatrix:

    def __init__(self, groups):
        self.ng = len(groups)
        self.groups = groups
        self.matrix = [[0 for g in groups]
                       for g in groups]
        self.to_index = {g: i for i, g in enumerate(groups)}

    def __getitem__(self, pair):
        return self.matrix[pair[0]][pair[1]]

    def _get_index(self, g):
        return self.to_index[g]

    def _order(self, groups, left, right):
        if left:
            for i1, i2 in zip(groups[:-1], groups[1:]):
                self.matrix[i1][i2] = -1
        if right:
            for i1, i2 in zip(groups[:-1], groups[1:]):
                self.matrix[i2][i1] = 1

    def order(self, *groups, left = True, right = True):
        self._order(list(map(self._get_index, groups)), left, right)

    def left_order(self, *groups):
        self.order(*groups, right = False)

    def right_order(self, *groups):
        self.order(*groups, left = False)

    def left_assoc(self, *groups):
        for g in groups:
            i = self._get_index(g)
            self.matrix[i][i] = -1

    def right_assoc(self, *groups):
        for g in groups:
            i = self._get_index(g)
            self.matrix[i][i] = 1

    def fill_intersection(self, l, r):
        ng = self.ng
        m = self.matrix
        for i in range(ng):
            pair = (m[l][i], m[i][r])
            if pair == (-1, -1):
                m[l][r] = -1
                return True
            elif pair == (1, 1):
                m[l][r] = 1
                return True
        return False

    def infer(self):
        # kind of a poor man's matrix multiplication
        ng = self.ng
        m = self.matrix
        while True:
            changed = False
            for i in range(ng):
                for j in range(ng):
                    if not m[i][j]:
                        changed |= self.fill_intersection(i, j)
            if not changed:
                break

    def set_relation(self, left, right, value):
        left = self._get_index(left)
        right = self._get_index(right)
        self.matrix[left][right] = value

    def __str__(self):
        width = max(map(len, self.groups))
        columns = "".rjust(width + 1) + " ".join(g[0] for g in self.groups)
        return (columns
                + "\n"
                + "\n".join(str(group).rjust(width)
                            + " "
                            + " ".join({-1: "l", 0: "-", 1: "r"}.get(col, "?")
                                       for col in row)
                            + " " + str(group)
                            for group, row in zip(self.groups, self.matrix))
                + "\n"
                + columns)


########################
### OPERATOR PARSING ###
########################


class Operator(struct):

    __prefix__ = "#!"
    __name__ = "Operator"

    def __init__(self, fixity, width, name, **others):
        super().__init__(fixity = fixity,
                         width = width,
                         name = name,
                         **others)

    def __hash__(self):
        return hash(self.fixity) ^ hash(self.width) ^ hash(self.name)

    def __eq__(self, other):
        return (isinstance(other, Operator)
                and self.fixity == other.fixity
                and self.width == other.width
                and self.name == other.name)

    def other_width(self):
        width = "short" if self.width == "wide" else "wide"
        return Operator(self.fixity, width, self.name)


class Bracket(struct):

    __prefix__ = "#!"
    __name__ = "Bracket"

    def __init__(self, type, **others):
        super().__init__(type = type,
                         **others)

    def __hash__(self):
        return hash(self.type)

    def __eq__(self, other):
        return (isinstance(other, Bracket)
                and self.type == other.type)


class Partial:
    def __init__(self, op, n, arg, loc):
        self.op = op
        self.n = n
        self.arg = arg
        self.loc = loc
    def __str__(self):
        return "Partial[%s, %s, %s]" % (self.op, self.n, self.arg)
    def __repr__(self):
        return str(self)

class NodeToken:
    def __init__(self, op, args, loc):
        self.op = op
        self.args = args
        self.loc = loc
    def __str__(self):
        return "NodeToken[%s, %s]" % (self.op, self.args)
    def __repr__(self):
        return str(self)

class Forward:
    def __init__(self, n):
        self.n = n
    def __str__(self):
        return "Forward[%s]" % self.n
    def __repr__(self):
        return str(self)

class OpMerge:
    def merge(self, left, right):
        return [left.op[1], right.op[1]]

class BracketMerge(OpMerge):
    def __init__(self, mappings):
        self.mappings = mappings

    def merge(self, left, right):
        try:
            return self.mappings[left.op[1].name + right.op[1].name]
        except KeyError as e:
            raise SyntaxError["bracket_mismatch"](
                open = left.op[1],
                close = right.op[1],
                nodes = [left, right]
                )


class OperatorParse:

    def __init__(self, source, tokenizer, groups, matrix):
        self.source = source
        self.tokenizer = tokenizer
        self.groups = groups
        self.matrix = matrix

        self.partials = []
        self.current_token = None

        self.leftn = -1
        self.rightn = -1
        self.done = False

        self.result = None

    def order(self, left, right):
        fx = (left.fixity, right.fixity)
        if (fx == ('infix', 'prefix') or
            fx == ('prefix', 'prefix')):
            return 1
        elif (fx == ('suffix', 'infix') or
              fx == ('suffix', 'suffix')):
            return -1
        else:
            value = self.matrix[self.groups[left], self.groups[right]]
            if callable(value):
                value = value(left, right)
            return value

    def assimilate_to_left(self, token, left, rightn):
        left.arg[1] = token
        left.n[1] = rightn
        self.finalize(left)

    def assimilate_to_right(self, token, right, leftn):
        right.arg[0] = token
        right.n[0] = leftn
        self.finalize(right)

    def assimilate_to_both(self, token, left, right, leftn, rightn, newop):
        self.partials[leftn] = Forward(rightn)
        new_partial = Partial([left.op[0], newop, right.op[2]],
                              [left.n[0], right.n[1]],
                              [left.arg[0], right.arg[1], token],
                              token.loc + left.loc + right.loc)
        self.partials[rightn] = new_partial
        self.finalize(new_partial)

    def assimilate_current(self):
        token = self.current_token
        self.current_token = None
        self.assimilate(token, self.leftn, self.rightn)

    def assimilate(self, token, leftn, rightn):
        # print("assim", token, leftn, rightn)
        if rightn < 0 and not self.done:
            if self.current_token:
                raise exc.Exception['impossible/curtok'](
                    "There shouldn't be a current token.")
            else:
                self.leftn = leftn
                self.rightn = -1
                self.current_token = token
        else:
            self.force_assimilate(token, leftn, rightn)

    def force_assimilate(self, token, leftn, rightn):
        left = self.partials[leftn] if leftn >= 0 else None
        right = self.partials[rightn] if rightn >= 0 else None

        if isinstance(left, Forward):
            return self.force_assimilate(token, left.n, rightn)
        if isinstance(right, Forward):
            return self.force_assimilate(token, leftn, right.n)

        # print(":", token, leftn, rightn, left, right)

        if not left and not right:
            self.result = token
        elif not left:
            self.assimilate_to_right(token, right, leftn)
        elif not right:
            self.assimilate_to_left(token, left, rightn)
        else:
            # order = self.matrix[self.groups[left.op[2]], self.groups[right.op[0]]]
            lop, rop = left.op[2], right.op[0]
            order = self.order(lop, rop)
            if order == -1:
                self.assimilate_to_left(token, left, rightn)
            elif order == 0:
                raise SyntaxError['priority'](left = lop,
                                              right = rop,
                                              locations = [left.loc, right.loc],
                                              order = self.order,
                                              parser = self)
                # raise Exception("priority error", left.op[2], right.op[0])
            elif order == 1:
                self.assimilate_to_right(token, right, leftn)
            elif isinstance(order, OpMerge):
                self.assimilate_to_both(token, left, right, leftn, rightn, order.merge(left, right))
            else:
                raise exc.Exception['order_specification'](
                    "Unknown order specification",
                    dict(order = order,
                         left = lop,
                         right = rop,
                         parser = self))
                # raise Exception("unknown", order)

    def finalize(self, partial):
        l, r = partial.arg[0], partial.arg[1]
        if l and r:
            self.assimilate(NodeToken(tag(partial.op[1], "location", partial.loc),
                                      partial.arg,
                                      partial.loc + l.loc + r.loc),
                            partial.n[0],
                            partial.n[1])

    def process(self, token):
        # print('process', token)
        if token.kind == 'id':
            self.assimilate(token, len(self.partials) - 1, -1)
        elif token.kind in ['infix', 'prefix', 'suffix']:
            if token.kind == 'infix':
                op = Operator('infix', 'wide' if token.ws[0] or token.ws[1] else 'short', token.args[0])
            elif token.kind == 'prefix':
                op = Operator('prefix', 'wide' if token.ws[1] else 'short', token.args[0])
            elif token.kind == 'suffix':
                op = Operator('suffix', 'wide' if token.ws[0] else 'short', token.args[0])
            partial = Partial(op = [op, op, op],
                              n = [None, None],
                              arg = [None, None],
                              loc = token.loc)
            self.partials.append(partial)
            self.rightn = len(self.partials) - 1
            self.assimilate_current()
        else:
            raise exc.Exception['unknown_fixity'](
                token = token,
                parser = self)

    def parse(self):
        for tok in self.tokenizer:
            self.process(tok)
        self.rightn = -1
        self.done = True
        self.assimilate_current()
        return self.result
