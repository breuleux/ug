
import re
from .codec import decode as de
from ..generic.parse import SubTokenizer, Tokenizer, subtok_rule, Token, VOID, SyntaxError


# Characters that can be the first character of an identifier.

chr_id_lead = de(r"""
a b c d e f g h i j k l m n o p q r s t u v w x y z
A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
_

`aac` `eac` `iac` `oac` `uac` `yac`
`Aac` `Eac` `Iac` `Oac` `Uac` `Yac`
`agr` `egr` `igr` `ogr` `ugr`
`Agr` `Egr` `Igr` `Ogr` `Ugr`
`aci` `eci` `ici` `oci` `uci`
`Aci` `Eci` `Ici` `Oci` `Uci`
`aum` `eum` `ium` `oum` `uum` `yum`
`Aum` `Eum` `Ium` `Oum` `Uum` `Yum`
`ati` `nti` `oti`
`Ati` `Nti` `Oti`
`aring` `Aring`

`alpha` `beta` `gamma` `delta` `epsilon` `zeta` `eta` `theta`
`lambda` `mu` `xi` `pi` `sigma` `tau` `phi` `chi` `psi` `omega`
`Gamma` `Delta` `Theta`
`Lambda` `Xi` `Pi` `Sigma` `Phi` `Psi` `Omega`

`dagger` `eth` `aelig` `oelig` `ccedil` `scaron` `thorn` `oslash`
`Dagger` `ETH` `AElig` `OElig` `Ccedil` `Scaron` `THORN` `Oslash`

`degree` `infinity` `empty`
""").split()

chr_id = chr_id_lead + "0 1 2 3 4 5 6 7 8 9".split()


# Characters that define operators
chr_op = de(r"""
.
+ - * / ~ ^ < > = : ? % # $ @
`union` `intersection`
`subset` `subseteq` `supset` `supseteq`
`in` `notin`
`after` `cw` `ccw`
`hearts` `spades` `clubs`
`to`
<> =< >= -> ~~ ~= /=

: & |
`and` `or` `not` `nand` `nor` `xor`
`gradient` `integral`
<- ->
<= =>
""").split()


### REJECTIONS ###

# Rejected for cause of being too similar to other letters:
# left, the rejected character, right, the character it is too similar to
# this list is used to help the user if they use rejected characters
chr_reject = de(r"""
`iota` i
`kappa` k
`nu` v
`omicron` o
`rho` p
`upsilon` u
`Alpha` A
`Beta` B
`Epsilon` E
`Zeta` Z
`Eta` H
`Iota` I
`Kappa` K
`Mu` M
`Nu` N
`Omicron` O
`Rho` P
`Tau` T
`Upsilon` Y
`Chi` X
""").split("\n")
chr_reject = dict(x.split() for x in chr_reject if x)


def rx_choice(chars, negate = False):
    return "[" + ("^" if negate else "") + "".join(map(re.escape, chars)) + "]"

def rx_without(oper, banned):
    rx = "(" + oper + ")($|" + rx_choice(banned, True) + ")"
    return re.compile(rx)


numchars = "0123456789"
alphachars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

# lead_idchars = alphachars
# idchars = lead_idchars + "_" + numchars
# opchars = "+-*/="

rx_num = "[0-9][0-9_]*"
rx_maynum = "(?:%s)?" % rx_num
rx_dec = "\\.(%s)" % rx_num
rx_maydec = "\\.(%s)" % rx_maynum
rx_exp = "(?:e|E)((?:\\+|-)?%s)" % rx_num

rx_radix = "(%s)(?:r|R)" % rx_num
rx_alphanum = "[a-zA-Z0-9_]+"
rx_mayalphanum = "(?:%s)?" % rx_alphanum
rx_rdec = "\\.(%s)" % rx_alphanum
rx_rmaydec = "\\.(%s)" % rx_mayalphanum


standard_matchers = [
    # Identifiers
    subtok_rule(chr_id, rx_choice(chr_id_lead) + rx_choice(chr_id) + "*", ["id", "id", 0]),

    # Numbers (other bases)
    subtok_rule(numchars, "%s(%s)%s" % (rx_radix, rx_mayalphanum, rx_rmaydec), ["id", "numR", 1, 2, 3]),
    subtok_rule(numchars, "%s(%s)" % (rx_radix, rx_mayalphanum), ["id", "numR", 1, 2, None]),

    # Numbers (base 10)
    subtok_rule(numchars, "(%s)%s%s" % (rx_num, rx_maydec, rx_exp), ["id", "num10", 1, 2, 3]),
    subtok_rule(numchars, "(%s)%s" % (rx_num, rx_maydec), ["id", "num10", 1, 2, None]),
    subtok_rule(numchars, "(%s)%s" % (rx_num, rx_exp), ["id", "num10", 1, None, 2]),
    subtok_rule(".", "%s%s" % (rx_dec, rx_exp), ["id", "num10", None, 1, 2]),
    subtok_rule(numchars, "(%s)" % (rx_num), ["id", "num10", 1, None, None]),
    subtok_rule(".", "%s" % (rx_dec), ["id", "num10", None, 1, None]),

    # Operators
    subtok_rule('"', "\"", ["prefix", 0], ws = (True, False), action = ["push", "string"]),
    subtok_rule("([{", "\\(|\\[|\\{", ["prefix", 0], action = ["push", "inbrack"]),
    subtok_rule(")]}", "\\)|\\]|\\}", ["suffix", 0], action = ["pop"]),
    subtok_rule(",", ",", ["infix", 0]),
    "nl_insert_point",
    subtok_rule(":", rx_without(":", chr_op), ["infix", 0]),
    subtok_rule("=", rx_without("=", chr_op), ["infix", 0]),
    subtok_rule("-", rx_without("->", chr_op), ["infix", 0]),
    subtok_rule(".", rx_without("\\.", chr_op), ["prefix", 0]),
    subtok_rule("#", rx_without("#", chr_op), ["prefix", 0]),
    subtok_rule("!", "!!", ["infix", 0]),
    subtok_rule("!", "!", ["prefix", 0]),
    subtok_rule(chr_op, "%s+" % rx_choice(chr_op), ["?fix", 0]),

    # False friends
    subtok_rule(chr_reject.keys(), rx_choice(chr_reject.keys()), ["id", "falsefriend", 0]),

    # Whatever else we encounter
    subtok_rule(True, ".", ["id", "unknown", 0])

    # Don't put anything here. It won't be reached.
]


def lreplace(l, old, new):
    return [new if x == old else x
            for x in l]

# The two "main" subtokenizers only differ in their handling of indent ("\n *")
# subtok_normal is top level, but entering a bracket switches to subtok_inbrack.
# subtok_normal yields INDENT tokens (postprocessed to create indented blocks)
# whereas subtok_inbrack yields NL tokens (which essentially reduce to commas)

subtok_normal = SubTokenizer(
    lreplace(standard_matchers,
             "nl_insert_point",
             subtok_rule("\n", "\n *", ["infix", "INDENT", lambda match: len(match[1:])])))

subtok_inbrack = SubTokenizer(
    lreplace(standard_matchers,
             "nl_insert_point",
             subtok_rule("\n", "\n *", ["infix", "NL", lambda match: len(match[1:])])))

# subtok_string parses the insides of strings. We switch to it upon
# seeing the " token, and pop out when we see another " token.
# Anything in-between becomes plain "str" tokens, to be processed
# later.

subtok_string = SubTokenizer([
        subtok_rule(True, "[^\"]+", ["id", "str", 0], ws = (False, False)),
        subtok_rule('"', "(\")\"", ["id", "str", 1], ws = (False, False)),
        subtok_rule('"', "\"", ["suffix", 0], ws = (False, True), action = ["pop"])])


def tokenizer_plus_indent(tokenizer):
    indent_stack = []
    current_indent = 0
    for tok in tokenizer:
        if tok.args[0] == 'INDENT':
            indent = tok.args[1]
            if indent > current_indent:
                indent_stack.append(current_indent)
                yield Token("infix", [''], (0, 0), loc = tok.loc)
                yield Token("id", [VOID], (0, 0), loc = tok.loc)
                # print("(")
                yield Token("prefix", ['('], (0, 0), loc = tok.loc)
                current_indent = indent
            elif indent < current_indent:
                # yield Token("suffix", [')'], (0, 0), loc = tok.loc)
                # print(")")
                first = True
                while indent_stack:
                    previous_indent = indent_stack.pop()
                    if not first:
                        yield Token("id", [VOID], (0, 0), loc = tok.loc)
                    else:
                        first = False
                    yield Token("suffix", [')'], (0, 0), loc = tok.loc)
                    if indent == previous_indent:
                        break
                yield Token("id", [VOID], (0, 0), loc = tok.loc)
                yield Token("infix", ['NL', 0], tok.ws, loc = tok.loc)
                if indent != previous_indent:
                    raise SyntaxError["indent_mismatch"](
                        token = tok,
                        indent = indent,
                        indent_above = current_indent,
                        indent_below = previous_indent)
                current_indent = previous_indent
            else:
                yield Token("infix", ['NL', 0], tok.ws, loc = tok.loc)
        else:
            yield tok

# def _t(tokenizer):
#     for x in tokenizer_plus_indent(tokenizer):
#         print(x)
#         yield x
#     print("AAAAAAAAAAAAAAAAAAAAA")


def tokenizer(source):
    t = Tokenizer(source, dict(normal = subtok_normal,
                               inbrack = subtok_inbrack,
                               string = subtok_string))
    t = tokenizer_plus_indent(t)
    return t


