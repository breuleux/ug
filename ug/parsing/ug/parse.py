
from .ast import make_ast, ASTCollapse
from .codec import decode as de
from .tokenizer import tokenizer
from ..generic.parse import \
    OperatorGroup, OperatorGroups, OperatorMatrix, \
    Bracket, BracketMerge, OperatorParse
from ...lib import hashstruct, tag, gettag

def is_assignment(op):
    return op.fixity == 'infix' and op.name.endswith("=")

def build_op_groups():
    return OperatorGroups(
        OperatorGroup("sjuxt", "XWY"),
        OperatorGroup("wjuxt", "X W Y"),

        OperatorGroup("pfx", "._Y", "#_Y", "?_Y"),
        OperatorGroup("deco", "@_Y"),

        OperatorGroup("add", "X_+_Y", "X_-_Y"),
        OperatorGroup("mul", "X_*_Y", "X_/_Y", "X_//_Y", "X_%_Y", "X_mod_Y"),
        OperatorGroup("unary", "+_Y", "-_Y", "~_Y"),
        OperatorGroup("pow", "X_**_Y"),

        OperatorGroup("range", "X_.._Y", "X_..", ".._Y", "X_to_Y"),
        OperatorGroup("by", "X_by_Y"),
        OperatorGroup("in", "X_in_Y"),
        OperatorGroup("map", "X_map_Y", "X_each_Y"),

        OperatorGroup("binor", "X_|_Y"),
        OperatorGroup("binxor", "X_^_Y"),
        OperatorGroup("binand", "X_&_Y"),

        OperatorGroup("not", "not_Y"),
        OperatorGroup("and", "X_and_Y"),
        OperatorGroup("or", "X_or_Y"),

        OperatorGroup("cmp",
                      "X_<_Y", "X_=<_Y", "X_==_Y",
                      "X_>_Y", "X_>=_Y", "X_!=_Y"),

        OperatorGroup("pr", de("<>_Y")),
        OperatorGroup("cond", "X_when_Y"),
        OperatorGroup("decl", is_assignment, "X_=_Y"),
        OperatorGroup("assoc", "X_=>_Y", "=>_Y"),
        OperatorGroup("lbda", "X_->_Y"),
        OperatorGroup("colon", "X_:_Y"),
        OperatorGroup("bang", "!_Y", "X_!!_Y"),
        # OperatorGroup("err", "X_!!_Y"),
        OperatorGroup("seq", "X_,_Y"),
        OperatorGroup("nl", "X_NL_Y"),

        OperatorGroup("open", "(_Y", "[_Y", "{_Y", "\"_Y"),
        OperatorGroup("close", "X_)", "X_]", "X_}", "X_\""),

        OperatorGroup("custom", lambda x: True)
        )

def build_op_matrix():

    op_matrix = OperatorMatrix(op_groups.gnames)

    op_matrix.left_assoc("sjuxt", "wjuxt", "add", "mul", "binor", "binxor", "binand", "cmp", "and", "or", "seq", "nl")
    op_matrix.right_assoc("pow", "lbda", "decl", "colon")

    # major
    op_matrix.order("pfx", "sjuxt", "wjuxt", "assoc", "decl", "cond", "bang", "nl", "seq")
    op_matrix.order("sjuxt", "custom", "assoc")
    op_matrix.order("seq", "open")
    op_matrix.order("seq", "close")
    op_matrix.order("sjuxt", "pow", "unary", "mul", "add",
                    "range", "in",
                    "binand", "binxor", "binor", "cmp",
                    "assoc")
    op_matrix.order("range", "by", "in")
    op_matrix.order("in", "wjuxt")
    op_matrix.order("by", "map", "assoc")
    op_matrix.order("sjuxt", "deco", "wjuxt")

    # logical connectives
    op_matrix.order("cmp", "and", "assoc")
    op_matrix.order("cmp", "or", "assoc")

    # lambda & colon
    op_matrix.order("wjuxt", "lbda", "bang")
    # op_matrix.order("lbda", "wjuxt")
    op_matrix.order("wjuxt", "colon", "bang")

    # infer
    op_matrix.infer()

    # corrections
    op_matrix.left_order("cond", "assoc")
    op_matrix.left_order("assoc", "cond")

    op_matrix.left_order("lbda", "assoc")
    op_matrix.right_order("lbda", "assoc")
    op_matrix.left_order("lbda", "decl")
    op_matrix.right_order("lbda", "decl")
    op_matrix.left_order("cond", "lbda")
    op_matrix.right_order("cond", "lbda")

    for g in ["sjuxt", "wjuxt", "pfx", "add", "mul", "unary", "pow", "range",
              "binor", "binxor", "binand", "cmp", "and", "or",
              "pr", "cond", "assoc", "lbda", "colon", "custom"]:
        op_matrix.right_order(g, "colon")
        op_matrix.right_order(g, "lbda")

    op_matrix.order("lbda", "map")

    op_matrix.set_relation("open", "close", BracketMerge({
                "()": Bracket('round'),
                "[]": Bracket('square'),
                "{}": Bracket('curly'),
                '""': Bracket('quote')
                }))

    return op_matrix


op_groups = build_op_groups()
op_matrix = build_op_matrix()

def parse(source):
    parser = OperatorParse(source, tokenizer(source), op_groups, op_matrix)
    ast0 = parser.parse()
    ast1 = make_ast(ast0)
    return ASTCollapse().visit(tag(hashstruct.round(ast1),
                                   'location',
                                   gettag(ast1, 'location')))



