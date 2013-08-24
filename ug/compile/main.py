
from .macros import macro_registry
from .compile import ExprCompiler, ASTRename, hs2
from ..lib import hashstruct as hs


def compile(ast, rename = True):
    c = ExprCompiler({}, macro_registry)
    if not isinstance(ast, hs.begin):
        ast = hs2.begin(ast)
    res = c.visit(ast)
    res = ASTRename(rename = rename).visit(res)
    return res

