
import re
from ..parsing.ug.ast import ASTVisitor, getloc
from descr.util import Raw, Description

class ASTHighlight(ASTVisitor):

    def __init__(self):
        self.transfer_locations = False
        self.cursor = 0

    # def visit(self, node):
    #     loc = getloc(node)
    #     rval = []
    #     results = rval
    #     if loc.start > self.cursor:
    #         text = loc.source.text[self.cursor:loc.start]
    #         text = text.split(';;', 1)
    #         rval.append(text[0])
    #         if len(text) == 2:
    #             rval.append([{'comment'}, ';;', text[1]])
    #         # results = []
    #         # rval.append(results)
    #         self.cursor = loc.start

    #     results += super().visit(node)
    #     results.insert(0, {'node', type(node).__name__})
    #     if loc.end > self.cursor:
    #         text = loc.source.text[self.cursor:loc.end]
    #         text = text.split(';;', 1)
    #         results.append(text[0])
    #         if len(text) == 2:
    #             rval.append([{'comment'}, ';;', text[1]])
    #         self.cursor = loc.end
    #     return rval

    def visit(self, node, outer):
        loc = getloc(node)
        results = []
        if loc.start > self.cursor:
            text = loc.source.text[self.cursor:loc.start]
            text = text.split(';;', 1)
            outer.append(text[0])
            if len(text) == 2:
                outer.append([{'comment'}, ';;', text[1]])
            # results = []
            # rval.append(results)
            self.cursor = loc.start

        results += super().visit(node)
        results.insert(0, {'node', type(node).__name__})
        if loc.end > self.cursor:
            text = loc.source.text[self.cursor:loc.end]
            text = text.split(';;', 1)
            results.append(text[0])
            if len(text) == 2:
                results.append([{'comment'}, ';;', text[1]])
            self.cursor = loc.end
        return results

    def visit_ugstr(self, node):
        # print(node, len(node), getloc(node).span, self.cursor)
        # self.cursor += len(node)
        loc = getloc(node)
        text = loc.source.text[loc.start:loc.end]
        self.cursor = loc.end
        return [{'kw_' + node}, text]

    def visit_value(self, node, v):
        return [{'value_' + type(v).__name__}]

    def visit_oper(self, node, op, a, b):
        rval = [{'op_' + op}]
        rval.append([{'leftof_' + op}] + self.visit(a, rval))
        rval.append([{'opname', 'opname_' + op}] + self.visit(op, rval))
        rval.append([{'rightof_' + op}] + self.visit(b, rval))
        return rval

    def seq(self, name, node, args):
        rval = []
        for i, arg in enumerate(args):
            rval.append([{name + str(i)}] + self.visit(arg, rval))
        if rval:
            rval[-1].insert(0, {name + 'n'})
        return rval

    def visit_juxt(self, node, *args):
        return self.seq('juxt', node, args)

    def visit_begin(self, node, *args):
        return self.seq('begin', node, args)

    def visit_square(self, node, *args):
        return self.seq('square', node, args)

    def visit_curly(self, node, *args):
        return self.seq('curly', node, args)

    def visit_generic(self, node):
        raise Exception("Unknown node", node)



# class ASTHighlight(ASTVisitor):

#     def classify(self, node, classes = set()):
#         c = tuple(getloc(node).span) + (classes | {type(node).__name__},)
#         return c

#     def visit_ugstr(self, node):
#         return [self.classify(node, {'kw_' + node})]

#     def visit_value(self, node, v):
#         return [self.classify(node)]

#     def visit_juxt(self, node, *args):
#         rval = []
#         for arg in args:
#             rval += self.visit(arg)
#         return rval

#     def visit_oper(self, node, op, a, b):
#         return ([self.classify(op, {'opname', 'opname_' + op}),
#                  self.classify(node, {'op_' + op}),]
#                 + self.visit(a) + self.visit(b))

#     def visit_begin(self, node, *args):
#         rval = []
#         for arg in args:
#             rval += self.visit(arg)
#         return rval

#     def visit_square(self, node, *args):
#         rval = []
#         for arg in args:
#             rval += self.visit(arg)
#         return rval

#     def visit_curly(self, node, *args):
#         rval = []
#         for arg in args:
#             rval += self.visit(arg)
#         return rval

#     def visit_generic(self, node):
#         raise Exception("Unknown node", node)


def highlight(ast):
    # return Description(ASTHighlight().visit(ast))
    res = [{'src'}]
    res += ASTHighlight().visit(ast, res)
    return Raw(res)

