
from types import FunctionType, MethodType
from descr import descr, HTMLRuleBuilder as RB
from ..lib import struct, hastag, gettag

def ugdescr(x, recurse = None):
    v = descr(x, ugdescr)
    if isinstance(x, struct):
        if hastag(x, "location") and gettag(x, "location"):
            return [{"$located"}] + list(v)
        else:
            return [{"!located"}] + list(v)
    elif hasattr(x, "__libname__"):
        return [{"$lib"}, x.__libname__]
    elif isinstance(x, (type, FunctionType)):
        return [{"$lib"}, x.__name__]
    else:
        return v

def minimal_rules():
    rules = RB()
    rules.css_border(".{!located}", "4px solid red")
    return rules

def pretty_rules():
    rules = RB()

    # Scalars and identifiers
    rules.pmclasses(".{@struct} .{@str}", "identifier", {"@str", "scalar"})
    rules.css_padding(".identifier, .{@int}, .{$lib}", "4px")
    rules.css_color(".{$lib}", "#8f8")

    # Values
    rules.mclasses(".{+#value}", "object")
    rules.mclasses(".{@struct} .{@int}", {"scalar"})
    rules.css_color(".{+#value} .identifier", "#f88")
    rules.css_padding(".{+#value}", "4px")
    rules.css_border(".{+#value} > .{@int}", "1px dotted #88f")
    rules.css_border(".{+#value} > .identifier", "1px dotted #f88")
    rules.css_border(".{+#value} > .{$lib}", "1px dotted #8f8")
    rules.css_border(".{+#value} > *", "1px dotted #888")

    # Operators and juxtaposition
    for cls, color in [(".{+#oper}", "#ff8"),
                       (".{+#juxt}", "#fff"),
                       (".{+#send}", "#fff")]:
        rules.mclasses(cls, "object")
        rules.css_border_bottom(".{+#oper}" + " > * > " + cls, "2px solid " + color)
        rules.css_border_bottom(".{+#juxt}" + " > * > " + cls, "2px solid " + color)
        rules.css_border_bottom(".{+#send}" + " > * > " + cls, "2px solid " + color)
        rules.css_margin(cls, "6px")

    def rearrange_oper(classes, children):
        op = children[0]
        results = [children[1]]
        for child in children[2:]:
            results += [[{"operator"}, op], child]
        return results

    rules.css_color(".operator", "#ff8")
    rules.rearrange(".{+#oper}", rearrange_oper)

    # Boxes
    for cls, color in [(".{+#square}", "#f80"),
                       (".{+#curly}", "#0a0"),
                       (".{+#begin}", "#08f"),

                       (".{+#seq}", "#f80")]:

        rules.builder_for(cls) \
            .mclasses("object") \
            .css_background_color(color) \
            .css_margin("4px") \
            .css_padding("4px") \
            .css_border_radius("5px")

        rules.builder_for(cls + " > *") \
            .css_background_color("#000") \
            .css_margin_left("2px") \
            .css_margin_right("2px") \
            .css_border_radius("5px")

    rules.css_margin_top(".{+#begin} > *", "6px")
    rules.css_margin_bottom(".{+#begin} > *", "6px")

    # Begin
    rules.css_display(".{+#begin} > *", "block")

    # Assign
    rules.mclasses(".{+#assign}", "object")
    rules.rearrange(".{+#assign}",
                    lambda classes, children:
                        [children[0],
                         [{"operator"}, ":="],
                         children[1]])

    # Lambda, declaring
    for cls, kw in [(".{+#lambda}", "λ"),
                    (".{+#declaring}", "declaring")]:

        rules.mclasses(cls, "object")
        rules.rearrange(cls,
                        lambda classes, children, kw=kw:
                            [[{"assoc"},
                              [{"arguments"}, [{"shortheader"}, kw]]
                              + list(children[0]),
                              children[1]]])

    rules.mclasses(".arguments", "@list")
    rules.css_color(".shortheader", "#88f")
    rules.css_padding(".shortheader", "3px")

    # # Void
    # rules.hide(".{@Void}")

    # UniqueVar
    rules.builder_for(".{@UniqueVar}") \
        .css_padding("3px").css_margin("3px")
    rules.builder_for(".{@UniqueVar} .identifier") \
        .css_padding("0px").css_margin("0px")
    rules.builder_for(".{@UniqueVar} .{@int}") \
        .css_color("#fff").css_padding("0px").css_margin("0px")
    rules.builder_for(".{@UniqueVar}>:last-child") \
        .css_vertical_align("sub").css_font_size("3pt")

    return rules

def hl_rules():
    rules = RB()

    for k, v in [('.value_int', '#88f'),
                 ('.value_float', '#88f'),
                 ('.value_str', '#f88'),
                 ('.opname', '#8f8'),
                 ('.comment', '#888'),
                 ('.kw_True', '#0ff'),
                 ('.kw_False', '#0ff'),
                 ('.kw_None', '#0ff'),
                 ('.{op_#} > .opname, .{op_#} > .ugstr', '#ff8'),
                 ('.{op_.} > .opname, .{op_.} > .ugstr', '#eee'),
                 ]:
        rules.css_color(k, v)

    rules.rule("span", {"display": "inline", "vertical-align": "none"})
    rules.rule(".src", {"white-space": "pre"})

    # rules.css_border_bottom(".{op_->} > :first-child", "1px solid #888")

    rules.pclasses(".{leftof_->}", "decl")
    rules.pclasses(".{leftof_=}", "decl")
    rules.pclasses(".{leftof_when}", "decl")

    def notlast(classes, parts):
        rval = {"juxtnn"} if "juxtn" not in classes else set()
        return rval
    rules.pclasses(".juxt > *", notlast)

    # variable declaration
    paths = [".decl.ugstr",
             ".decl.square > .ugstr",
             ".decl > .juxtn.ugstr",
             ".decl > .juxtn.square > .ugstr",
             ".decl > .{rightof_=>}.ugstr",
             ".decl > .{rightof_**}.ugstr",
             ".decl > .{rightof_*}.ugstr",
             ".decl > .{leftof_=}.ugstr",
             ".decl :not(.rightof_when) .juxtn.ugstr",
             ".decl :not(.rightof_when) .juxtn.square > .ugstr",
             ".decl :not(.rightof_when) .{rightof_=>}.ugstr",
             ".decl :not(.rightof_when) .{rightof_**}.ugstr",
             ".decl :not(.rightof_when) .{rightof_*}.ugstr",
             ".decl :not(.rightof_when) .{leftof_=}.ugstr"
             ]
    for path in paths:
        rules.rule(path, {'color': "#f80"})

    # variable type declaration
    paths = [".decl > .juxtnn .juxtn.square > .ugstr",
             ".decl > .juxtnn",
             ".decl > .{op_#} > .opname",
             ".decl > .{op_#} > .ugstr",
             ".decl :not(.rightof_when) .juxtnn .juxtn.square > .ugstr",
             ".decl :not(.rightof_when) .juxtnn",
             ".decl :not(.rightof_when) .{op_#} > .opname",
             ".decl :not(.rightof_when) .{op_#} > .ugstr"
             ]
    for path in paths:
        rules.rule(path, {'color': "#0a8"})

    # leftmost of :
    paths = [".{leftof_:} > .juxt0",
             ".{leftof_:}.ugstr",
             ".opname_and",
             ".opname_or",
             ".opname_not",
             ".opname_each",
             ".opname_map",
             ".opname_when",
             ".kw_break.ugstr",
             ".kw_continue.ugstr",
             ".kw_finally.ugstr",
             ".kw_success.ugstr"]
    for path in paths:
        rules.rule(path, {'color': "#f8f",
                          'font-weight': 'bold'})

    # leftmost of :
    paths = []
    for path in paths:
        rules.rule(path, {'color': "#f8f",
                          'font-weight': 'bold'})
    


    # rules.css_color(".decl .juxtnn.ugstr", "green")

    # rules.css_color(".decl .square .ugstr", "red")
    # rules.css_color(".decl .juxtnn .square .ugstr", "green")



    # rules.pclasses(".decl", "spring")
    # rules.pclasses(".spring > .juxtn", "spring")
    # rules.pclasses(".spring > .square > *", "spring")


    # rules.pclasses(".decl > .square", "sqcoco")
    # rules.pclasses(".sqcoco > ", "coco")
    # rules.pclasses(".coco > .juxtn", "coco")

    # rules.css_color(".spring.ugstr", "red")


    # rules.css_color(".decl .juxtn.square > .ugstr", "red")
    # rules.css_color(".decl.square > .ugstr", "red")
    # rules.css_color(".decl .ugstr", "green")

    # rules.css_color(".{leftof_->} .square > .ugstr", "green")

    # path = ".decl > :first-child.juxt"
    # path = path + ", " + path.replace(".juxt", " .juxt")
    # rules.css_color(path, "#fa8")

    # # path = ".decl > :first-child.juxt .ugstr"
    # # path = path + ", " + path.replace(".juxt", " .juxt")
    # # rules.css_color(path, "#fa8")

    # path = ".decl > :first-child.juxt > :last-child"
    # path = path + ", " + path.replace(".juxt", " .juxt")
    # rules.css_color(path, "#0a8")

    # path = ".decl > :first-child .square > .ugstr, .decl > :first-child.ugstr"
    # rules.css_color(path, "#0a8")

    return rules


ast_rules = minimal_rules() + pretty_rules()
hl_rules = hl_rules()

