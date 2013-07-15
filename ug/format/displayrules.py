
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


rules = minimal_rules() + pretty_rules()
