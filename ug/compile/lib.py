
import operator
from functools import reduce

from ..parsing import VOID
from ..lib import (hashstruct, anonstruct, attrdict,
                   hybrid, index, hastag, index)

hs = hashstruct

ug_library = {}
rev_ug_library = {}


def library_function(name):
    if isinstance(name, str):
        def deco(f):
            ug_library[name] = f
            rev_ug_library[f] = name
            try:
                f.__libname__ = name
            except (AttributeError, TypeError):
                pass
            return f
        return deco
    else:
        return library_function(name.__name__)(name)

def opaque_function(f):
    return library_function("%%" + f.__name__)(f)


operators = """
abs abs
+ add
& and_
== eq
// floordiv
>= ge
> gt
invert invert
is is_
=< le
lshift lshift
< lt
mod mod
* mul
!= ne
| or_
** pow
rshift rshift
- sub
/ truediv
xor xor
"""

operators = [entry.split(" ") for entry in operators.split("\n") if entry]

for name, name_in_module in operators:
    library_function(name)(getattr(operator, name_in_module))

# @library_function
# def tuple_index(tup, idx):
#     return tup[idx]





def _convert_formula(f):

    if isinstance(f, tuple):
        new = []
        i = 0
        ranges = [0, 0, None]
        keys = []

        dstar = False
        # saw_assoc = False
        # over = False
        # defaults = []

        acceptable = (str, tuple, hs.deconstruct, hs.check,
                      hs.star, hs.assoc, hs.dstar, hs.default,
                      hs.default_assoc)

        def ban(*nogood):
            nonlocal acceptable 
            acceptable = tuple(set(acceptable) - set(nogood))

        def process_entry(entry):
            nonlocal acceptable, dstar, i

            if (isinstance(entry, hs.default)
                and isinstance(entry[0], hs.assoc)):
                entry = hs.default_assoc(entry[0][0], entry[0][1], entry[1])

            if not isinstance(entry, acceptable):
                raise Exception("Violates order:", entry)

            if isinstance(entry, hs.star):
                ban(hs.star, hs.default)
                acceptable += (str, tuple, hs.deconstruct, hs.check)
                i = 2
                ranges[-1] = 0
                new.append((entry[0], ))
            elif isinstance(entry, hs.assoc):
                ban(hs.star, str, tuple, hs.deconstruct, hs.check)
                keys.append(entry[0])
                new.append((entry[1], ))
            elif isinstance(entry, hs.default_assoc):
                ban(hs.star, str, tuple, hs.deconstruct, hs.check)
                keys.append(entry[0])
                new.append((entry[1], entry[2]))
            elif isinstance(entry, hs.dstar):
                ban(hs.star, str, tuple, hs.deconstruct, hs.check,
                    hs.assoc, hs.default_assoc, hs.dstar)
                dstar = True
                new.append((entry[0], ))
            elif isinstance(entry, hs.default):
                subf, d = entry[:]
                ban(str, tuple, hs.deconstruct, hs.check)
                ranges[1] += 1
                new.append((subf, d))
            # elif saw_assoc:
            #     raise Exception("=> must be at the end")
            else:
                ranges[i] += 1
                new.append(entry)
            

            # nonlocal over, dstar, saw_assoc
            # if over:
            #     raise Exception("** must be last")
            # if isinstance(entry, hs.star):
            #     if saw_assoc:
            #         raise Exception("=> must be at the end")
            #     ranges.append(0)
            #     new.append(entry[0])
            # elif isinstance(entry, hs.assoc):
            #     saw_assoc = True
            #     keys.append(entry[0])
            #     new.append(entry[1])
            # elif isinstance(entry, hs.dstar):
            #     dstar = True
            #     over = True
            #     new.append(entry[0])
            # elif isinstance(entry, hs.default):
            #     raise Exception("eee")
            #     # subf, d = entry[:]
            #     # defaults.append(d)
            # elif saw_assoc:
            #     raise Exception("=> must be at the end")
            # else:
            #     ranges[-1] += 1
            #     new.append(entry)

        for entry in f:
            process_entry(entry)

        # if len(ranges) > 2:
        #     raise Exception("More than one *")

        return (ranges[0],
                ranges[1],
                ranges[2],
                tuple(keys),
                bool(dstar)) + tuple((_convert_formula(x),) + tuple(d)
                                     for x, *d in new)

    elif isinstance(f, hs.deconstruct):
        return hs.deconstruct(f[0], *_convert_formula(f[1:]))

    elif isinstance(f, hs.check):
        return hs.check(f[0], _convert_formula(f[1]))

    elif isinstance(f, str):
        return f


def _deconstruct(dctor, value):
    if dctor is None:
        if isinstance(value, (tuple, list, dict, hybrid)):
            return value
        else:
            raise Exception("Not deconstructible")
    else:
        return dctor.__deconstruct__(value)

def _deconstruct2(value, f):

    if isinstance(f, str):
        return (value,)

    elif isinstance(f, tuple):
        if isinstance(value, (tuple, list)):
            t, d = value, {}
        elif isinstance(value, dict):
            t, d = (), dict(value)
        elif isinstance(value, hybrid):
            t, d = value.tuple, dict(value.dict)
        else:
            raise TypeError("Not deconstructible")

        nfirst, ndefaults, nlast, keys, dstar, *subf = f
        results = [_deconstruct2(t[i], subf[i][0])
                   for i in range(nfirst)]

        lt = len(t)
        for i in range(nfirst, nfirst + ndefaults):
            p, default = subf[i]
            results.append(_deconstruct2(t[i] if i < lt else default, p))

        if nlast is not None:
            remainder = lt - nfirst - ndefaults
            if nlast and remainder < nlast:
                raise TypeError("not enough")
            results.append(_deconstruct2(t[nfirst+ndefaults:len(t)-nlast],
                                        subf[nfirst+ndefaults][0]))
            results += [_deconstruct2(t[lt-nlast+i],
                                      [x[0] for x in subf[nfirst+ndefaults+i+1]])
                        for i in range(nlast)]
        elif lt > nfirst + ndefaults:
            raise TypeError("Expected exactly", nfirst)

        for key, keyf in zip(keys, subf[nfirst + (nlast+1 if nlast is not None else 0):]):
            try:
                r = d.pop(key)
            except KeyError:
                if len(keyf) == 2:
                    r = keyf[1]
                else:
                    raise
            results.append(_deconstruct2(r, keyf[0]))
        if dstar:
            results.append(_deconstruct2(d, subf[-1]))
        elif d:
            raise TypeError("Extra keys", d)

        return reduce(tuple.__add__, results, ())

    elif isinstance(f, hs.check):
        chk, var = f[:]
        result = check(chk, value)
        if var is None:
            return ()
        else:
            return (result,)

    elif isinstance(f, hs.deconstruct):
        dctor, *spec = f[:]
        return _deconstruct2(spec, _deconstruct(dctor, value))

    elif f is None:
        return ()


@library_function
def check(checker, value):
    if hasattr(checker, '__check__'):
        return checker.__check__(value)
    elif isinstance(checker, type):
        if isinstance(value, checker):
            return value
        else:
            raise TypeError("Expected %s but got %s" % (checker, type(value)))
    else:
        raise TypeError("Cannot check with", checker)


@library_function
class check_equal:
    def __init__(self, value):
        self.value = value
    def __check__(self, other):
        if self.value == other:
            return other
        raise TypeError("Expected == %s but got %s" % (self.value, other))


@library_function
class Deconstructor:

    def __init__(self, formula):
        self.formula = formula
        self._formula = _convert_formula(formula)

    def __call__(self, value):
        return _deconstruct2(value, self._formula)

    def __str__(self):
        return "Deconstructor[%s]" % str(self.formula)

    def __repr__(self):
        return str(self)


library_function(hybrid)
library_function(dict)
library_function(index)
library_function("#")(hs)

library_function("pymap")(map)
library_function(iter)
library_function(zip)

library_function("VOID")(VOID)


@library_function
def send(obj, msg):
    if isinstance(msg, tuple):
        return obj(*msg)
    elif isinstance(msg, dict):
        return obj(**msg)
    elif isinstance(msg, hybrid):
        return obj(*msg.tuple, **msg.dict)
    elif isinstance(msg, str):
        return getattr(obj, msg)
    elif isinstance(msg, index):
        return obj[msg.item]
    elif isinstance(msg, hs.assign):
        m, v = msg[:]
        if isinstance(m, str):
            return setattr(obj, m, v)
        elif isinstance(m, index):
            obj[m.item] = v
            return None
        else:
            return obj.__recv__(msg)
    else:
        return obj.__recv__(msg)

@library_function
def send_safeguard(obj, msg):
    try:
        return obj.__recv_safeguard__(msg)
    except AttributeError:
        if isinstance(msg, tuple):
            return hs.ok(obj(*msg))
        elif isinstance(msg, dict):
            return hs.ok(obj(**msg))
        elif isinstance(msg, hybrid):
            return hs.ok(obj(*msg.tuple, **msg.dict))
        elif isinstance(msg, str):
            return hs.ok(getattr(obj, msg))
        elif isinstance(msg, index):
            return hs.ok(obj[msg.item])
        raise


@library_function("map")
def ugmap(seq, obj):
    for entry in seq:
        result = send_safeguard(obj, entry)
        if isinstance(result, hs.ok):
            yield result[0]
        else:
            continue



@library_function
def patch_dict(*args):
    if len(args) == 0:
        return {}
    else:
        rval = {}
        for entry in args:
            rval.update(entry)
        return rval

@library_function
def patch_tuple(*args):
    if len(args) == 0:
        return ()
    elif len(args) == 1:
        return tuple(args[0])
    else:
        return reduce(lambda x, y: tuple(x) + tuple(y), args)

@library_function
def assign(obj, item, value):
    if isinstance(item, index):
        obj[item.item] = value
    elif isinstance(item, str):
        setattr(obj, item, value)
    else:
        obj.__recv__(hs.assign(item, value))


class ugobj:

    def __call__(self, *args, **kwargs):
        if kwargs:
            if args:
                return self.__recv__(hybrid(args, kwargs))
            else:
                return self.__recv__(kwargs)
        else:
            return self.__recv__(args)

    def __getattr__(self, attr):
        if attr.startswith('__'):
            return getattr(super(), attr)
        else:
            return self.__recv__(attr)

    def __getitem__(self, item):
        return self.__recv__(index(item))


@library_function
def make_object(*specifications):

    # for deconstructor, f in specifications:
    #     f.__name__ = name

    class R(ugobj):

        def __recv_safeguard__(self, arg):
            errors = []
            for deconstructor, f in specifications:
                try:
                    args = deconstructor(arg)
                except TypeError as e:
                    errors.append(e)
                    continue
                return hs.ok(f(*args))
            return hs.fail(errors)

        def __recv__(self, arg):
            errors = []
            for deconstructor, f in specifications:
                try:
                    args = deconstructor(arg)
                except TypeError as e:
                    errors.append(e)
                    continue
                return f(*args)
            raise TypeError("Nothing matched", errors)

    # R.__name__ = name
    # R.__recv__.__name__ = name
    return R()


@library_function
def do(gen):
    for x in gen:
        pass


library_function("to")(range)

@library_function("..")
def to(start, end):
    if start is VOID: start = None
    if end is VOID: end = None
    return slice(start, end)

@library_function("by")
def by(r, step):
    if isinstance(r, slice):
        return slice(r.start, r.stop, step)
    elif isinstance(r, range):
        # TODO: *surely* there is a better way
        s = repr(r).split("(")[1][:-1]
        values = list(map(int, s.split(", ")))
        return range(values[0], values[1], step)
    else:
        raise TypeError("Wrong type for 'by'.")

@library_function
def trycatch(thunk, handler, else_, finally_):
    try:
        rval = thunk()
    except Exception as e:
        if handler is not None:
            results = send_safeguard(handler, e)
            if isinstance(results, hs.ok):
                return results[0]
            raise
        raise
    else:
        if else_ is not None:
            return send(else_, rval)
        return rval
    finally:
        if finally_:
            finally_()
