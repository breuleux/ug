
import itertools
from collections import OrderedDict
import operator
from functools import reduce
from types import FunctionType

from ..parsing import Void, SyntaxError
from ..parsing.ug.ast import transfer
from ..lib import (hashstruct, anonstruct, attrdict,
                   struct,
                   hybrid, index, hastag, tag, ugstr)
from ..tools import exc

class UGTypeError(exc.RichException, TypeError):
    pass

class DeconstructError(UGTypeError):
    pass

class GuardError(UGTypeError):
    pass

class MatchError(UGTypeError):
    pass

SHF = False


NoneType = type(None)

hs = hashstruct

ug_library = {}
rev_ug_library = {}


def pair(x, y):
    return [x, y]

def symbol(x):
    return x

def struct_project(f, var):
    return hs.check(f, var)

def struct_deconstruct(f, args):
    return hs.deconstruct(f, *args)

def struct_star(x):
    return hs.star(x)

def struct_dstar(x):
    return hs.dstar(x)

def struct_assoc(k, v):
    return hs.assoc(k, v)

def struct_default(k, v):
    return hs.default(k, v)

def delay(fn):
    raise Exception("no laziness here")


class Partial:

    def __init__(self, struct):
        if isinstance(struct, (list, tuple)):
            self.struct = hybrid(struct, {})
        elif isinstance(struct, dict):
            self.struct = hybrid([], struct)
        elif isinstance(struct, hybrid):
            self.struct = struct
        else:
            raise Exception

    def merge(self, arg):
        if isinstance(arg, (list, tuple)):
            l, d = arg, {}
        elif isinstance(arg, dict):
            l, d = [], arg
        elif isinstance(arg, hybrid):
            l, d = arg.tuple, arg.dict
        else:
            raise Exception

        l0, d0 = self.struct.tuple, self.struct.dict

        usedl = [False for _ in l]
        usedd = {k: False for k, _ in d.items()}

        newl = []
        newd = {}

        for x in l0:
            if isinstance(x, Hole):
                if isinstance(x.id, int):
                    v = l[x.id]
                    usedl[x.id] = True
                else:
                    v = d[x.id]
                    usedd[x.id] = True

                if x.star == "*":
                    newl += v
                elif x.star == "**":
                    newd.update(v)
                else:
                    newl.append(v)
            else:
                newl.append(x)

        for k, x in d0.items():
            if isinstance(x, Hole):
                if isinstance(x.id, int):
                    v = l[x.id]
                    usedl[x.id] = True
                else:
                    v = d[x.id]
                    usedd[x.id] = True
                    
                newd[k] = v
            else:
                newd[k] = x

        return hybrid(newl, newd)

    def __str__(self):
        return 'Partial %s' % self.struct
    def __repr__(self):
        return 'Partial %s' % self.struct

class Hole:
    def __init__(self, id, star = ""):
        self.id = id
        self.star = star
    def __str__(self):
        return '%s_%s' % (self.star, self.id)
    def __repr__(self):
        return '%s_%s' % (self.star, self.id)


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


absent = object()

@library_function
class BreakException(Exception):
    def __init__(self, value = absent):
        if value is not absent:
            self.value = value

@library_function
class ContinueException(Exception):
    def __init__(self, value = absent):
        if value is not absent:
            self.value = value



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


@library_function("in")
def ugin(a, b):
    _SHOW_FRAME = SHF
    return a in b


def _convert_formula(f):
    _SHOW_FRAME = SHF

    if isinstance(f, (tuple, list)):
        new = []
        i = 0
        ranges = [0, 0, None]
        keys = []

        dstar = False

        acceptable = (NoneType, str, tuple, list, hs.deconstruct, hs.check,
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
                raise SyntaxError['lhs/order'](
                    message = ("Clauses in []s in a left hand side must "
                               "be laid in this order: positional variables, "
                               "positional variables with defaults, "
                               "*variable (at most one), positional variables, "
                               "keyword variables with or without defaults, "
                               "**variable (at most one). The highlighted "
                               "element is the first to violate the order."),
                    node = entry)

            if isinstance(entry, hs.star):
                ban(hs.star, hs.default)
                acceptable += (str, tuple, list, hs.deconstruct, hs.check)
                i = 2
                ranges[i] = 0
                new.append((entry[0], ))
            elif isinstance(entry, hs.assoc):
                ban(hs.star, str, tuple, list, hs.deconstruct, hs.check)
                keys.append(entry[0])
                new.append((entry[1], ))
            elif isinstance(entry, hs.default_assoc):
                ban(hs.star, str, tuple, list, hs.deconstruct, hs.check)
                keys.append(entry[0])
                new.append((entry[1], entry[2]))
            elif isinstance(entry, hs.dstar):
                ban(hs.star, str, tuple, list, hs.deconstruct, hs.check,
                    hs.assoc, hs.default_assoc, hs.dstar)
                dstar = True
                new.append((entry[0], ))
            elif isinstance(entry, hs.default):
                subf, d = entry[:]
                ban(str, tuple, list, hs.deconstruct, hs.check)
                ranges[1] += 1
                new.append((subf, d))
            else:
                ranges[i] += 1
                new.append((entry,))

        for entry in f:
            process_entry(entry)

        return transfer(ugtuple(
            (ranges[0],
             ranges[1],
             ranges[2],
             tuple(keys),
             bool(dstar)) + tuple((_convert_formula(x),) + tuple(d)
                                  for x, *d in new)), f)

    elif isinstance(f, hs.deconstruct):
        rval = hs.deconstruct(f[0], *_convert_formula(f[1:]))
        transfer(rval, f)
        return rval

    elif isinstance(f, hs.check):
        rval = hs.check(f[0], _convert_formula(f[1]))
        transfer(rval, f)
        return rval

    elif isinstance(f, str):
        return f

    elif f is None:
        return f

    elif isinstance(f, (hs.star, hs.dstar, hs.default)):
        raise SyntaxError['lhs/bad_deconstructor'](
            message = "This expression is only valid inside []s.",
            node = f)

    else:
        raise SyntaxError['lhs/bad_deconstructor'](
            message = "Invalid deconstructor: " + str(f),
            node = f)


def _deconstruct(f, dctor, value):
    _SHOW_FRAME = SHF

    if dctor is None:
        if isinstance(value, (tuple, list, dict, hybrid)):
            return value
        else:
            raise DeconstructError['not_deconstructible'](
                value = value,
                pattern = f)
    elif hasattr(dctor, '__deconstruct__'):
        rval = dctor.__deconstruct__(value)
        return rval
    else:
        raise DeconstructError['not_deconstructor'](
            deconstructor = dctor,
            pattern = f)

def _deconstruct2(value, f):
    _SHOW_FRAME = SHF

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
            raise DeconstructError['not_deconstructible'](
                value = value,
                pattern = f)

        nfirst, ndefaults, nlast, keys, dstar, *subf = f

        low = nfirst + (nlast or 0)
        if nlast is None:
            high = low + ndefaults
        else:
            high = None

        lt = len(t)

        if lt < low or high is not None and lt > high:
            raise DeconstructError['wrong_length'](
                expected = (low, high),
                received = lt,
                pattern = f)

        results = []

        for i in range(nfirst):
            results.append(_deconstruct2(t[i], subf[i][0]))

        for i in range(nfirst, nfirst + ndefaults):
            p, default = subf[i]
            results.append(_deconstruct2(t[i] if i < lt else default, p))

        if nlast is not None:
            results.append(_deconstruct2(t[nfirst+ndefaults:len(t)-nlast],
                                        subf[nfirst+ndefaults][0]))
            results += [_deconstruct2(t[lt-nlast+i],
                                      subf[nfirst+ndefaults+i+1][0])
                        for i in range(nlast)]


        for key, keyf in zip(keys, subf[nfirst + (nlast+1 if nlast is not None else 0):]):
            try:
                r = d.pop(key)
            except KeyError:
                if len(keyf) == 2:
                    r = keyf[1]
                else:
                    raise DeconstructError['missing_key'](
                        key = key,
                        pattern = f)
            results.append(_deconstruct2(r, keyf[0]))
        if dstar:
            results.append(_deconstruct2(d, subf[-1][0]))
        elif d:
            raise DeconstructError['extra_keys'](
                keys = d,
                pattern = f)

        return reduce(tuple.__add__, results, ())

    elif isinstance(f, hs.check):
        chk, var = f[:]
        result = check(chk, value, pattern = f)
        if var is None:
            return ()
        else:
            return (result,)

    elif isinstance(f, hs.deconstruct):
        dctor, *spec = f[:]
        return _deconstruct2(_deconstruct(f, dctor, value), tuple(spec))

    elif f is None:
        return ()

    else:
        raise Exception("Unknown:", f)


@library_function("@")
class Checker:
    def __init__(self, void, f):
        _SHOW_FRAME = SHF
        self.f = f
    def __check__(self, value):
        _SHOW_FRAME = SHF
        return self.f(value)
    def __call__(self, value):
        _SHOW_FRAME = SHF
        return self.f(value)


class Raiser:
    def __recv__(self, message):
        _SHOW_FRAME = SHF
        raise message
raiser = Raiser()

library_function("raise")(raiser)


@library_function
def check(checker, value, pattern = None):
    _SHOW_FRAME = SHF

    try:
        checker = getattr(checker, '__check__')
    except AttributeError:
        pass
    else:
        return checker(value)

    if isinstance(checker, type):
        if isinstance(value, checker):
            return value
        else:
            raise UGTypeError['bad_type'](
                expected = checker,
                received = type(value),
                pattern = pattern)
    else:
        raise UGTypeError['bad_check'](
            checker = checker,
            value = value,
            pattern = pattern)


@library_function
class check_equal:
    def __init__(self, value):
        self.value = value
    def __check__(self, other):
        _SHOW_FRAME = SHF
        if self.value == other:
            return other
        raise UGTypeError['bad_value'](
            expected = self.value,
            received = other)


@library_function
class Deconstructor:

    def __init__(self, formula):
        self.formula = formula
        self._formula = _convert_formula(formula)

    def __call__(self, value):
        _SHOW_FRAME = SHF
        return _deconstruct2(value, self._formula)

    def __str__(self):
        return "Deconstructor[%s]" % str(self.formula)

    def __repr__(self):
        return str(self)


library_function(hybrid)
library_function(dict)
library_function(OrderedDict)
# library_function(index)
library_function("#")(hs)

library_function("pymap")(map)
library_function(iter)
library_function(zip)

library_function("Void")(Void)

library_function("%%tuple")(lambda *args: args)
library_function("%%list")(lambda *args: list(args))


@library_function
def send(obj, msg):
    _SHOW_FRAME = SHF
    try:
        f = type(obj).__recv__
    except AttributeError:
        if isinstance(msg, (tuple, list)):
            return obj(*msg)
        elif isinstance(msg, dict):
            return obj(**msg)
        elif isinstance(msg, hybrid):
            return obj(*msg.tuple, **msg.dict)
        elif isinstance(msg, str):
            return getattr(obj, msg)
        elif isinstance(msg, hs.index):
            if msg[0] is to:
                return obj[:]
            return obj[msg[0]]
        elif isinstance(msg, hs.assign):
            m, v = msg[:]
            if isinstance(m, str):
                return setattr(obj, m, v)
            elif isinstance(m, hs.index):
                if m[0] is to:
                    obj[:] = v
                else:
                    obj[m[0]] = v
                return None
            else:
                return obj.__recv__(msg)
        elif isinstance(msg, Partial):
            def part(*args, **kwargs):
                return send(obj, msg.merge(hybrid(args, kwargs)))
            return part

        # print(obj)
        raise
    else:
        # print(f, obj)
        return f(obj, msg)

    # if isinstance(msg, (list, tuple)):
    #     return obj(*msg)
    # elif isinstance(msg, dict):
    #     return obj(**msg)
    # elif isinstance(msg, hybrid):
    #     return obj(*msg.tuple, **msg.dict)
    # elif isinstance(msg, str) and not msg.startswith("__"):
    #     return getattr(obj, msg)
    # elif isinstance(msg, index):
    #     return obj[msg.item]
    # elif isinstance(msg, hs.assign):
    #     m, v = msg[:]
    #     if isinstance(m, str):
    #         return setattr(obj, m, v)
    #     elif isinstance(m, index):
    #         obj[m.item] = v
    #         return None
    #     else:
    #         return obj.__recv__(msg)
    # else:
    #     try:
    #         f = obj.__recv__
    #     except AttributeError:
    #         if isinstance(msg, str):
    #             return getattr(obj, msg)
    #         else:
    #             raise
    #     else:
    #         return f(msg)

@library_function
def send_safeguard(obj, msg):
    _SHOW_FRAME = SHF
    try:
        f = obj.__recv_safeguard__
    except AttributeError:
        if isinstance(msg, (tuple, list)):
            return hs.ok(obj(*msg))
        elif isinstance(msg, dict):
            return hs.ok(obj(**msg))
        elif isinstance(msg, hybrid):
            return hs.ok(obj(*msg.tuple, **msg.dict))
        elif isinstance(msg, str):
            return hs.ok(getattr(obj, msg))
        elif isinstance(msg, hs.index):
            return hs.ok(obj[msg[0]])
        raise
    else:
        return f(msg)


library_function("chain")(itertools.chain)

@library_function("map")
def ugmap(seq, obj):
    return list(uggen(seq, obj))

@library_function("gen")
def uggen(seq, obj):
    _SHOW_FRAME = SHF
    for entry in seq:
        try:
            result = send_safeguard(obj, entry)
        except BreakException as e:
            if hasattr(e, "value"):
                yield e.value
            break
        except ContinueException as e:
            if hasattr(e, "value"):
                yield e.value
            continue

        if isinstance(result, hs.ok):
            yield result[0]
        elif isinstance(result, hs.guard_fail):
            continue
        else:
            raise TypeError("Match failed", result)

@library_function("each")
def ugeach(seq, obj):
    _SHOW_FRAME = SHF
    result = None
    for entry in seq:
        try:
            result = send_safeguard(obj, entry)
        except BreakException as e:
            if hasattr(e, "value"):
                result = e.value
            else:
                result = None
            break
        except ContinueException as e:
            continue

        if isinstance(result, (hs.ok, hs.guard_fail)):
            result = result[0]
        else:
            raise TypeError("Match failed", result[0])
    return result


@library_function("while")
def ugwhile(test, body):
    _SHOW_FRAME = SHF
    result = None
    while test():
        try:
            result = body()
        except BreakException as e:
            if hasattr(e, "value"):
                result = e.value
            else:
                result = None
            break
        except ContinueException as e:
            if hasattr(e, "value"):
                result = e.value
            else:
                result = None
            continue
    return result


@library_function
def patch_dict(*args):
    _SHOW_FRAME = SHF
    if len(args) == 0:
        return {}
    else:
        rval = {}
        for entry in args:
            rval.update(entry)
        return rval

@library_function
def patch_odict(*args):
    _SHOW_FRAME = SHF
    if len(args) == 0:
        return OrderedDict()
    else:
        rval = OrderedDict()
        for entry in args:
            rval.update(entry)
        return rval

@library_function
def patch_tuple(*args):
    _SHOW_FRAME = SHF
    if len(args) == 0:
        return ()
    elif len(args) == 1:
        return tuple(args[0])
    else:
        return reduce(lambda x, y: tuple(x) + tuple(y), args)

@library_function
def patch_list(*args):
    _SHOW_FRAME = SHF
    if len(args) == 0:
        return ()
    elif len(args) == 1:
        return list(args[0])
    else:
        return reduce(lambda x, y: list(x) + list(y), args)

@library_function
def assign(obj, item, value):
    _SHOW_FRAME = SHF
    if isinstance(item, hs.index):
        obj[item[0]] = value
    elif isinstance(item, str):
        setattr(obj, item, value)
    else:
        obj.__recv__(hs.assign(item, value))


class ugobj:

    def __call__(self, *args, **kwargs):
        _SHOW_FRAME = SHF
        if kwargs:
            if args:
                return self.__recv__(hybrid(args, kwargs))
            else:
                return self.__recv__(kwargs)
        else:
            return self.__recv__(args)

    def __getattr__(self, attr):
        _SHOW_FRAME = SHF
        if attr.startswith('__'):
            return getattr(super(), attr)
        else:
            return self.__recv__(attr)

    def __getitem__(self, item):
        _SHOW_FRAME = SHF
        return self.__recv__(hs.index(item))

    def __setattr__(self, attr, value):
        _SHOW_FRAME = SHF
        if attr.startswith('__'):
            setattr(super(), attr, value)
        else:
            return self.__recv__(hs.assign(attr, value))

    def __setitem__(self, item, value):
        _SHOW_FRAME = SHF
        return self.__recv__(hs.assign(hs.index(item), value))


class uglitobj(ugobj):
    pass

@library_function
def make_object(*specifications):

    class R(uglitobj):

        __ugspecs__ = specifications

        def __recv_safeguard__(self, arg):
            _SHOW_FRAME = SHF
            errors = []
            guards = []
            for deconstructor, guard, f in specifications:
                try:
                    args = deconstructor(arg)
                except TypeError as e:
                    errors.append(e)
                    continue
                if guard and not guard(*args):
                    guards.append(GuardError(guard = guard))
                    continue
                return hs.ok(f(*args))
            if errors:
                return hs.fail(errors)
            return hs.guard_fail(guards)

        def __recv__(self, arg):
            _SHOW_FRAME = SHF
            errors = []
            for deconstructor, guard, f in specifications:
                try:
                    args = deconstructor(arg)
                except TypeError as e:
                    errors.append(e)
                    continue
                if guard and not guard(*args):
                    errors.append(GuardError(guard = guard))
                    continue
                return f(*args)
            raise MatchError(errors = errors)

    return R()


@library_function
def do(gen):
    _SHOW_FRAME = SHF
    for x in gen:
        pass


library_function("to")(range)

@library_function("..")
def to(start, end):
    if start is Void: start = None
    if end is Void: end = None
    return slice(start, end)

@library_function("by")
def by(r, step):
    _SHOW_FRAME = SHF
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
    _SHOW_FRAME = SHF
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

@library_function
def escape(f):
    raise Exception("unsupported")


library_function("nonzero")(bool)


@library_function
def apply_guard(guard):
    _SHOW_FRAME = SHF
    if not guard():
        raise GuardError(guard = guard)


class ugtuple(tuple):
    def __init__(self, t):
        _SHOW_FRAME = SHF
        super().__init__(t)
        self.__tags__ = {}

class uglist(list):
    def __init__(self, t):
        _SHOW_FRAME = SHF
        super().__init__(t)
        self.__tags__ = {}

@library_function
def maytag(obj, name, value):
    _SHOW_FRAME = SHF
    if isinstance(obj, tuple):
        obj = ugtuple(obj)
    elif isinstance(obj, list):
        obj = uglist(obj)
    elif isinstance(obj, str):
        obj = ugstr(obj)
    elif isinstance(obj, FunctionType):
        obj.__tags__ = {}
    try:
        return tag(obj, name, value)
    except AttributeError:
        return obj


@library_function
def make_class(bases, elements):
    _SHOW_FRAME = SHF
    d = {}
    for k, v in elements.items():
        if isinstance(v, uglitobj):
            specs = v.__ugspecs__
            if len(specs) != 1:
                raise Exception("class method can only have a single clause")
            deconstructor, guard, f = specs[0]
            formula = deconstructor.formula
            if (not guard
                and isinstance(formula, (tuple, list))
                and not [x for x in formula if not isinstance(x, str)]):
                f.__name__ = k
                d[k] = f
            else:
                raise Exception("class method must be simple")
        else:
            d[k] = v
    rval = type("__", tuple(bases), d)
    rval.__module__ = "abc"
    return rval

@library_function
def setattribute(x, attr, value):
    _SHOW_FRAME = SHF
    setattr(x, attr, value)
    return x


@library_function
def imp(specifications):
    _SHOW_FRAME = SHF
    results = []
    for entry in specifications:
        if isinstance(entry, str):
            results.append(__import__(entry))
        elif isinstance(entry, hs.import_from):
            m = __import__(entry[0], fromlist = entry[1:])
            results.append([getattr(m, x) for x in entry[1:]])

    return results


library_function(struct)


@library_function
def dynlet(variables, body):
    return body()


class Wrap(ugobj):
    def __recv_safeguard__(self, message):
        return hs.ok((message,))
    def __recv__(self, message):
        return (message,)

library_function("wrap")(Wrap())


class frozendict(dict):
    def __setattr__(self, attr, value):
        raise TypeError("'frozendict' object does not support setting attributes")
    def __setitem__(self, item, value):
        raise TypeError("'frozendict' object does not support item assignment")
    def __str__(self):
        return "frozendict(%s)" % super().__str__()

class frozenOrderedDict(OrderedDict):
    def __init__(self, *args, **kwargs):
        self.__locked = False
        super().__init__(*args, **kwargs)
        self.__locked = True
            
    def __setitem__(self, item, value):
        if self.__locked:
            raise TypeError("'frozenOrderedDict' object does not support item assignment")
        else:
            super().__setitem__(item, value)

class Frz(ugobj):
    def __recv_safeguard__(self, message):
        return hs.ok(self.__recv__(message))
    def __recv__(self, message):
        _SHOW_FRAME = SHF
        # print(message)
        if isinstance(message, list):
            return tuple(message)
        elif isinstance(message, set):
            return frozenset(message)
        elif isinstance(message, OrderedDict):
            return frozenOrderedDict(message)
        elif isinstance(message, dict):
            return frozendict(message)
        elif isinstance(message, (tuple, frozenset, frozendict, frozenOrderedDict)):
            return message
        elif isinstance(message, ugobj):
            return send(message, frz)()
        else:
            return message.__frz__()

class Mut(ugobj):
    def __recv_safeguard__(self, message):
        return hs.ok(self.__recv__(message))
    def __recv__(self, message):
        _SHOW_FRAME = SHF
        # print(message)
        if isinstance(message, tuple):
            return list(message)
        elif isinstance(message, frozenset):
            return set(message)
        elif isinstance(message, frozenOrderedDict):
            return OrderedDict(message)
        elif isinstance(message, frozendict):
            return dict(message)
        elif isinstance(message, (list, set, dict, OrderedDict)):
            return message
        else:
            return message.__mut__()

frz = Frz()
library_function("frz")(frz)
library_function("mut")(Mut())

