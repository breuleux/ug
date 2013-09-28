
import exc
from copy import copy
from collections import OrderedDict
from weakref import WeakValueDictionary
from .utilities import attrdict

class TypeError(exc.TypeError):
    def __init__(self, **keywords):
        self.__dict__.update(keywords)
    def __str__(self):
        return "[%s]" % (", ".join("%s = %s" % (k, v)
                                   for k, v in self.__dict__.items()))
    def __repr__(self):
        return str(self)


class hybrid:
    def __init__(self, t, d):
        self.tuple = tuple(t)
        self.dict = dict(d)
    def __str__(self):
        return str(self.tuple) + "&" + str(self.dict)
    def __repr__(self):
        return str(self)

class index:
    def __init__(self, item):
        self.item = item
    def __str__(self):
        return "?(" + repr(self.item) + ")"
    def __repr__(self):
        return "?(" + repr(self.item) + ")"


class structmc(type):
    def __deconstruct__(self, v):
        if isinstance(v, self):
            return hybrid(v.__l__, v.__d__)
        else:
            raise TypeError['deconstruct'](
                value = v,
                type = type(v),
                expected = self)
    def __str__(self):
        return self.__prefix__ + self.__name__
    def __repr__(self):
        return str(self)
    def __descr__(self, descr):
        return str(self)


class struct(metaclass = structmc):

    __prefix__ = '?'
    __name__ = '?'
    __slots__ = ['__l__', '__d__', '__mutable__', '__extendable__', '__tags__']

    def __init__(self, *args, __mutable__ = True, __extendable__ = True, **kwargs):
        self.__l__ = list(args) if __mutable__ or __extendable__ else args
        self.__d__ = kwargs
        self.__mutable__ = __mutable__
        self.__extendable__ = __extendable__
        self.__tags__ = attrdict()

    def __mut__(self):
        return type(self)(*self.__l__,
                           __mutable__ = True,
                           __extendable__ = True,
                           **self.__d__)

    def __frz__(self):
        return type(self)(*self.__l__,
                           __mutable__ = False,
                           __extendable__ = False,
                           **self.__d__)

    def __recv__(self, message):
        if isinstance(message, str):
            return getattr(self, message)
        elif isinstance(message, index):
            return self[message.item]
        elif isinstance(message, hashstruct.assign):
            m, v = message[:]
            if isinstance(m, index):
                m = m.item
            elif not isinstance(m, str):
                raise Exception("%s does not acknowledge message %s" % (self, message))
            self[m] = v
        else:
            raise Exception("%s does not acknowledge message %s" % (self, message))

    def __getattr__(self, attr):
        if attr.startswith('__'):
            # return super().__getattr__(attr)
            return getattr(super(), attr)
        else:
            try:
                return self[attr]
            except KeyError as k:
                raise AttributeError(self, k.args)

    def __setattr__(self, attr, value):
        if attr.startswith('__'):
            super().__setattr__(attr, value)
        else:
            self[attr] = value

    def __getitem__(self, k):
        if isinstance(k, (int, slice)):
            return self.__l__[k]
        else:
            return self.__d__[k]

    def __setitem__(self, k, v):
        if self.__mutable__:
            if isinstance(k, (int, slice)):
                self.__l__[k] = v
            else:
                if k in self.__d__ or self.__extendable__:
                    self.__d__[k] = v
                else:
                    raise KeyError("Field not in structure.")
        else:
            raise TypeError("Not mutable.")

    # def __len__(self):
    #     return len(self.__l__)

    def __hash__(self):
        if self.__mutable__ or self.__extendable__:
            raise TypeError("not hashable")
        else:
            r = hash(self.__name__)
            for entry in self.__l__:
                r ^= hash(entry)
            for k, v in self.__d__.items():
                r ^= hash(k) ^ hash(v)
            return r

    def __eq__(self, other):
        return (isinstance(other, self.__class__)
                and self.__l__ == other.__l__
                and self.__d__ == other.__d__)

    def __unify_walk__(self, other, U):
        if isinstance(other, type(self)):
            U = U.walk(self.__l__, other.__l__)
            if U:
                return U.walk(self.__d__, other.__d__)
            else:
                return False
        return False

    def __str__(self):
        delims = "[]" # if isinstance(self.__d__, OrderedDict) else "{}"
        return "%s%s%s%s%s" % (self.__prefix__,
                               self.__name__,
                               delims[0],
                               ", ".join(list(map(repr, self.__l__))
                                         + ["%r = %r" % (k, v)
                                            for k, v in self.__d__.items()]),
                               delims[1])

    def __repr__(self):
        return str(self)

    def __descr__(self, descr):
        # name = self.__prefix__ + self.__name__
        # return ({"@struct", "@struct.%s" % name},
        #         ({"label"}, name),
        #         (({"sequence"},)
        #          + tuple(descr(x) for x in self.__l__)
        #          + tuple(({"assoc"}, descr(k), descr(v))
        #                  for k, v in self.__d__.items())))

        name = self.__prefix__ + self.__name__
        return [(({"@struct", "@struct.%s" % name, "+"+name, "object"},)
                 + tuple(descr(x) for x in self.__l__)
                 + tuple(({"field", "+"+str(k)}, descr(v))
                         for k, v in self.__d__.items()))]
                
                 
                 


class StructTypeFactory:

    def __init__(self, prefix):
        self.prefix = prefix
        self.cache = WeakValueDictionary()

    def __getitem__(self, name):
        try:
            return self.cache[name]
        except KeyError:
            t = type(name,
                     (struct,),
                     dict(__prefix__ = self.prefix,
                          __name__ = name,
                          __slots__ = []))
            self.cache[name] = t
            return t

    def __getattr__(self, attr):
        if attr.startswith("__"):
            return super().__getattr__(attr)
        else:
            return self[attr]

    def __call__(self, *args, **kwargs):
        return self[""](*args, **kwargs)


hashstruct = StructTypeFactory("#")
anonstruct = StructTypeFactory("")
