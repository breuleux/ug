
class attrdict(dict):
    __slots__ = []
    def __getattr__(self, attr):
        if attr.startswith("__"):
            return super().__getattr__(attr)
        try:
            return self[attr]
        except KeyError:
            raise AttributeError(attr, self)
    def __setattr__(self, attr, value):
        if attr.startswith("__"):
            super().__setattr__(attr, value)
        self[attr] = value

class ugstr(str):
    def __init__(self, *args):
        self.__tags__ = attrdict()
        super().__init__(*args)
    def tag(self, attr, value):
        tag(self, attr, value)
        return self

def tag(obj, attr, value):
    obj.__tags__[attr] = value
    return obj

def gettag(obj, attr):
    try:
        return obj.__tags__[attr]
    except KeyError:
        raise KeyError("No tag named '%s'" % attr, obj)

def hastag(obj, attr):
    return hasattr(obj, '__tags__') and attr in obj.__tags__

