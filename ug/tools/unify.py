
from copy import copy
from functools import partial


################################

class Keyword:

    def __init__(self, name, nonzero=True):
        self.name = name
        self.nonzero = nonzero

    def __nonzero__(self):
        return self.nonzero

    def __str__(self):
        return "<%s>" % self.name

    def __repr__(self):
        return "<%s>" % self.name

ABORT = Keyword("ABORT", False)
RETRY = Keyword("RETRY", False)
FAILURE = Keyword("FAILURE", False)


simple_types = (int, float, str, bool, None.__class__, Keyword)


ANY_TYPE = Keyword("ANY_TYPE")
FALL_THROUGH = Keyword("FALL_THROUGH")

def comm_guard(type1, type2):
    def wrap(f):
        old_f = f.__globals__[f.__name__]
        def new_f(arg1, arg2, *rest):
            if (type1 is ANY_TYPE or isinstance(arg1, type1)) \
                   and (type2 is ANY_TYPE or isinstance(arg2, type2)):
                pass
            elif (type1 is ANY_TYPE or isinstance(arg2, type1)) \
                     and (type2 is ANY_TYPE or isinstance(arg1, type2)):
                arg1, arg2 = arg2, arg1
            else:
                try:
                    return old_f(arg1, arg2, *rest)
                except:
                    raise

            try:
                variable = f(arg1, arg2, *rest)
            except:
                raise
            if variable is FALL_THROUGH:
                try:
                    return old_f(arg1, arg2, *rest)
                except:
                    raise
            else:
                return variable

        new_f.__name__ = f.__name__
        def typename(type):
            if isinstance(type, Keyword):
                return str(type)
            elif isinstance(type, (tuple, list)):
                return "(" + ", ".join([x.__name__ for x in type]) + ")"
            else:
                return type.__name__
        new_f.__doc__ = str(old_f.__doc__) + "\n" + ", ".join([typename(type) for type in (type1, type2)]) + "\n" + str(f.__doc__ or "")
        return new_f
    return wrap




################################


class Variable:
    """
    Serves as a base class of variables for the purpose of unification.
    Behavior for unifying various types of variables should be added as
    overloadings of the 'unify' function.
    """
    def __init__(self, name = "?"):
        self.name = name
    def __str__(self):
        return self.__class__.__name__ + "(" + ", ".join(["%s=%s" % (key, value) for key, value in self.__dict__.items()]) + ")"
    def __repr__(self):
        return str(self)

class FreeVariable(Variable):
    """
    This Variable can take any value.
    """
    def __str__(self):
        return "?"+self.name
    def __repr__(self):
        return str(self)

class BoundVariable(Variable):
    """
    This Variable is bound to a value accessible via the value field.
    """
    def __init__(self, name, value):
        self.name = name
        self.value = value

class OrVariable(Variable):
    """
    This Variable could be any value from a finite list of values,
    accessible via the options field.
    """
    def __init__(self, name, options):
        self.name = name
        self.options = options

class NotVariable(Variable):
    """
    This Variable can take any value but a finite amount of forbidden
    values, accessible via the not_options field.
    """
    def __init__(self, name, not_options):
        self.name = name
        self.not_options = not_options

class VariableInList: # not a subclass of Variable
    """
    This special kind of variable is matched against a list and unifies
    an inner Variable to an OrVariable of the values in the list. For
    example, if we unify VariableInList(FreeVariable('x')) to [1,2,3],
    the 'x' variable is unified to an OrVariable('?', [1,2,3]).
    """
    def __init__(self, variable):
        self.variable = variable

class RestVariable: # not a subclass of Variable
    """
    This special kind of variable is matched against the remainder of
    a list. For instance, if we unify
    [1, RestVariable(FreeVariable('x'))] to [1,2,3], then 'x' is unified
    to BoundVariable([2, 3])
    """
    def __init__(self, variable):
        self.variable = variable

class Or(Variable):
    def __init__(self, *options):
        self.options = options

class ChoiceTaken(Variable):
    def __init__(self, orig, value):
        self.orig = orig
        self.value = value

class And(Variable):
    def __init__(self, *clauses):
        self.clauses = clauses

class Not(Variable):
    def __init__(self, *options):
        self.options = options


################################

class VariableFactory:

    def __init__(self, vartype):
        self.vartype = vartype
        self.vars = {}

    def __call__(self, name, *args, **kwargs):
        sig = (self.vartype, name)
        if sig in self.vars:
            return self.vars[sig]
        else:
            v = self.vartype(name, *args, **kwargs)
            self.vars[sig] = v
            return v


Var = VariableFactory(FreeVariable)
V = Var
OrV = VariableFactory(OrVariable)
NV = VariableFactory(NotVariable)


################################


class Unification:
    """
    This class represents a possible unification of a group of variables
    with each other or with tangible values.
    """

    def __init__(self, inplace = False):
        """
        If inplace is False, the merge method will return a new Unification
        that is independant from the previous one (which allows backtracking).
        """
        self.unif = {}
        self.inplace = inplace

    def merge(self, new_best, *vars):
        """
        Links all the specified vars to a Variable that represents their
        unification.
        """
        if self.inplace:
            U = self
        else:
            # Copy all the unification data.
            U = Unification(self.inplace)
            for var, (best, pool) in self.unif.items():
                # The pool of a variable is the set of all the variables that
                # are unified to it (all the variables that must have the same
                # value). The best is the Variable that represents a set of
                # values common to all the variables in the pool.
                U.unif[var] = (best, pool)
        # We create a new pool for our new set of unified variables, initially
        # containing vars and new_best
        new_pool = set(vars)
        new_pool.add(new_best)
        for var in copy(new_pool):
            best, pool = U.unif.get(var, (var, set()))
            # We now extend the new pool to contain the pools of all the variables.
            new_pool.update(pool)
        # All variables get the new pool.
        for var in new_pool:
            U.unif[var] = (new_best, new_pool)
        return U

    def __getitem__(self, v):
        """
        For a variable v, returns a Variable that represents the tightest
        set of possible values it can take.
        """
        return self.unif.get(v, (v, None))[0]

    def walk(self, a, b):
        return unify_walk(a, b, self)


################################


def unify_walk(a, b, U):
    """
    unify_walk(a, b, U) returns an Unification where a and b are unified, given the
    unification that already exists in the Unification U. If the unification fails,
    it returns False.

    There are two ways to expand the functionality of unify_walk. The first way is:
    @comm_guard(type_of_a, type_of_b)
    def unify_walk(a, b, U):
        ...
    A function defined as such will be executed whenever the types of a and b
    match the declaration. Note that comm_guard automatically guarantees that
    your function is commutative: it will try to match the types of a, b or b, a.
    It is recommended to define unify_walk in that fashion for new types of Variable
    because different types of Variable interact a lot with each other, e.g.
    when unifying an OrVariable with a NotVariable, etc. You can return the
    special marker FALL_THROUGH to indicate that you want to relay execution
    to the next match of the type signature. The definitions of unify_walk are tried
    in the reverse order of their declaration.

    Another way is to override __unify_walk__ in an user-defined class.

    Limitations: cannot embed a Variable in another (the functionality could
    be added if required)

    Here is a list of unification rules with their associated behavior:
        
    """
    if a.__class__ != b.__class__:
        return False
    elif a == b:
        return U
    else:
        return False

@comm_guard(FreeVariable, ANY_TYPE)
def unify_walk(fv, o, U):
    """
    FreeV is unified to BoundVariable(other_object)
    """
    v = BoundVariable("?", o)
    return U.merge(v, fv)

@comm_guard(BoundVariable, ANY_TYPE)
def unify_walk(bv, o, U):
    """
    The unification succeed iff BV.value == other_object
    """
    if bv.value == o:
        return U
    else:
        return False

@comm_guard(OrVariable, ANY_TYPE)
def unify_walk(ov, o, U):
    """
    The unification succeeds iff other_object in OrV.options
    """
    if o in ov.options:
        v = BoundVariable("?", o)
        return U.merge(v, ov)
    else:
        return False

@comm_guard(NotVariable, ANY_TYPE)
def unify_walk(nv, o, U):
    """
    The unification succeeds iff other_object not in NV.not_options
    """
    if o in nv.not_options:
        return False
    else:
        v = BoundVariable("?", o)
        return U.merge(v, nv)

@comm_guard(FreeVariable, Variable)
def unify_walk(fv, v, U):
    """
    Both variables are unified.
    """
    v = U[v]
    return U.merge(v, fv)

@comm_guard(BoundVariable, Variable)
def unify_walk(bv, v, U):
    """
    V is unified to BV.value
    """
    return unify_walk(v, bv.value, U)

@comm_guard(OrVariable, OrVariable)
def unify_walk(a, b, U):
    """
    OrV(list1) == OrV(list2) == OrV(intersection(list1, list2))
    """
    opt = intersection(a.options, b.options)
    if not opt:
        return False
    elif len(opt) == 1:
        v = BoundVariable("?", opt[0])
    else:
        v = OrVariable("?", opt)
    return U.merge(v, a, b)

@comm_guard(NotVariable, NotVariable)
def unify_walk(a, b, U):
    """
    NV(list1) == NV(list2) == NV(union(list1, list2))
    """
    opt = union(a.not_options, b.not_options)
    v = NotVariable("?", opt)
    return U.merge(v, a, b)

@comm_guard(OrVariable, NotVariable)
def unify_walk(o, n, U):
    """
    OrV(list1) == NV(list2) == OrV(list1 \ list2)
    """
    opt = [x for x in o.options if x not in n.not_options]
    if not opt:
        return False
    elif len(opt) == 1:
        v = BoundVariable("?", opt[0])
    else:
        v = OrVariable("?", opt)
    return U.merge(v, o, n)

@comm_guard(VariableInList, (list, tuple))
def unify_walk(vil, l, U):
    """
    Unifies VIL's inner Variable to OrV(list).
    """
    v = vil.variable
    ov = OrVariable("?", l)
    return unify_walk(v, ov, U)

@comm_guard((list, tuple), (list, tuple))
def unify_walk(l1, l2, U):
    """
    Tries to unify each corresponding pair of elements from l1 and l2.
    """
    if isinstance(l1[-1], RestVariable):
        if len(l1) > len(l2) + 1:
            return False
        U = unify_walk(l1[-1].variable, l2[len(l1)-1:], U)
        l1 = l1[:-1]
    elif isinstance(l2[-1], RestVariable):
        return unify_walk(l2, l1, U)
    else:
        if len(l1) != len(l2):
            return False
    i = 0
    for x1, x2 in zip(l1, l2):
        U = unify_walk(x1, x2, U)
        if not U:
            return False
    return U

@comm_guard(dict, dict)
def unify_walk(d1, d2, U):
    """
    Tries to unify values of corresponding keys.
    """
    for (k1, v1) in d1.items():
        if k1 in d2:
            U = unify_walk(v1, d2[k1], U)
            if U is False:
                return False
    return U

@comm_guard(ANY_TYPE, ANY_TYPE)
def unify_walk(a, b, U):
    """
    Checks for the existence of the __unify_walk__ method for one of
    the objects.
    """
    if not isinstance(a, Variable) and not isinstance(b, Variable) \
           and hasattr(a, "__unify_walk__"):
        return a.__unify_walk__(b, U)
    else:
        return FALL_THROUGH

@comm_guard(Variable, ANY_TYPE)
def unify_walk(v, o, U):
    """
    This simply checks if the Var has an unification in U and uses it
    instead of the Var. If the Var is already its tighest unification,
    falls through.
    """
    best_v = U[v]
    if v is not best_v:
        return unify_walk(o, best_v, U) # reverse argument order so if o is a Variable this block of code is run again
    else:
        return FALL_THROUGH # call the next version of unify_walk that matches the type signature

@comm_guard(Or, ANY_TYPE)
def unify_walk(ov, o, U):
    for i, opt in enumerate(ov.options):
        U2 = unify_walk(o, opt, U)
        if U2:
            return U2.merge(ChoiceTaken(ov, i), ov)
    else:
        return False

@comm_guard(And, ANY_TYPE)
def unify_walk(av, o, U):
    for clause in av.clauses:
        U = unify_walk(o, clause, U)
        if not U:
            return False
    return U

@comm_guard(Not, ANY_TYPE)
def unify_walk(nv, o, U):
    for opt in nv.options:
        U2 = unify_walk(o, opt, U)
        if U2:
            return False
    return U



################################


class FVar:

    def __init__(self, fn, *args):
        self.fn = fn
        self.args = args

    def __call__(self, u):
        return self.fn(*[unify_build(arg, u) for arg in self.args])


################################


def unify_merge(a, b, U):
    return a

@comm_guard(Variable, ANY_TYPE)
def unify_merge(v, o, U):
    return v

@comm_guard(BoundVariable, ANY_TYPE)
def unify_merge(bv, o, U):
    return bv.value

@comm_guard(ChoiceTaken, ANY_TYPE)
def unify_merge(ch, o, U):
    return unify_merge(ch.orig.options[ch.value], o, U)

@comm_guard(Not, ANY_TYPE)
def unify_merge(nv, o, U):
    return o

@comm_guard(And, ANY_TYPE)
def unify_merge(av, o, U):
    result = o
    for clause in av.clauses:
        result = unify_merge(clause, result, U)
    return result

@comm_guard(VariableInList, (list, tuple))
def unify_merge(vil, l, U):
    return [unify_merge(x,x,U) for x in l]

@comm_guard((list, tuple), (list, tuple))
def unify_merge(l1, l2, U):
    return [unify_merge(x1, x2, U) for x1, x2 in zip(l1, l2)]

@comm_guard(dict, dict)
def unify_merge(d1, d2, U):
    d = d1.__class__()
    for k1, v1 in d1.items():
        if k1 in d2:
            d[k1] = unify_merge(v1, d2[k1], U)
        else:
            d[k1] = unify_merge(v1, v1, U)
    for k2, v2 in d2.items():
        if not k2 in d1:
            d[k2] = unify_merge(v2, v2, U)
    return d

@comm_guard(FVar, ANY_TYPE)
def unify_merge(vs, o, U):
    return vs(U)

@comm_guard(ANY_TYPE, ANY_TYPE)
def unify_merge(a, b, U):
    if not isinstance(a, Variable) and not isinstance(b, Variable) \
           and hasattr(a, "__unify_merge__"):
        return a.__unify_merge__(b, U)
    else:
        return FALL_THROUGH

@comm_guard(Variable, ANY_TYPE)
def unify_merge(v, o, U):
    """
    This simply checks if the Var has an unification in U and uses it
    instead of the Var. If the Var is already its tighest unification,
    falls through.
    """
    best_v = U[v]
    if v is not best_v:
        return unify_merge(o, best_v, U) # reverse argument order so if o is a Variable this block of code is run again
    else:
        return FALL_THROUGH # call the next version of unify_walk that matches the type signature

@comm_guard(Variable, ANY_TYPE)
def unify_merge(v, o, U):
    """
    This simply checks if the Var has an unification in U and uses it
    instead of the Var. If the Var is already its tighest unification,
    falls through.
    """
    best_v = U[v]
    if v is not best_v:
        return unify_merge(o, best_v, U) # reverse argument order so if o is a Variable this block of code is run again
    else:
        return FALL_THROUGH # call the next version of unify_walk that matches the type signature
    


################################


def unify_build(x, U):
    return unify_merge(x, x, U)


################################


def unify(a, b):
    U = unify_walk(a, b, Unification())
    if not U:
        return None, False
    else:
        return unify_merge(a, b, U), U


################################


if __name__ == "__main__":

    vx = NotVariable("x", ["big", "bones"])
    vy = OrVariable("y", ["hello", "big"])
    vz = V("z")
    va = V("a")
    vl = VariableInList(vz)

    pattern1 = dict(hey=vx, ulala=va, a=1)
    pattern2 = dict(hey=vy, ulala=10, b=2)

    U = unify_walk(pattern1, pattern2, Unification())

    if U:
        print(U[va])
        print(U[vx])
        print(U[vy])
        print(U[vz])
        print(unify_merge(pattern1, pattern2, U))
    else:
        print("no match")


    U = unify_walk((1, 2), (va, va), Unification())
    if U:
        print(U[va])
    else:
        print("not unifiable")

    U = unify_walk((1, 2, 3, 4, 5), (1, 2, RestVariable(va)), Unification())
    if U:
        print(U[va])
    else:
        print("not unifiable")

    print("-----------")

    # U = unify_walk([Or([V("a"), V("a")], [5, V("a")]), 5],
    #                [[7, 7], Not(8)],
    #                Unification())
    # print(U[V("a")])

    print(unify([[7, 7], Not(8)],
                [Or([V("a"), V("a")], [5, V("a")]), 5])[0])

    print(unify([[7, 7], Not(8)],
                [[V("a"), V("a")], 5])[0])

    print(unify([[8, V("c")], And(9, V("b"))],
                [[V("a"), V("b")], 9])[0])


