class Application(object):
    def __init__(self, func, args):
        assert isinstance(func, (FunObj, ConObj, Application)), str(func)
        assert isinstance(args, list), str(args)

        self.func = func
        self.args = args
        self.__value = None

    def saturate(self, next):
        if self.func is None or self.__value:
            return next

        if isinstance(self.func, Application) and len(self.func.args) < self.func.func.arity:
            self.args = self.func.args + self.args
            self.func = self.func.func
            return self.saturate(next)

        assert isinstance(self.func, (FunObj, ConObj)), str(self.func)

        if len(self.args) < self.func.arity:
            return next

        if isinstance(self.func, ConObj):
            self.__value = self.func
            return next

        push_frame(self, next)
        return self.func.code[0]

    def __get_value(self):
        #print "__get_value"
        return self.__value
    def setValue(self, next, value):
        #print "__set_value", value
        if isinstance(value, Application): # and isinstance(value.func, FunObj):
            self.args = self.args[self.func.arity:] + value.args
            self.func = value.func
            if self.func and len(self.args) >= self.func.arity:
                return self.saturate(next)
        elif isinstance(value, Application) and isinstance(value.func, ConObj):
            self.args = value.args
            self.func = value.func
            return next
        elif isinstance(value, Value):
            self.func = None
            self.args = []
        self.__value = value
        return next
    value = property(__get_value, None)

    def __get_arity(self):
        if self.func:
            return self.func.arity
        else:
            return 0
    arity = property(__get_arity, None)
    def __get_tag(self):
        if self.value:
            return self.value.tag
        elif self.func:
            return self.func.tag
        else:
            raise AttributeError
    tag = property(__get_tag, None)

    def __str__(self):
        return "<App %s %i(%i) %s>" % (self.func, len(self.args), self.arity, self.value)

    def __repr__(self):
        return str(self)

class Value:
    def __init__(self, val):
        self.value = val
        self.args = []
        self.arity = 0

    def saturate(self, next):
        return next

    def __str__(self):
        return "<Value %s>" % (repr(self.value), )
    def __repr__(self):
        return str(self)

from objs import FunObj, ConObj
from state import stack, lookup, tramampoline, push_frame, pop_frame, get_app, saturate
