import struct
from StringIO import StringIO

#import profile

class FunObj:
    def __init__(self, code):
        #profile.enter("funobj")
        self.arity = struct.unpack(">B", code.read(1))[0]
        self.stack = struct.unpack(">H", code.read(2))[0]
        self.consts = ConstTable(code)
        codeLength = struct.unpack(">H", code.read(2))[0]
        #profile.enter("funobj:loadbc")
        self.__code = code.read(codeLength)
        #profile.exit("funobj:loadbc")
        #profile.exit("funobj")

    def eval(self, args):
        app = Application(state.lookup(self.id), args)
        state.push_frame(app, None)
        state.tramampoline(self.code[0])

    def saturate(self):
        state.tramampoline(self.code[0])
        return state.stack.pop()

    def lookup(self, strtbl):
        for c in self.consts:
            if isinstance(c, FullyQualifId):
                c.lookup(strtbl)

    def apply(self, state, args):
        assert len(args) == self.arity
        return Application(self, args)

    def lazyLoadBC(self):
        self.__dict__["code"] = loadbc(StringIO(self.__code))
        del self.__code
        return self.code
    code = property(lazyLoadBC)

    def __str__(self):
        return "<FunObj %s %i>" % (self.name, len(self.code))
    def __repr__(self):
        return str(self)

class ConObj:
    def __init__(self, code):
        #profile.enter("conobj")
        self.size = struct.unpack(">B", code.read(1))[0]
        self.tag = struct.unpack(">B", code.read(1))[0]
        self.arity = self.size
        #profile.exit("conobj")

    def lookup(self, strtbl):
        pass

    def saturate(self):
        return self

    def __str__(self):
        return "<ConObj>"
    def __repr__(self):
        return str(self)

objtypes = {"F": FunObj, "C": ConObj}

def ConstTable(code):
    #profile.enter("const table")
    tbl = []
    num = struct.unpack(">H", code.read(2))[0]
    for i in range(num):
        type = code.read(1)
        if type in "AF0CZPX":
            data = FullyQualifId(code)
        elif type == "i":
            data = struct.unpack(">I", code.read(4))
        elif type == "l":
            size = struct.unpack(">b", code.read(1))[0]
            bytes = struct.unpack(">%iB" % (abs(size), ), code.read(abs(size)))
            data = 0L
            for b in bytes:
                data = data << 8
                data = data | b
            if size < 0:
                data = -data
        elif type == "f":
            size = struct.unpack(">b", code.read(1))[0]
            bytes = struct.unpack(">%iB" % (abs(size), ), code.read(abs(size)))
            mant = 0L
            for b in bytes:
                mant = mant << 8
                mant = mant | b
            exp = struct.unpack(">h", code.read(2))[0]
            data = float("0."+str(mant))*10**exp
            if size < 0:
                data = -data
        elif type == "d":
            size = struct.unpack(">b", code.read(1))[0]
            bytes = struct.unpack(">%iB" % (abs(size), ), code.read(abs(size)))
            mant = 0L
            for b in bytes:
                mant = mant << 8
                mant = mant | b
            exp = struct.unpack(">h", code.read(2))[0]
            data = float("0."+str(mant))*10**exp
            if size < 0:
                data = -data
        elif type == "s":
            size = struct.unpack(">H", code.read(2))[0]
            data = struct.unpack(">%is" % (size, ), code.read(size))[0]
        else:
            raise SystemError, "Unknown const type (%s)" % (repr(type), )
        tbl.append(data)
    #profile.exit("const table")
    return tbl

class FullyQualifId:
    def __init__(self, code):
        self.module = QualifId(code)
        self.item = QualifId(code)
    def lookup(self, strtbl):
        self.module = self.module.lookup(strtbl)
        self.item = self.item.lookup(strtbl)
    def __str__(self):
        if isinstance(self.module, str):
            return "<FullyQualifId %s.%s>" % (self.module, self.item)
        else:
            return "<FullyQualifId %s.%s>" % (self.module.stringIndexs, self.item.stringIndexs)
    def __repr__(self):
        return str(self)

class FakeFullyQualifId:
    def __init__(self, module, item):
        self.module = module
        self.item = item
    def __str__(self):
        return "<FullyQualifId %s.%s>" % (self.module, self.item)
    def __repr__(self):
        return str(self)

class QualifId:
    def __init__(self, code):
        length = struct.unpack(">B", code.read(1))[0]
        self.stringIndexs = struct.unpack(">%iH" % (length, ), code.read(length*2))
    def lookup(self, strtbl):
        if len(self.stringIndexs) != 1:
            return ""
        try:
            return strtbl[self.stringIndexs[0]]
        except IndexError:
            print strtbl
            print self.stringIndexs
            raise
    def __str__(self):
        return "<QualifId %s>" % (str(self.stringIndexs), )
    def __repr__(self):
        return str(self)

from bytecode_helpers import loadbc
from bc import bytecodes
from application import Application
import state
