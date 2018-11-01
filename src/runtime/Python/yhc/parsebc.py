#import profile

from module import Module

import struct
import StringIO

def loadHeader(code):
    assert code.read(4) == "HSBC"

    return struct.unpack(">HHHH", code.read(8))

def loadStringTable(code):
    count = struct.unpack(">H", code.read(2))[0]

    table = []
    for i in range(count):
        length = struct.unpack(">H", code.read(2))[0]
        table.append(struct.unpack("%is" % (length, ), code.read(length))[0])

    return table

def loadObjectTable(code, header, strtbl, moduleName):
    table = {}
    for i in range(header[3]):
        id = QualifId(code)
        name = id.lookup(strtbl)
        length = struct.unpack(">H", code.read(2))[0]
        type = code.read(1)
        if length > 1:
            #profile.enter("create1")
            data = objtypes[type](code)
            #rofile.exit("create1")
            #profile.enter("create2")
            data.id = FakeFullyQualifId(moduleName, name)
            #profile.exit("create2")
            data.name = name
            #profile.enter("create3")
            data.lookup(strtbl)
            #profile.exit("create3")
            data.object_table = table
        else:
            data = None
        table[name] = data
    return table

def parseBC(fp_code):
    #profile.enter("load code")
    code = StringIO(fp_code.read())
    #profile.exit("load code")

    #profile.enter("load header")
    header = loadHeader(code)
    assert header[2] == 0
    #profile.exit("load header")

    #profile.enter("load string table")
    strtbl = loadStringTable(code)
    #profile.exit("load string table")

    moduleName = QualifId(code).lookup(strtbl)

    #profile.enter("load object table")
    objtbl = loadObjectTable(code, header, strtbl, moduleName)
    #profile.exit("load object table")

    return Module(header, strtbl, moduleName, objtbl)

from bytecodes import *
from objs import *
