import struct

struct_mapping = {"u8":"B", "s8": "b", "u16": ">H", "s16": ">h"}

def loadbc(data):
    code = []
    while True:
        pos = data.tell()
        bccode = data.read(1)
        if len(bccode) == 0:
            break
        assert data.tell() - pos == 1, data.tell() - pos
        bc = bytecodes[struct.unpack(">B", bccode)[0]]
        if len(bc[0]) == 0:
            code.append((bc[1], (), pos))
        else:
            params = []
            for p in bc[0]:
                params.append(readparam(p, data, params))
            code.append((bc[1], tuple(params), pos))
    return buildbc(code)

def readparam(param, data, prev):
    if param[0] is not None and param[1] is not None:
        # table
        size = prev[int(param[0])-1]
        tbl = []
        for i in range(size):
            tbl.append([readparam(p, data, None) for p in param[1]])
        return dict(zip(range(size), tbl))
    elif param[0] is None:
        return int(param[1])
    elif param[1] is None:
        code = struct_mapping[param[0]]
        return struct.unpack(code, data.read(struct.calcsize(code)))[0]
    else:
        raise SystemError

def buildbc(code):
    nc = []
    next = None
    for inst, args, pos in code[::-1]:
        fargs = (next, ) + args
        nc.append(lambda i=inst, n=fargs: i(*n))
        nc[-1].func_name = inst.func_name
        nc[-1].pos = pos
        next = nc[-1]
    nc.reverse()
    return nc

from bc import bytecodes
import bytecodes as bcmod
