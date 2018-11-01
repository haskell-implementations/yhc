from application import Value

def stdoutC(arg):
    #print "stdoutC", arg
    import sys
    return Value(sys.stdout)

def primCreateForeignPtr(arg):
    #print "primCreateForeignPtr", arg
    return arg.value

def primAttachForeignPtr(arg1, arg2):
    #print "primAttachForeignPtr", arg1, arg2
    return Value(0)

def primExitWith(arg):
    #print "primExitWith", arg
    import sys
    sys.exit(0)

def hPutCharC(fp, char):
    #print "hPutCharC", fp, char
    if isinstance(char.value, list):
        char, r = char.value
    else:
        char, r = char.value, Value(0)
    #print char
    fp.value.value.value.write(char)
    return r

def hGetErrorC(arg):
    #print "hGetErrorC", arg
    return Value(0)

def hCloseC(arg):
    #print "hCloseC", arg
    return Value(0)

