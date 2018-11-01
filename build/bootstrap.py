import os
import sys

def bootstrap(env):
    print "Bootstrapping..."
    mkdir("inst/lib/yhc/packages/yhc-base/1.0/")
    mkdir("inst/lib/yhc/packages/yhc-base/1.0/YHC/")
    mkdir("inst/lib/yhc/packages/yhc-base/1.0/Data/")

    copy(env, "depends/SConscript", "build/SConscript.depends")
    copy(env, "depends/cpphs/SConscript", "build/SConscript.cpphs")
    copy(env, "depends/filepath/SConscript", "build/SConscript.filepath")
    copy(env, "depends/uniplate/SConscript", "build/SConscript.uniplate")
    copy(env, "depends/ctypes/SConscript", "build/SConscript.ctypes")
    copy(env, "depends/ctypes/libffi/SConscript", "build/SConscript.libffi")
    copy(env, "depends/ctypes/libffi/fficonfig.h", "build/ffih/fficonfig.h")
    copy(env, "depends/ctypes/libffi/include/ffi.h", "build/ffih/ffi.h")
    copy(env, "depends/ctypes/libffi/src/cfield.c", "build/ffih/cfield.c")

    copy(env, "src/packages/yhc-base-1.0/Prelude.hi", "bootstrap/packages/yhc-base/1.0/Prelude.hi")
    copy(env, "src/packages/yhc-base-1.0/PreludeBuiltin.hi", "bootstrap/packages/yhc-base/1.0/PreludeBuiltin.hi")
    copy(env, "src/packages/yhc-base-1.0/YHC/Internal.hi", "bootstrap/packages/yhc-base/1.0/YHC/Internal.hi")
    copy(env, "src/packages/yhc-base-1.0/Data/Ratio.hi", "bootstrap/packages/yhc-base/1.0/Data/Ratio.hi")
    print "Done."

def mkdir(dirname, umask=None):
    if not os.path.exists(dirname):
        os.makedirs(dirname)

def copy(env, to, file, umask = None):
    if not os.path.exists(to) or os.path.getmtime(to) < os.path.getmtime(file):
        open(to, "wb").write(open(file, "rb").read())
        print "%s -> %s" % (file, to)
    if umask:
        os.chmod(to, umask)

def copydir(env, to, file, umask = None):
    for root, dirs, files in os.walk(file):
        if not os.path.exists(to + root[len(file):]):
            mkdir(to + root[len(file):], umask)
        for f in files:
            copy(env, to + root[len(file):] + os.sep + f, root + os.sep + f, umask)
