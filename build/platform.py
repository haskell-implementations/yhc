import sys
import os

def platform(env, hsenv, args):
    if sys.platform == "win32":
        cctype = args.get('cc', "vs")
    else:
        cctype = args.get('cc', "gcc")
    env["ENV"]["COMPILER"] = cctype
    if cctype == "gcc":
        pass
    elif cctype == "vs":
        pass
    elif cctype == "ghc":
        env["CC"] = "ghc -Bbuild/"
        env["CCFLAGS"] = []
        env["INCPREFIX"] = "-I"
        env["CCCOMFLAGS"] = "$CPPFLAGS $_CPPDEFFLAGS $_CPPINCFLAGS -c $SOURCES -o $TARGET $CCPCHFLAGS $CCPDBFLAGS"
    else:
        print "Unrecognized compiler type '%s'." % (cctype, )

    if sys.platform == "win32":
       return win32(env, hsenv, args)
    else:
       return other(env, hsenv, args)

def win32(env, hsenv, args):
    if env["ENV"]["COMPILER"] == "vs":
        env.Append(LINKFLAGS="/NODEFAULTLIB:LIBC")
    env.Append(CPPPATH=["#src\\runtime\\BCKernel\\msvc\\gmp", "#depends\\ctypes\\libffi_msvc", "#src\\runtime\\BCKernel\\win32"])
    env.Append(LIBPATH=["#src\\runtime\\BCKernel\\msvc\\gmp"])
    if env["ENV"]["COMPILER"] == "ghc":
        env.Append(CCFLAGS=["-optc -DPy_FatalError=printf"])

        env.Append(CCFLAGS=["""-optc -DVERSION=\\"%s\\" """ % (env["ENV"]["VERSION"], )])
    else:
        env.Append(CCFLAGS=["/DPy_FatalError=printf"])

        env.Append(CCFLAGS="""/DVERSION=\\"%s\\" """ % (env["ENV"]["VERSION"], ))

    build_stack = int(args.get('yhistack', "0"))
    if build_stack == 1:
        env.Append(CCFLAGS=["/DSTACK_TRACE"])

    reltype = args.get('type', "normal")
    if reltype == "debug":
        print "Debug Build Enabled."
    elif reltype == "release":
        print "Release Build Enabled."
        env.Append(CCFLAGS="/O2")
        hsenv.Append(HSFLAGS="-O")
    elif reltype == "normal":
        pass
    else:
        print "Unrecognized build type '%s'." % (reltype, )

    return []

def other(env, hsenv, args):
    env.Append(CCFLAGS="-Wall")
    reltype = args.get('type', "normal")
    if reltype == "debug":
        print "Debug Build Enabled."
        env.Append(CCFLAGS="-g")
    elif reltype == "release":
        print "Release Build Enabled."
        env.Append(CCFLAGS="-O2")
        hsenv.Append(HSFLAGS="-O")
    elif reltype == "normal":
        env.Append(CCFLAGS="-O")
    else:
        print "Unrecognized build type '%s'." % (reltype, )

    yhilibs = []

    if env["ENV"]["COMPILER"] == "ghc":
        env.Append(CCFLAGS="""-optc -DVERSION=\\"%s\\" """ % (env["ENV"]["VERSION"], ))
    else:
        env.Append(CCFLAGS="""-DVERSION=\\"%s\\" """ % (env["ENV"]["VERSION"], ))

    build_stack = int(args.get('yhistack', "0"))
    if build_stack == 1:
        env.Append(CCFLAGS=["-DSTACK_TRACE"])

    platform = env["ENV"]["ARCH"]
    if platform == "x86":
        yhilibs.extend(["m", "pthread"])
        env.Append(CPPPATH=["#depends/ctypes/libffi/include", "#depends/ctypes/libffi/src/x86"])
    elif platform == "x86_64":
        yhilibs.extend(["m", "pthread"])
        env.Append(CPPPATH=["#depends/ctypes/libffi/include", "#depends/ctypes/libffi/src/x86"])
    elif platform == "ppc":
        if sys.platform != "darwin":
            yhilibs.extend(["m", "pthread"])
        env.Append(CPPPATH=["#depends/ctypes/libffi/include", "#depends/ctypes/libffi/src/powerpc"])

    if sys.platform not in ["freebsd6"]:
        yhilibs.append("dl")

    return yhilibs
