import sys
import os
import popen2

def CheckArchitecture(context, env, args):
    context.Message("Checking for architecture... ")

    if sys.platform == "win32":
        env["ENV"]["ARCH"] = "x86"
        context.Result("Windows implies x86")
        return True

    if not args.get("arch") and sys.platform != "win32":
        fp = os.popen("uname -m")
        string = fp.read().strip()
        fp.close()
    elif sys.platform == "win32":
        env["ENV"]["ARCH"] = "x86"
        context.Result("Windows implies x86")
        return True
    else:
        env["ENV"]["ARCH"] = args.get("arch")
        context.Result("%s (forced)" % (args.get("arch")), )
        return True

    if string in ["i686", "i586", "i486", "i386"]:
        env.Append(CCFLAGS="-DX86")
        env["ENV"]["ARCH"] = "x86"
        context.Result("x86")
        return True
    elif string in ["ppc", "Power Macintosh"]:
        if sys.platform == "darwin":
            env.Append(CCFLAGS="-DPOWERPC_DARWIN")
        else:
            env.Append(CCFLAGS="-DPOWERPC")
        env["ENV"]["ARCH"] = "ppc"
        context.Result("ppc")
        return True
    elif string == "x86_64":
        env.Append(CCFLAGS="-DX86_64")
        env["ENV"]["ARCH"] = "x86_64"
        context.Result("x86_64")
        return True
    else:
        context.Result("Unknown (%s)" % (string, ))
        return False

def CheckOS(context, env):
    context.Message("Checking for operating system... ")

    context.Result(sys.platform)
    return True
