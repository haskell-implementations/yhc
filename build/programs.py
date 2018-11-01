import sys
import os
import re

def create_check(windowsname, othername, textname, envvar):
    def Check(context, env):
        context.Message("Checking for %s binary... " % (textname, ))
        bin = env.Detect(othername) or env.Detect(windowsname)
        if bin:
            env[envvar] = '"' + bin + '"'
            context.Result(env[envvar])
            return True
        else:
            context.Result("Not found")
            return False
    return Check

CheckSVN = create_check("svn.exe", "svn", "Subversion", "SVNBIN")
CheckGHC = create_check("ghc.exe", "ghc", "GHC", "GHCBIN")
CheckDarcs = create_check("darcs.exe", "darcs", "Darcs", "DARCSBIN")
CheckScons = create_check("scons.bat", "scons", "Scons", "SCONSBIN")
CheckHaddock = create_check("haddock.exe", "haddock", "Haddock", "HADDOCKBIN")

def CheckPythonVersion(context, env):
    context.Message("Checking for Python version 2.3 or later... ")

    major, minor, micro, releaselevel, serial = sys.version_info
    if major >= 2 and minor >= 3:
        context.Result("Found version %i.%i.%i %s (%i)." % (major, minor, micro, releaselevel, serial))
        return True
    else:
        context.Result("Too old. Found version %i.%i.%i %s (%i)." % (major, minor, micro, releaselevel, serial))
        return False

def CheckGHCVersion(context, env, hsenv):
    context.Message("Checking for GHC version 6.4.1 or later... ")

    fp = os.popen(env["GHCBIN"] + " --version")
    string = fp.read()
    fp.close()

    m = re.search("(\d+[.]\d+([.]\d)?)", string)
    if m is None:
        context.Result("Error running '%s'." % (env["GHCBIN"] + " --version"))
        return False
    if len(m.group(0).split(".")) == 2:
        major, minor = m.group(0).split(".")
        point = 0
    else:
        major, minor, point = m.group(0).split(".")
    major, minor, point = int(major), int(minor), int(point)
    if major < 6 or (major == 6 and minor < 4):
        context.Result("Too old. Found version %s." % (m.group(0), ))
        return False
    else:
        if major == 6 and (minor == 4 or minor == 6):
            hsenv.Append(LIBS=["mtl"])
        else:
            hsenv.Append(LIBS=["mtl","containers","pretty","packedstring"])
        context.Result("Found version %s." % (m.group(0), ))
        return True

def CheckSconsVersion(context, env):
    context.Message("Checking for Scons version... ")

    fp = os.popen(env["SCONSBIN"] + " --version")
    string = fp.read()
    fp.close()

    m = re.search("script: v(.+?[.].+?[.].+?[.].+?),", string)
    if m is None:
        context.Result("Error running '%s'." % (env["SCONSBIN"] + " --version"))
        return False
    else:
        context.Result("Found version %s." % (m.group(1), ))
        return True

def CheckDarcsVersion(context, env):
    context.Message("Checking for Darcs version... ")

    fp = os.popen(env["DARCSBIN"] + " --version")
    string = fp.read()
    fp.close()

    if string is None or len(string) == 0:
        context.Result("Error running '%s'." % (env["DARCSBIN"] + " --version"))
        return False

    string = string.strip()
    if "rc" in string:
        string = string.split("rc")[0]
    major, minor, point = [int(x) for x in string.split(" ")[0].split(".")]

    if major >= 1 and minor >= 0 and point >= 4:
        context.Result("Found version %s." % (string, ))
        return True
    else:
        context.Result("Too old. Found version %s." % (string, ))
        return False

def CheckSVNVersion(context, env):
    context.Message("Checking for Subversion version... ")

    fp = os.popen(env["SVNBIN"] + " --version")
    string = fp.read()
    fp.close()

    m = re.search("(\d+?[.]\d+?[.]\d+)", string)
    if m is None:
        context.Result("Error running '%s'." % (env["SVNBIN"] + " --version"))
        return False
    else:
        context.Result("Found version %s." % (m.group(1), ))
        return True

def CheckHaddockVersion(context, env):
    context.Message("Checking for Haddock version... ")

    fp = os.popen(env["HADDOCKBIN"] + " --version")
    string = fp.read()
    fp.close()

    m = re.search("(\d+?[.]\d+?([.]\d+)?)", string)
    if m is None:
        context.Result("Error running '%s'." % (env["HADDOCKBIN"] + " --version"))
        return False
    else:
        context.Result("Found version %s." % (m.group(1), ))
        return True
