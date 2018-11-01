import sys

from SCons import *

from config_h import config_h_build
from typesize import CheckCharSize, CheckShortSize, CheckIntSize, CheckLongSize, CheckLongLongSize, CheckFloatSize, CheckDoubleSize, CheckVoidPtrSize
from endian import CheckEndianness
from programs import CheckSVN, CheckGHC, CheckDarcs, CheckScons, CheckGHCVersion, CheckSconsVersion, CheckDarcsVersion, CheckSVNVersion, CheckPythonVersion, CheckHaddock, CheckHaddockVersion
from arch import CheckArchitecture, CheckOS
from libgmp import CheckGMPVersion
from clean import empty
from version import getVersion

from traceback import *

checks = {"CheckCharSize": CheckCharSize,
          "CheckShortSize": CheckShortSize,
          "CheckIntSize": CheckIntSize,
          "CheckLongSize": CheckLongSize,
          "CheckLongLongSize": CheckLongLongSize,
          "CheckFloatSize": CheckFloatSize,
          "CheckDoubleSize": CheckDoubleSize,
          "CheckVoidPtrSize": CheckVoidPtrSize,
          "CheckEndianness": CheckEndianness,
          "CheckSVN": CheckSVN,
          "CheckSVNVersion": CheckSVNVersion,
          "CheckGHC": CheckGHC,
          "CheckGHCVersion": CheckGHCVersion,
          "CheckDarcs": CheckDarcs,
          "CheckDarcsVersion": CheckDarcsVersion,
          "CheckScons": CheckScons,
          "CheckSconsVersion": CheckSconsVersion,
          "CheckHaddock": CheckHaddock,
          "CheckHaddockVersion": CheckHaddockVersion,
          "CheckArchitecture": CheckArchitecture,
          "CheckOS": CheckOS,
          "CheckGMPVersion": CheckGMPVersion,
          "CheckPythonVersion": CheckPythonVersion
}

def setup_version(env, Configure, args):
    if int(args.get('release', 0)):
        vtype = 0
    elif int(args.get('snapshot', 0)):
        vtype = 1
    else:
        vtype = 2

    env["ENV"]["VERSION"] = getVersion(vtype)
    print "Building version %s." % (env["ENV"]["VERSION"], )

    conf = Configure(env, checks)

    if not conf.CheckArchitecture(env, args):
        print "Error. Your architecture is unsupported. Please contact the Yhc developers."
        sys.exit(1)
    conf.CheckOS(env)

    conf.Finish()

def always_configure(env, hsenv, Configure, args):
    conf = Configure(env, checks)

    if not args.get("skipctypes", 0):
        if not conf.CheckSVN(env):
            print "Error. 'svn' not found in PATH. Will use ctypes snapshot."
        else:
            conf.CheckSVNVersion(env)
    ghc = args.get("ghc", None)
    if ghc:
        print "Using %s as GHC binary... " % (ghc, )
        env["GHCBIN"] = '"' + ghc + '"'
    elif not conf.CheckGHC(env):
        print "Error. 'ghc' not found in PATH. Please install GHC before compiling Yhc."
        sys.exit(1)
    hsenv["HS"] = env["GHCBIN"]
    if not conf.CheckGHCVersion(env, hsenv):
        print "Error. GHC version 6.4.1 or later not found in PATH. Please install a later version of GHC before compiling Yhc."
        sys.exit(1)

    conf.Finish()


def gmp_configure(env, Configure, args):
    conf = Configure(env, checks)

    # block moved from always_configure
    # /me stabs in the dark
    if sys.platform != "win32":
        if args.get('cc', "gcc") == "ghc":
            ldflag = "-optl "
        else:
            ldflag = ""
        if not conf.CheckHeader("gmp.h"):
            print "Error. libgmp must be installed and gmp.h made accessible before you can compile Yhc."
            sys.exit(1)
        if not conf.CheckLibWithHeader("gmp", "gmp.h", "C", "mpz_t integ; mpz_init (integ);", 0):
            if not conf.CheckLibWithHeader("GMP", "gmp.h", "C", "mpz_t integ; mpz_init (integ);", 0):
                print "Error. libgmp must be installed made accessible before you can compile Yhc."
                sys.exit(1)
            else:
                env["libgmp"] = ldflag + "-Xlinker GMP"
        else:
            env["libgmp"] = ldflag + "-Xlinker -lgmp"
        conf.CheckGMPVersion(env)
    else:
        env["libgmp"] = "gmp.lib"

    conf.Finish()

def configure(env, Configure, args):

    try:
        conf = Configure(env, checks)
    except:
        print "exception in Configure: ",  print_tb(sys.exc_info()[2])
	conf = None

    if not conf.CheckPythonVersion(env):
        print "Error. Python 2.3 or later must be used to compile Yhc"
        sys.exit(1)
    if conf.CheckScons(env) and sys.platform != "win32":
        conf.CheckSconsVersion(env)
    if not conf.CheckDarcs(env):
        print "Error. 'darcs' not found in PATH. Please install Darcs before compiling Yhc."
        sys.exit(1)
    conf.CheckDarcsVersion(env)
    if not conf.CheckHaddock(env):
        print "Error. Haddock not found, you won't be able to build the documentation."
    else:
        conf.CheckHaddockVersion(env)

    vals = {}

    vals["WORDS_BIGENDIAN"] = conf.CheckEndianness()
    vals["SIZEOF_CHAR"] = conf.CheckCharSize()
    vals["SIZEOF_SHORT"] = conf.CheckShortSize()
    vals["SIZEOF_INT"] = conf.CheckIntSize()
    vals["SIZEOF_LONG"] = conf.CheckLongSize()
    vals["SIZEOF_LONG_LONG"] = conf.CheckLongLongSize()
    vals["SIZEOF_FLOAT"] = conf.CheckFloatSize()
    vals["SIZEOF_DOUBLE"] = conf.CheckDoubleSize()
    vals["SIZEOF_VOIDP"] = conf.CheckVoidPtrSize()

    if len([x for x in vals.values() if 0 is x]):
        print "Failed to check types sizes/endianness. You do have a working C compiler don't you?"
        print "If so then please contact the Yhc developers for help."
        conf.Finish()
        empty(".sconf_temp")
        sys.exit(1)

    if sys.platform == "win32":
        vals["HAVE_GCC_LABELS"] = 0
        vals["WIN32"] = 1
    else:
        vals["HAVE_GCC_LABELS"] = 1
        vals["WIN32"] = 0
    threads = int(args.get('threads', 1))
    if not threads:
        print "Threading disabled."
        vals["HAVE_LIBPTHREAD"] = 0
    else:
        vals["HAVE_LIBPTHREAD"] = conf.CheckLib("pthread", "pthread_create", "pthread.h")

    config_h_build('config.h', 'config.h.new.in', vals)

    conf.Finish()
