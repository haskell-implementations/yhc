# -*- coding: utf-8 -*-
# Copyright (C) 2005 JosÃ© Pablo Ezequiel "Pupeno" FernÃ¡ndez Silva
#
# This file is part of scons-haskell.
#
# Scons-haskell is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# Scons-haskell is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with scons-haskell; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

import pickle
import os

import SCons.Tool
import SCons.Action
from SCons.Scanner import Scanner
from SCons.Defaults import ObjSourceScan
import string

depsversion = 2

class HaskellDepsCache:
    def __init__(self):
        if os.path.exists(".sconf_temp/haskelldeps"):
            try:
                self.deps = pickle.loads(open(".sconf_temp/haskelldeps").read())
                if not isinstance(self.deps, tuple) or self.deps[0] != depsversion:
                    self.deps = {}
                else:
                    self.deps = self.deps[1]
            except EOFError:
                self.deps = {}
        else:
            self.deps = {}

    def write(self):
        try:
            fp = open(".sconf_temp/haskelldeps", "w")
        except IOError:
            pass
        else:
            fp.write(pickle.dumps((depsversion, self.deps)))
            fp.close()

    def __getitem__(self, key):
        try:
            return self.deps[key]
        except KeyError:
            return None
    def __setitem__(self, key, value):
        self.deps[key] = value

depscache = HaskellDepsCache()

def generate(env):
    env["HSLINK"] = "$HS $_LIBS $SOURCES -o $TARGET"
    env["HSCOM"] = "$HS $_IMPORTS $_LIBS $HSFLAGS -c $SOURCE -o $TARGET"
    env["_IMPORTS"] = "${_concat(IMPORTSPREFIX, LIBPATH, IMPORTSSUFFIX, __env__)}"
    env["IMPORTSPREFIX"] = "-i"
    env["IMPORTSSUFFIX"] = ""
    env["_LIBS"] = "${_concat(LIBSPREFIX, LIBS, LIBSSUFFIX, __env__)}"
    env["LIBSPREFIX"] = "-package "
    env["LIBSSUFFIX"] = ""

    haskellSuffixes = [".hs", ".lhs"]

    compileAction = SCons.Action.Action("$HSCOM")

    linkAction = SCons.Action.Action("$HSLINK")

    def addHaskellInterface(target, source, env):
        """ Add the .hi target with the same name as the object file. """
        targetName = os.path.splitext(str(target[0]))[0]
        env.SideEffect(targetName + ".hi", target)
        return (target, source)

    def importedModules(node, env, path):
        """ Use ghc to find all the imported modules. """
        root, ext = os.path.splitext(str(node))
        if ext == ".o" or ext == ".obj" or ext == ".hi":
            if os.path.exists(root + ".hs"):
                return ["#"+root + ".hs"]
            elif os.path.exists(root + ".lhs"):
                return ["#"+root + ".lhs"]
            else:
                raise SystemError, "Unable to find source file for %s" % (str(node), )
        d = depscache[str(node)]
        if d is not None and d[0] == os.path.getmtime(str(node)):
            return d[1]
        print "Getting dependencies for %s." % (str(node), )

        #print "Figuring out dependencies for " + str(node)
        def removeFile(fileName, errmsg = None):
            """ Try to remove fileName, returns true on success, false otherwise. """
            if os.path.exists(fileName):
                try:
                    os.remove(fileName)
                except OSError:
                    print "Unable to remove '%s'." % fileName
                    return False
            return True

        # Generate the name of the file that is going to contain the dependency mappings.
        fileName = os.path.join(os.path.dirname(str(node)),
                                "." + os.path.basename(str(node)) + ".dep")

        # Just in case the file already exist, to avoid the creation of a .bak file, delete it.
        if not removeFile(fileName):
            print "Dependencies will not be calculated."
            return []

        # Build the command to obtain the dependency mapping from ghc.
        command = ["ghc", "-M", "-optdep-f", "-optdep" + fileName]
        if env._dict.has_key("HSFLAGS") and isinstance(env._dict["HSFLAGS"], list):
            command += [" ".join(env["HSFLAGS"])]
        elif env._dict.has_key("HSFLAGS") and isinstance(env._dict["HSFLAGS"], str):
            command += [env["HSFLAGS"]]
        if env._dict.has_key("LIBPATH"):
            command += ["-i" + string.join(env["LIBPATH"], ":")]
        # if env._dict.has_key("LIBS"):
        #    command += [env["LIBSPREFIX"] + string.join(env["LIBS"], env["LIBSPREFIX"])]
        command += [str(node)]
        command = " ".join(command)

        commandIn, commandOut = os.popen4(command, "t")
        errorMessage = commandOut.read()
        commandIn.close()
        commandOut.read()
        if(errorMessage != ""):
                print "An error ocurred running `%s`:" % command
                for line in string.split(errorMessage, "\n"):
                    print ">" + line
                print "Dependencies will not be calculated."
                removeFile(fileName)
                return []

        try:
            file = open(fileName, "r")
            fileContents = file.read()
            file.close()
        except:
            print "Unable to open '%s'." % fileName
            print "Dependencies will not be calculated."
            removeFile(fileName)
            return []

        fileContents = string.split(fileContents, "\n")

        deps = []
        for line in fileContents:
            #print "deps=%s." % str(deps)
            if len(line) > 0 and line[0] != "#":
                files = string.split(line, ":")
                target = string.strip(files[0])
                source = string.strip(files[1])
                if source != str(node) and os.path.splitext(source)[1] == ".hi":
                    deps.append("#" + str(source))

        depscache[str(node)] = os.path.getmtime(str(node)), deps
        depscache.write()

        removeFile(fileName)
        return deps

    haskellScanner = Scanner(function = importedModules,
                             name = "HaskellScanner",
                             skeys = haskellSuffixes,
                             recursive = False)

    haskellProgram = SCons.Builder.Builder(action = linkAction,
                                           prefix = "$PROGPREFIX",
                                           suffix = "$PROGSUFFIX",
                                           src_suffix = "$OBJSUFFIX",
                                           src_builder = "HaskellObject")
    env["BUILDERS"]["HaskellProgram"] = haskellProgram

    haskellLibrary = SCons.Builder.Builder(action = SCons.Action.Action("$ARCOM"),
                                           prefix = "$LIBPREFIX",
                                           suffix = "$LIBSUFFIX",
                                           src_suffix = "$OBJSUFFIX",
                                           src_builder = "HaskellObject")
    env["BUILDERS"]["HaskellLibrary"] = haskellLibrary

    haskellObject = SCons.Builder.Builder(action = compileAction,
                                          emitter = addHaskellInterface,
                                          prefix = "$OBJPREFIX",
                                          suffix = "$OBJSUFFIX",
                                          src_suffix = haskellSuffixes,
                                          source_scanner = haskellScanner)
    env["BUILDERS"]["HaskellObject"] = haskellObject

def exists(env):
    return env.Detect(["ghc"])
