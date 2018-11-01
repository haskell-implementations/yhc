import sys
import os
import tarfile
import urllib2

def update(env):
    env.Execute(env.Action("darcs pull"))
    if not os.path.exists("src/compiler"):
        env.Execute(env.Action("darcs get --partial --repo-name=src/compiler http://darcs.haskell.org/york-compiler98"))
    else:
        env.Execute(env.Action("darcs pull --repodir=src/compiler --all"))

def depends(env, args, only_checkout=False):
    if not os.path.exists("depends/"):
        os.mkdir("depends")

    if not os.path.exists("depends/cpphs"):
        env.Execute(env.Action("darcs get --partial --repo-name=depends/cpphs http://www.cs.york.ac.uk/fp/yhc/dependencies/cpphs"))
    elif not only_checkout:
        env.Execute(env.Action("darcs pull --repodir=depends/cpphs --all http://www.cs.york.ac.uk/fp/yhc/dependencies/cpphs"))

    if not os.path.exists("depends/filepath"):
        env.Execute(env.Action("darcs get --partial --repo-name=depends/filepath http://www.cs.york.ac.uk/fp/yhc/dependencies/filepath"))
    elif not only_checkout:
        env.Execute(env.Action("darcs pull --repodir=depends/filepath --all http://www.cs.york.ac.uk/fp/yhc/dependencies/filepath"))

    if not os.path.exists("depends/uniplate"):
        env.Execute(env.Action("darcs get --partial --repo-name=depends/uniplate http://www.cs.york.ac.uk/fp/yhc/dependencies/uniplate"))
    elif not only_checkout:
        env.Execute(env.Action("darcs pull --repodir=depends/uniplate --all http://www.cs.york.ac.uk/fp/yhc/dependencies/uniplate"))

    if not os.path.exists("src/compiler"):
        env.Execute(env.Action("darcs get --partial --repo-name=src/compiler http://darcs.haskell.org/york-compiler98"))
    elif not only_checkout:
        env.Execute(env.Action("darcs pull --repodir=src/compiler --all"))


    if args.get("skipctypes", 0):
        return

    try:
        svn = env["SVNBIN"]
    except KeyError:
        svn = False

    if args.get("ctypes", "svn") == "svn" and svn:
        if not os.path.exists("depends/ctypes"):
            env.Execute(env.Action("svn co http://svn.python.org/projects/python/trunk/Modules/_ctypes/ depends/ctypes"))
        elif not only_checkout:
            env.Execute(env.Action("cd depends/ctypes && svn update"))
    else:
        if not os.path.exists("depends/ctypes-snapshot-20070206.tar.gz"):
            print "Downloading Ctypes snapshot..."
            url = urllib2.urlopen("http://www.cs.york.ac.uk/fp/yhc/dependencies/ctypes-snapshot-20070206.tar.gz")
            fp = open("depends/ctypes-snapshot-20070206.tar.gz", "wb")
            fp.write(url.read())
            fp.close()
            print "Downloaded."
        #tarfile.open("depends/ctypes-snapshot-20070206.tar.gz", "r:gz").extractall("depends")
        extractall(tarfile.open("depends/ctypes-snapshot-20070206.tar.gz", "r:gz"), "depends")
        try:
            os.unlink("depends/ctypes-snapshot-20070206.tar.gz")
        except OSError:
            pass

def extractall(self, path=".", members=None):
    """Extract all members from the archive to the current working
        directory and set owner, modification time and permissions on
        directories afterwards. `path' specifies a different directory
        to extract to. `members' is optional and must be a subset of the
        list returned by getmembers().
    """
    directories = []

    if members is None:
        members = self

    for tarinfo in members:
        if tarinfo.isdir():
            # Extract directory with a safe mode, so that
            # all files below can be extracted as well.
            try:
                os.makedirs(os.path.join(path, tarinfo.name), 0777)
            except EnvironmentError:
                pass
            directories.append(tarinfo)
        else:
            self.extract(tarinfo, path)

    # Reverse sort directories.
    directories.sort(lambda a, b: cmp(a.name, b.name))
    directories.reverse()

    # Set correct owner, mtime and filemode on directories.
    for tarinfo in directories:
        path = os.path.join(path, tarinfo.name)
        try:
            self.chown(tarinfo, path)
            self.utime(tarinfo, path)
            self.chmod(tarinfo, path)
        except tarfile.ExtractError, e:
            if self.errorlevel > 1:
                raise
            else:
                self._dbg(1, "tarfile: %s" % e)
