def getVersion(type):
    fp = open("version", "r")
    version = fp.readline().strip()
    fp.close()

    if type != 0:
        count = 0
        lines = filter(lambda x: x.find("**") != -1, open("_darcs/inventory", "r").readlines())
        dt = long(lines[-1].split("**")[1].strip()[:-1])
        version = version + "-%i" % (dt, )
        if type != 1:
            version = version + "-custom"

    return version
