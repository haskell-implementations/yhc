class Module:
    def __init__(self, header, strtbl, name, objtbl):
        self.header = header
        self.strtbl = strtbl
        self.name = name
        self.objtbl = objtbl

    def update(self, mod):
        self.strtbl += mod.strtbl
        self.objtbl.update(mod.objtbl)

    def findFunction(self, name):
        return self.objtbl[name]

    def lookup(self, name):
        return self.objtbl[name]
