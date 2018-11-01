import os

Import("env")
Import("hsenv")

yhcfiles = []
for root, dir, files in list(os.walk(".")):
    for f in files:
        if f.endswith("NplusK.hs") or f.endswith("XML.hs") or f.endswith("Setup.hs") or (root+os.sep+f).endswith("Core" + os.sep + "Reduce.hs"):
            pass
        elif f.endswith("Main.hs"):
            yhcfiles.append(hsenv.HaskellObject(root + os.sep + f, HSFLAGS=["""-cpp -DyhcVERSION=\\"%s\\" """ % (env["ENV"]["VERSION"], )]))
        elif f.endswith(".hs") or f.endswith(".lhs"):
            yhcfiles.append(hsenv.HaskellObject(root + os.sep + f))

Return("yhcfiles")
