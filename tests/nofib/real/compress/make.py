def main():
    build("Defaults.hs")
    build("BinConv.hs")
    build("PTTrees.hs")
    build("Encode.hs")
    build("Main.hs")
    run("Main")
