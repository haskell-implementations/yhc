def main():
    build("Parse.hs")
    build("PrologData.hs")
    build("Interact.hs")
    build("Subst.hs")
    build("Engine.hs")
    build("Version.hs")
    build("Main.hs")
    run("Main")

