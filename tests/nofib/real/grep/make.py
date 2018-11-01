def main():
    build("Parsers.hs")
    build("Main.lhs", "-unlit")
    run("Main")
    
