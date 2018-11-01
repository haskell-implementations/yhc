def main():
    build("Utilities.lhs", "-unlit")
    build("LambdaLift.lhs", "-unlit")
    build("Print.lhs", "-unlit")
    build("Test.hs")
    build("Main.lhs", "-unlit")
    run("Main")
