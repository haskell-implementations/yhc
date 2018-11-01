def main():
    build("Encode.hs")
    build("WriteRoutines.hs", "-cpp")
    build("Main.hs")
