def main():
    build("PSlib.hs")
    build("StdLib.hs")
    build("Parse.hs")  
    build("GRIP.hs")
    build("Graph.hs")
    build("Pool.hs")
    build("Activity.hs")
    build("Spark.hs")
    build("Main.hs")
    run("Main")
 
