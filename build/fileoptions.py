import sys

def fileOptions():
     try:
         fp = open("options.txt", "r")
     except IOError:
         return {}

     d = {}
     for line in fp.readlines():
         line = line.strip()
         if len(line) == 0 or line[0] == "#":
            continue
         elif line.find("=") == -1:
             sys.stderr.write("Line '%s' does not have an equals sign." % (line, ))
         else:
             sign = line.find("=")
             d[line[:sign].strip()] = line[sign+1:].strip()
     return d
