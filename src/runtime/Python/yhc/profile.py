import time
import atexit

start = time.time()

calls = {}
stack = []

def display():
    print "Runtime %.3f seconds" % (time.time() - start)

    funcs = calls.keys()
    funcs.sort()
    for f in funcs:
        count, total = calls[f]
        if len(f) < 8:
            tabs = "\t\t"
        else:
            tabs = "\t"
        print "%s%s%i\t%.3f\t%.3f" % (f, tabs, count, total, total/count)

def enter(name):
    stack.append((name, time.time()))

def exit(name):
    assert stack[-1][0] == name
    name, start = stack.pop()
    try:
        count, total = calls[name]
    except KeyError:
        count, total = 0, 0.0
    count += 1
    total += (time.time() - start)
    calls[name] = count, total

atexit.register(display)