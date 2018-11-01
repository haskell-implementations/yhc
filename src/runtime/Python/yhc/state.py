#import profile

import os

from parsebc import parseBC

from trace import trace

done_init = False
modules = {}
frame_stack = []

class StackOfStacks:
    def __init__(self):
        self.__stacks = []
        self.__stack = []
        self.fp = 0
    def push_frame(self):
        self.__stacks.append(self.__stack)
        self.__stack = []
    def pop_frame(self):
        self.__stack = self.__stacks.pop()
    def push(self, value):
        assert value is not None
        self.__stack.append(value)
        #print "push", value
    def pop(self):
        r =  self.__stack.pop()
        #print "pop", r
        return r
    def getStack(self):
        return self.__stack
    def getStacks(self):
        return self.__stacks
    def __getitem__(self, item):
        try:
            return self.__stack[len(self.__stack) - item - 1]
        except IndexError, m:
            raise IndexError, str(m) + " (%i of %i)" % (item, len(self.__stack))
    def __setitem__(self, item, val):
        try:
            self.__stack[len(self.__stack) - item - 1] = val
        except IndexError, m:
            raise IndexError, str(m) + " (%i of %i)" % (item, len(self.__stack))
    def __len__(self):
        return len(self.__stack)
    def __str__(self):
        return str(self.__stack)

stack = StackOfStacks()

def evalfile(filename):
    if not done_init:
        init()
    #profile.enter("evalfile")
    bc = parseBC(open(filename,"rb"))
    if not modules.has_key(bc.name):
        modules[bc.name] = bc
    else:
        modules[bc.name].update(bc)
    #profile.exit("evalfile")
    return bc

count = 0
def tramampoline(code):
    global frame_stack, stack, count
    while code:
        stack.fp = code.pos
        #print "--- %i %i" % (count, stack.fp)
        #count += 1
        #for s in stack.getStack()[::-1]:
        #    print s
        #trace(code)
        #for f, next, c in frame_stack[::-1]:
        #    print f
        #func = code.func_name
        #profile.enter(func)
        code = code()
        #profile.exit(func)

def push_frame(app, code):
    global stack
    assert len(frame_stack) == len(stack.getStacks())
    #print "push frame"
    frame_stack.append((app, code, stack.fp))
    stack.push_frame()

def pop_frame():
    global stack
    assert len(frame_stack) == len(stack.getStacks())
    #print "pop frame"
    app, code, stack.fp = frame_stack.pop()
    stack.pop_frame()
    return code

def get_app():
    return frame_stack[-1][0]

def saturate(x):
    if hasattr(x, "saturate"):
        x.saturate()
        return x
    elif isinstance(x, tuple):
        return lookup(x)

def lookup(const):
    #profile.enter("lookup")
    try:
        modules[const.module]
    except KeyError:
        evalfile(os.environ.get("YHC_BASE_PATH") + "/lib/yhc/packages/yhc-base/1.0/" + const.module + ".hbc")

    try:
        try:
            return modules[const.module].lookup(const.item)
        except KeyError:
            # we only get here if we've failed
            import sys
            sys.stderr.write("Unknown name '%s.%s'\n" % (const.module, const.item))
            sys.exit(-1)
    finally:
        #profile.exit("lookup")
        pass

import bytecodes

def init():
    global done_init
    if done_init:
        return
    done_init = True
    import glob
    base = os.environ.get("YHC_BASE_PATH")
    files = glob.glob("%s/lib/yhc/packages/yhc-base/1.0/YHC/*.hbc" % (base, )) + glob.glob("%s/lib/yhc/packages/yhc-base/1.0/System/*.hbc" % (base, )) + glob.glob("%s/lib/yhc/packages/yhc-base/1.0/Foreign/*.hbc" % (base, ))
    for f in files:
        evalfile(f)
    import builtin
    for obj in builtin.export:
        modules[""].objtbl[obj] = getattr(builtin, obj)
