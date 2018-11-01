#
# This file is simply to allow an instruction trace in the same format as that from the C interpretor.
#

def trace(code):
     try:
         traces[code.func_name](code)
     except KeyError:
         print "INS: %s" % (code.func_name, )

def trace_NEED_HEAP(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    if val == 32:
        print "INS: NEED_HEAP_32"
    else:
        print "INS: NEED_HEAP %i" % (val, )

def trace_ZAP_ARG(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    print "INS: ZAP_ARG %i" % (val, )

def trace_PUSH_CONST(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    if val < 8:
        print "INS: PUSH_CONST_%i" % (val, )
    else:
        print "INS: PUSH_CONST %i" % (val, )

def trace_PUSH_CHAR(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    print "INS: PUSH_CHAR %i" % (val, )

def trace_PUSH(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    if val < 4:
        print "INS: PUSH_%i" % (val, )
    else:
        print "INS: PUSH_ %i" % (val, )

def trace_PUSH_ARG(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    if val < 4:
        print "INS: PUSH_ARG_%i" % (val, )
    else:
        print "INS: PUSH_ARG %i" % (val, )

def trace_PUSH_ZAP_ARG(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    if val < 4:
        print "INS: PUSH_ZAP_ARG_%i" % (val, )
    else:
        print "INS: PUSH_ZAP_ARG %i" % (val, )

def trace_ZAP_STACK(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    print "INS: ZAP_STACK_P1 %i" % (val, )

def trace_PUSH_ZAP(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    if val < 4:
        print "INS: PUSH_ZAP_%i" % (val, )
    else:
        print "INS: PUSH_ZAP %i" % (val, )

def trace_MK_AP(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    if val < 5:
        print "INS: MK_AP_%i" % (val, )
    else:
        print "INS: MK_AP %i" % (val, )

def trace_MK_PAP(code):
    args = code.func_defaults[1][1:]
    (val, n) = args
    print "INS: MK_PAP_P1 %i %i" % (val,n )

def trace_POP(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    print "INS: POP_P1 %i" % (val, )

def trace_APPLY(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    if val < 2:
        print "INS: APPLY_%i" % (val, )
    else:
        print "INS: APPLY %i" % (val, )

def trace_MK_CON(code):
    args = code.func_defaults[1][1:]
    (val, ) = args
    if val < 2:
        print "INS: MK_CON_%i" % (val, )
    else:
        print "INS: MK_CON %i" % (val, )

def trace_TABLE_SWITCH(code):
    args = code.func_defaults[1][1:]
    (count, tbl) = args
    def doelement(x, y):
        return "%i -> %i" % (x, y[0]+1)
    print "INS: TABLE_SWITCH [%i] {%s}" % (count, ", ".join([doelement(x, y) for (x,y) in zip(tbl.keys(), tbl.values())]) )

traces = {
    "NEED_HEAP": trace_NEED_HEAP,
    "ZAP_ARG": trace_ZAP_ARG,
    "PUSH_CHAR": trace_PUSH_CHAR,
    "PUSH_CONST": trace_PUSH_CONST,
    "PUSH": trace_PUSH,
    "PUSH_ARG": trace_PUSH_ARG,
    "PUSH_ZAP_ARG": trace_PUSH_ZAP_ARG,
    "PUSH_ZAP": trace_PUSH_ZAP,
    "MK_AP": trace_MK_AP,
    "MK_PAP": trace_MK_PAP,
    "POP": trace_POP,
    "APPLY": trace_APPLY,
    "MK_CON": trace_MK_CON,
    "TABLE_SWITCH": trace_TABLE_SWITCH,
    "ZAP_STACK": trace_ZAP_STACK,
}