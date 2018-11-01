def END_CODE(next):
    raise SystemError

def NEED_HEAP(next, size):
    return next

def PUSH(next, n):
    stack.push(stack[n])
    return next

def PUSH_ZAP(next, n):
    PUSH(None, n)
    ZAP_STACK(None, n+1)
    return next

def ZAP_STACK(next, val):
    stack[val] = Application(lookup(FakeFullyQualifId("Prelude", "_zap_stack")), [])
    return next

def ZAP_ARG(next, val):
    get_app().args[val] = Application(lookup(FakeFullyQualifId("Prelude", "_zap_arg")), [])
    return next

def PUSH_ARG(next, val):
    #print "Pushing arg", val, get_app().args
    stack.push(get_app().args[val])
    return next

def PUSH_ZAP_ARG(next, val):
    PUSH_ARG(None, val)
    ZAP_ARG(None, val)
    return next

def AP_ARG(next):
    raise NotImplementedError

def PUSH_INT(next, val):
    stack.push(Value(int(val)))
    return next

def PUSH_CHAR(next, val):
    stack.push(Value(chr(val)))
    return next

def PUSH_CONST(next, val):
    const = get_app().func.consts[val]
    if hasattr(const, "module"):
        const = lookup(const)
        stack.push(Application(const, []))
    else:
        stack.push(Value(const))
    return next

def MK_AP(next, val):
    func = lookup(get_app().func.consts[val])
    if isinstance(func, tuple):
        func = func[1]
    if len(stack) < func.arity:
        raise SystemError, "MK_AP tried to pop %i arguments from a stack with %i elements for function %s. Stack contents = %s" % (func.arity, len(stack), str(func), str(stack))
    args = [stack.pop() for x in range(func.arity)]

    stack.push(Application(func, args))

    return next

def MK_PAP(next, val, arity):
    #print lookup(get_app().func.consts[val])
    args = [stack.pop() for x in range(arity)]
    stack.push(Application(lookup(get_app().func.consts[val]), args))
    return next

def APPLY(next, n):
    app = stack.pop()
    args = [stack.pop() for x in range(n)]

    #print app
    #print args
    #print app.arity, n
    if isinstance(app, Application) and app.args is not None and len(app.args + args) >= app.arity:
        #print "apply type 1"
        missing = app.arity-len(app.args)
        app.args.extend(args[:missing])
        apply = lookup(FakeFullyQualifId("", "_apply"))
        #print args, missing, args[:missing], args[missing:]
        for arg in args[missing:]:
            app = Application(apply, [app, arg])
    elif isinstance(app, Application):
        #print "apply type 2"
        app.args.extend(args)
    elif not isinstance(app, Application) and app.arity < n:
        #print "apply type 3"
        app = Application(app, args[:app.arity])
        apply = lookup(FakeFullyQualifId("", "_apply"))
        for arg in args[app.arity:]:
            app = Application(apply, [app, arg])
    else:
        #print "apply type 4"
        app = Application(app, args)
    stack.push(app)
    return next

def MK_CON(next, val):
    func = lookup(get_app().func.consts[val])
    args = [stack.pop() for x in range(func.arity)]
    app = Application(func, args)
    stack.push(app)
    return next

def UNPACK(next):
    app = stack.pop()
    if not hasattr(app, "args") or not app.args:
        stack.push(app)
        return next
    for arg in app.args[::-1]:
        stack.push(arg)
    return next

def SLIDE(next):
        raise NotImplementedError

def POP(next, n):
    [stack.pop() for x in range(n)]
    return next

def ALLOC(next):
        raise NotImplementedError

def UPDATE(next):
        raise NotImplementedError

def SELECT(next):
        raise NotImplementedError

def RETURN(next):
    val = stack.pop()
    app = get_app()
    code = pop_frame()
    stack.push(val)
    return app.setValue(code, val)

def EVAL(next):
    app = stack.pop()
    if len(app.args) < app.arity or isinstance(app, Value) or isinstance(app.func, ConObj) or app.func is None:
        stack.push(app)
        return next
    else:
        return app.saturate(next)

def RETURN_EVAL(next):
    app = stack.pop()

    oldapp = get_app()
    code = pop_frame()
    stack.push(app)
    return oldapp.setValue(code, app)
    #return 

def TABLE_SWITCH(next, count, tbl):
    item = stack.pop()
    #tramampoline(item.saturate(None))
    stack.push(item)
    tag = item.tag
    #print "fp", stack.fp, "tag", tag, "offset", tbl[tag][0]
    offset = tbl[tag][0]+1

    initial = stack.fp
    i = next
    while (i.pos - initial) < offset:
        i = i.func_defaults[1][0]
    return i

def LOOKUP_SWITCH(next, count, default, tbl):
    item = stack.pop()
    #tramampoline(item.saturate(None))
    stack.push(item)
    tag = item.tag
    try:
        offset = [x for x in tbl.values() if x[0] == tag][0]+1
    except IndexError:
        offset = default+1

    initial = stack.fp
    i = next
    while (i.pos - initial) < offset:
        i = i.func_defaults[1][0]
    return i

def INT_SWITCH(next, count, default, tbl):
    item = stack.pop()
    #tramampoline(item.saturate(None))
    stack.push(item)
    assert isinstance(item.value, Value) and isinstance(item.value.value, int), item.value
    try:
        offset = [x for x in tbl if x == item.value.value][0]
    except IndexError:
        offset = default

    initial = stack.fp
    i = next
    while (i.pos - initial) < offset:
        i = i.func_defaults[1][0]
    return i

def JUMP_FALSE(next):
        raise NotImplementedError

def JUMP(next):
        raise NotImplementedError

def ADD_W(next):
        raise NotImplementedError

def ADD_F(next):
        raise NotImplementedError

def ADD_D(next):
        raise NotImplementedError

def SUB_W(next):
        raise NotImplementedError

def SUB_F(next):
        raise NotImplementedError

def SUB_D(next):
        raise NotImplementedError

def MUL_W(next):
        raise NotImplementedError

def MUL_F(next):
        raise NotImplementedError

def MUL_D(next):
        raise NotImplementedError

def DIV_W(next):
        raise NotImplementedError

def DIV_F(next):
        raise NotImplementedError

def DIV_D(next):
        raise NotImplementedError

def MOD_W(next):
        raise NotImplementedError

def MOD_F(next):
        raise NotImplementedError

def MOD_D(next):
        raise NotImplementedError

def EQ_W(next):
        raise NotImplementedError

def EQ_F(next):
        raise NotImplementedError

def EQ_D(next):
        raise NotImplementedError

def NE_W(next):
        raise NotImplementedError

def NE_F(next):
        raise NotImplementedError

def NE_D(next):
        raise NotImplementedError

def LE_W(next):
        raise NotImplementedError

def LE_F(next):
        raise NotImplementedError

def LE_D(next):
        raise NotImplementedError

def LT_W(next):
        raise NotImplementedError

def LT_F(next):
        raise NotImplementedError

def LT_D(next):
        raise NotImplementedError

def GE_W(next):
        raise NotImplementedError

def GE_F(next):
        raise NotImplementedError

def GE_D(next):
        raise NotImplementedError

def GT_W(next):
        raise NotImplementedError

def GT_F(next):
        raise NotImplementedError

def GT_D(next):
        raise NotImplementedError

def NEG_W(next):
        raise NotImplementedError

def NEG_F(next):
        raise NotImplementedError

def NEG_D(next):
        raise NotImplementedError

def STRING(next):
    s = stack.pop()
    assert isinstance(s.value, str)
    if len(s.value) > 1:
        cstring = lookup(FakeFullyQualifId("", "_primCString"))
        stack.push(Application(lookup(FakeFullyQualifId("", "Cons")), [Value(s.value[0]), Application(cstring, [Value(s.value[1:])])]))
    else:
        stack.push(Application(lookup(FakeFullyQualifId("", "Cons")), [Value(s.value[0]), Application(lookup(FakeFullyQualifId("", "Nil")), [])]))
    return next

def FROM_ENUM(next):
        raise NotImplementedError

def PRIMITIVE(next):
    name = get_app().func.consts[0].item
    #print stack
    #print get_app().args
    assert hasattr(primitives, name), "Primitive %s not yet implemented" % (name, )
    stack.push(getattr(primitives, name)(*(get_app().args)))
    return next

def SELECTOR_EVAL(next):
        raise NotImplementedError

from objs import FakeFullyQualifId, ConObj
from state import stack, lookup, push_frame, pop_frame, get_app, tramampoline
from application import Application, Value
import primitives
