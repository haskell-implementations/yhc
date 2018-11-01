from objs import ConObj, FunObj
from StringIO import StringIO

# [(NEED_HEAP, 32),(PUSH_ZAP_ARG, 1),(PUSH_ZAP_ARG, 0),(EVAL),(APPLY, 1),(RETURN_EVAL)]
_apply = FunObj(StringIO("\x02\x00\x00\x00\x00\x00\x06\x01\x18\x17\x5A\x42\x5B"))
_apply.name = "_apply"

# [(NEED_HEAP, 32). (PUSH_ZAP_ARG, 0). (STRING). (RETURN)]
_primCString = FunObj(StringIO("\x01\x00\x00\x00\x00\x00\x04\x01\x17\x85\x59"))
_primCString.name = "_primCString"

Nil = ConObj(StringIO("\x00\x00"))
Cons = ConObj(StringIO("\x02\x01"))

export = ["_apply", "_primCString", "Nil", "Cons"]

del StringIO
del ConObj
del FunObj
