code = """
#include <stdio.h>

int main(int argc, char** argv) {
    printf("%%i\\n", sizeof(%s));
    return 0;
}
"""

def CheckTypeSize(context, type):
    context.Message("Checking size of %s... " % (type, ))
    res, output = context.TryRun(code % (type, ), ".c")
    if res:
        try:
            v = int(output)
        except ValueError:
            context.Result(0)
            return 0
        else:
            context.Result(output[:-1]) # strip the trailing new line
            return v
    else:
        context.Result(0)
        return 0

def CheckCharSize(context):
    return CheckTypeSize(context, "char")
def CheckShortSize(context):
    return CheckTypeSize(context, "short")
def CheckIntSize(context):
    return CheckTypeSize(context, "int")
def CheckLongSize(context):
    return CheckTypeSize(context, "long")
def CheckLongLongSize(context):
    return CheckTypeSize(context, "long long")
def CheckFloatSize(context):
    return CheckTypeSize(context, "float")
def CheckDoubleSize(context):
    return CheckTypeSize(context, "double")
def CheckVoidPtrSize(context):
    return CheckTypeSize(context, "void*")
