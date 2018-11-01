code = """
#include "stdio.h"
#include "gmp.h"

int main(int argc, char** argv) {
    printf("%i.%i.%i", __GNU_MP_VERSION, __GNU_MP_VERSION_MINOR, __GNU_MP_VERSION_PATCHLEVEL);
    return 0;
}
"""

def CheckGMPVersion(context, env):
    context.Message("Checking for libgmp version... ")
    res, output = context.TryRun(code, ".c")
    if res:
        context.Result(output.strip())
        return output.strip()
    else:
        context.Result(0)
        return None
