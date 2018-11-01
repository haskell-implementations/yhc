/* a simplified version of bytecodes.h for things that don't want to
   define complex operations */
#define op(n,x) ins(x),
#define op1(n,x) ins(x),
#define op2(n,x) ins(x),
#define op1S(n,x) ins(x),
#define op2S(n,x) ins(x),
#define op1_1(n,x) ins(x),
#define op2_1(n,x) ins(x),
#define opT(n,x) ins(x),
#define opL(n,x) ins(x),
#define opJ(n,x) ins(x),
# include "bytecodes.h"
#undef op
#undef op1
#undef op2
#undef op1S
#undef op2S
#undef op1_1
#undef op2_1
#undef opT
#undef opL
#undef opJ
