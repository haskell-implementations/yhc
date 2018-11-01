#ifndef __pretty_h__
#define __pretty_h__

#include "module.h"

void pr_binary(Int dig, UInt num);
Int pr_ins_table(Int i, UInt8* code);
Int pr_ins_lookup(Char* name, Int i, UInt8* code);
Int pr_ins(Int i, UInt8* code);
void pr_code(UInt8* code);
void pr_finfo(FInfo* info);
void pr_cinfo(CInfo* info);
void pr_pinfo(PInfo* info);
void pr_xinfo(XInfo* info);
void pr_info(Info* info);
void pr_object(Object* obj);
void pr_module(Module* mod);
void pr_nodeWith(Node* node, Node** args, Int depth);
void pr_node(Node* node, Int depth);
void pr_constItem(UByte type, ConstItem item);

#endif
