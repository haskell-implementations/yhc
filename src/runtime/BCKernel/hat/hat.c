#include <malloc.h>
#include <assert.h>

#include "../node.h"
#include "hat-c.h"
#include "hat.h"
#include "../primitive.h"

#define _DEBUG_ 0

#if _DEBUG_
# define SHOW(x) x
#else
# define SHOW(x)
#endif


HStackEntry* G_tp;
HStackEntry* G_tpLimit;
HStackEntry* G_tpBase;
HStackFrame* G_tfp;
HStackEntry  G_cp;
HStackEntry  G_up;

static HStackEntry traceCON(Node* node, Bool inTrusted, int depth, HStackEntry* args);
static HStackEntry traceNodeRec(Node* node, Bool inTrusted, int depth);

/* update a hatinfo given the corresponding hat module */



/* called to start hat */
void hgm_init(char* mainName){
#ifdef HAT
  /* allocate the tstack */
  G_tpLimit = (HStackEntry*)malloc(sizeof(HStackEntry) * TSTACK_SIZE);
  G_tpBase = &G_tpLimit[TSTACK_SIZE-1];
  G_tp = G_tpBase;
  G_tfp = (HStackFrame*)G_tpBase;

  /* current parent */
  G_cp.offset = 0;

  /* update pointer */
  G_up.offset = 0;

  /* open the file */
  hat_Open(mainName);
#endif
}

/* called when program finishes */
void hgm_exit(){
#ifdef HAT
  hat_Close();
#endif
}

/* print the trace stack */
void hgm_printStack(){
  HStackEntry* p;
  HStackEntry* end = ((HStackEntry*)G_tfp);

  printf("--- TSTACK ------------\n");
  for (p = G_tp; p < end; p++){
     char* flags[] = { "U", "T", "F", "H" };
     char* use[] = { "use", "nouse" };

     printf("  %s:0lx%08lx:[%s]\n", flags[p->flags], (UInt)p->offset, use[p->hadUse]);
  }
  printf("CP=0x%lx UP=0x%lx\n", (UInt)G_cp.offset, (UInt)G_up.offset);
  printf("-----------------------\n");
}


/* trace a module given a pointer to a HM_ object */
static FileOffset traceModule(HModule* mod){
  if (!mod){
    return 0;
  }
  if (mod->tracePtr){
    /* it's already traced so return that */
    return mod->tracePtr;
  }
  SHOW(printf("traceModule %p '%s'\n", mod, mod->name); fflush(stdout));
  /* update trace pointer and otherwise we need to trace it */
  if (!*mod->name){
    mod->tracePtr = mkRoot();
  }else{
    mod->tracePtr = mkModule(mod->name, mod->name, 1);
  }
  return mod->tracePtr;
}

/* trace source position */
static FileOffset traceSrcPos(HPos* srcptr){
  FileOffset mp;

  if (!srcptr){
    /* position information missing */
    return 0;
  }
  if (srcptr->tracePtr){
    /* it's already traced so return that */
    return srcptr->tracePtr;
  }
  /* otherwise trace it */
  mp = traceModule(srcptr->module);
  SHOW(printf("traceSrcPos %p\n", srcptr); fflush(stdout));
  srcptr->tracePtr = mkSrcPos(mp, srcptr->start, srcptr->end);
  /* update the PTR */
  return srcptr->tracePtr;
}

/* trace a string as a character list */
static FileOffset traceString(HPos* hpos, Bool inTrusted, const char* string){
  const char* p;
  Int len = strlen(string);

  /* generate list tail */
  FileOffset ret = traceCON(G_nodeNil, inTrusted, 1, NULL).offset;

  /* go over every character */
  for (p = &string[len-1]; p >= string; p--){
    /* trace the character */
    FileOffset sp = traceSrcPos(hpos);
    FileOffset chr = mkChar(G_cp.offset, sp, *p);

    /* trace the cons */
    CInfo* cinfo = (CInfo*)G_infoCons;
    HInfo* hinfo = &cinfo->hatInfo;
    FileOffset mp = traceModule(hinfo->module);
    FileOffset con = mkConstructor(mp, hpos->start, hpos->end, 0, cinfo->number, hinfo->name);
    ret = mkValueApp2(G_cp.offset, sp, con, chr, ret);
  }
  return ret;
}

/* trace a constructor node */
static HStackEntry traceCON(Node* node, Bool inTrusted, int depth, HStackEntry* args){
  CInfo* cinfo;
  HInfo* hinfo;
  HNode* hnode;
  HPos* hpos;
  Int start, end;
  FileOffset sp, fp;
  char* name;
  FileOffset argTemps[HAT_MAX_ARGS];

  /* get node info */
  cinfo = (CInfo*)NODE_INFO(node);
  hinfo = &cinfo->hatInfo;
  hnode = &node->hatNode;
  name = hinfo->name;

  /* get src information */
  hpos = (HPos*)hnode->data;
  start = hpos ? hpos->start : 0;
  end = hpos ? hpos->end : 0;
  sp = traceSrcPos(hpos);

  /* special constructors */
  if (cinfo == &G_infoChar){
    fp = mkChar(G_cp.offset, sp, ((INode*)node)->value);
  }else if (cinfo == &G_infoInt){
    fp = mkInt(G_cp.offset, sp, ((INode*)node)->value);
  }else if (cinfo == &G_infoFloat){
    fp = mkInt(G_cp.offset, sp, ((FloatNode*)node)->value);
  }else if (cinfo == &G_infoDouble){
    fp = mkInt(G_cp.offset, sp, ((DoubleNode*)node)->value);
  }else if (cinfo == &G_infoInteger){
    fp = mkInteger(G_cp.offset, sp, "<INTEGER>");
  }else if (cinfo == &G_infoString){
    StringNode* snode = (StringNode*)node;
    fp = traceString(hpos, inTrusted, snode->string);
  }else{
    Int fstArg, lstArg, size;
    Bool hasPtrs;
    FileOffset mp;

    size = node_size(node, &fstArg, &lstArg, &hasPtrs);
    /* trace all the arguments if not provided */
    if (args){
      Int i;
      for (i = fstArg; i <= lstArg && hasPtrs; i++){
        argTemps[i-fstArg] = args[i-fstArg].offset;
      }
    }else{
      Int i;
      for (i = fstArg; i <= lstArg && hasPtrs; i++){
        HStackEntry e = traceNodeRec(node->args[i], inTrusted, depth-1);
        argTemps[i-fstArg] = e.offset;
      }
    }
    if (!hasPtrs){
      /* special case for abstract things */
      fp = mkAbstract(name);
    }else{
      /* otherwise use the general constructor tracing */
      mp = traceModule(hinfo->module);
      fp = mkConstructor(mp, start, end, 0, cinfo->number, name);
      if (size == 0){
        fp = mkValueUse(G_cp.offset, sp, fp);
      }else{
        fp = mkValueAppN(G_cp.offset, sp, fp, size, argTemps);
      }
    }
  }
  /* update and return (careful of cyclic graphs) */
  if (!HN_IS_TRACED(hnode)){
    hnode->flags = HNFL_TRACED;
    hnode->hadUse = sp != 0;
    hnode->data = (void*)fp;
  }
  return HSTACK_ENTRY(HNFL_TRACED, fp, sp != 0);
}

/* trace a function */
static HStackEntry traceFunction(FInfo* finfo){
  HNode* hnode;
  HInfo* hinfo;
  FileOffset mp, fp;
  HPos* hpos;
  Int start, end;

  /* check we're not already traced */
  hnode = &finfo->hatNode;
  if (HN_IS_TRACED(hnode)){
    return HSTACK_ENTRY(hnode->flags, (FileOffset)hnode->data, hnode->hadUse);
  }

  /* check for lambda */
  if (finfo->flags & FFL_LAMBDA){
    /* this is actually a lambda, trace it as such */
    fp = mkLambda();
  }else{
    /* trace the module */
    hinfo = &finfo->hatInfo;
    mp = traceModule(hinfo->module);

    /* trace source pos */
    hpos = (HPos*)hnode->data;
    start = hpos ? hpos->start : 0;
    end = hpos ? hpos->end : 0;

    /* create the variable */
    fp = mkVariable(mp, start, end, 0 /* FIXME: fixity */, finfo->arity, hinfo->name, false /* FIXME: not always false */);
    if (finfo->arity == 0){
      /* special case for CAFs */
      fp = mkConstDef(0, fp);
    }
  }
  /* update flags and pointer */
  hnode->flags = HNFL_TRACED;
  hnode->hadUse = false;
  hnode->data = (void*)fp;
  return HSTACK_ENTRY(HNFL_TRACED, fp, false);
}

/* trace a VAP node */
static HStackEntry traceVAP(Node* node, Bool inTrusted, int depth, HStackEntry* args, PInfo* pinfoParm){
  PInfo* pinfo;
  FInfo* finfo;
  Int size;
  HPos* hpos;
  HNode* hnode;
  FileOffset sp, up, pp, fp, func;
  Bool hadUse;
  FileOffset argTemps[HAT_MAX_ARGS];

  /* decide which PInfo we'll use */
  if (!pinfoParm){
    pinfo = (PInfo*)NODE_INFO(node);
    hnode = &node->hatNode;
  }else{
    pinfo = pinfoParm;
    hnode = (HNode*)node;
  }

  size = pinfo->size;
  finfo = PINFO_FINFO(pinfo);

  if (finfo->flags & FFL_INVIS){
    /* invisible function, trace with forward only */
    return hgm_traceForward(node);
  }
  /* trace arguments if we don't have them */
  if (args){
    Int i;
    for (i = 0; i < size; i++){
      argTemps[i] = args[i].offset;
    }
  }else{
    Int i;
    for (i = 0; i < size; i++){
      HStackEntry e = traceNodeRec(node->args[i], inTrusted, depth-1);
      argTemps[i] = e.offset;
    }
  }
  /* trace positon */
  hpos = (HPos*)hnode->data;
  sp = traceSrcPos(hpos);
  /* trace the function */
  func = traceFunction(finfo).offset;
  /* different things for zero arity */
  if (size == 0){
    up = func;
    hadUse = false;    /* all traceFunction's are false */
    pp = G_cp.offset == 0 ? func : G_cp.offset;
    fp = mkConstUse(pp, sp, func);
  }else{
    func = mkValueUse(G_cp.offset, sp, func);
    hadUse = sp != 0;
    fp = up = mkAppN(G_cp.offset, sp, func, size, argTemps);
  }
  /* update info and flags (careful of cycles) */
  if (!HN_IS_TRACED(hnode)){
    hnode->flags = HNFL_TRACED;
    hnode->hadUse = hadUse;
    hnode->data = (void*)up;
  }
  return HSTACK_ENTRY(HNFL_TRACED, fp, sp != 0);
}

/* returns whether a node is traced */
static Bool isTraced(Node* node){
  return HN_IS_TRACED(&node->hatNode);
}

/* returns whether a node is trusted */
static Bool isTrusted(Node* node){
  PInfo* pinfo = (PInfo*)NODE_INFO(node);
  FInfo* finfo = PINFO_FINFO(pinfo);
  return (finfo->hatInfo.flags & HIFL_TRUSTED) != 0;
}

/* returns whether a node is invisible */
static Bool isInvisible(Node* node){
  PInfo* pinfo = (PInfo*)NODE_INFO(node);
  FInfo* finfo = PINFO_FINFO(pinfo);
  return (finfo->flags & FFL_INVIS) != 0;
}

/* returns whether a node is prim apply */
static Bool isPrimApply(Node* node){
  PInfo* pinfo = (PInfo*)NODE_INFO(node);
  FInfo* finfo = PINFO_FINFO(pinfo);
  return (finfo->flags & FFL_PRIM_APPLY) != 0;
}

/* enter a node */
static void enterNode(Node* node){
  HNode* hnode;

  hnode = &node->hatNode;

  switch (hnode->flags){
  case HNFL_TRACED:
    entResult((FileOffset)hnode->data, (FileOffset)hnode->hadUse);
    break;
  case HNFL_TRACED_HID:
    assert(!hnode->hadUse);
    entResult((FileOffset)hnode->data, false);
    break;
  case HNFL_TRACED_FW:
    entForward((FileOffset)hnode->data, 0 /* FIXME: what's this about? */);
    break;
  default:
    abort();
  }
}

/* give the result of a hnode */
static void resultHNode(HNode* hnode, FileOffset result){
 switch (hnode->flags){
  case HNFL_TRACED:
    resResult((FileOffset)hnode->data, result, (FileOffset)hnode->hadUse);
    break;
  case HNFL_TRACED_HID:
    assert(!hnode->hadUse);
    resResult((FileOffset)hnode->data, result, false);
    break;
  case HNFL_TRACED_FW:
    resForward((FileOffset)hnode->data, result);
    break;
  default:
    abort();
  }
}

/* give the result of a node */
static void resultNode(Node* node, FileOffset result){
  resultHNode(&node->hatNode, result);
}

/* give the result using a hstackentry */
static void resultHStackEntry(HStackEntry* ent, FileOffset result){
  HNode hnode;
  hnode.flags = ent->flags; hnode.hadUse = ent->hadUse; hnode.data = (void*)ent->offset;
  resultHNode(&hnode, result);
}


/* trace a node to a given depth */
static HStackEntry traceNodeRec(Node* node, Bool inTrusted, int depth){
  HNode* hnode;
  Info* info;

  REMOVE_IND(node, Node*);
  hnode = &node->hatNode;

  /* check it's not traced already */
  if (HN_IS_TRACED(hnode)){
    return HSTACK_ENTRY(hnode->flags, (FileOffset)hnode->data, hnode->hadUse);
  }

  /* check whether we've run out of depth */
  if (depth <= 0){
    FileOffset fp = mkForward(0);
    hnode->flags = HNFL_TRACED_FW;
    hnode->hadUse = false;
    hnode->data = (void*)fp;
    return HSTACK_ENTRY(HNFL_TRACED_FW, fp, false);
  }

  /* do different things depending on node info */
  info = NODE_INFO(node);
  if (info->tag == I_CINFO){
    return traceCON(node, inTrusted, depth, NULL);
  }else if (info->tag == I_PINFO){
    FInfo* finfo = PINFO_FINFO((PInfo*)info);
    if ((finfo->hatInfo.flags & HIFL_INVISIBLE) ||
        (inTrusted && finfo->hatInfo.flags & HIFL_TRUSTED)){
      FileOffset fp = mkForward(0);
      hnode->flags = HNFL_TRACED_FW;
      hnode->hadUse = false;
      hnode->data = (void*)fp;
      return HSTACK_ENTRY(HNFL_TRACED_FW, fp, false);
    }else{
      return traceVAP(node, inTrusted, depth, NULL, NULL);
    }
  }else{
    abort();
  }
}

/* trace a heap node */
HStackEntry hgm_traceNode(Node* node, Bool inTrusted){
  return traceNodeRec(node, inTrusted, 5);
}

/* trace a variable */
HStackEntry hgm_traceVar(Node* node, HVarDesc* desc){
  FileOffset up, dp, sp, vp, mp, pp;
  HStackEntry ne;
  HPos* pos;

  pos   = &desc->pos;
  mp    = traceModule(pos->module);
  sp    = traceSrcPos(pos);
  vp    = mkVariable(mp, pos->start, pos->end, 0, 0, desc->name, true);
  pp    = G_cp.offset;
  dp    = mkConstDef(pp, vp);
  up    = mkConstUse(pp, sp, dp);
  ne    = hgm_traceNode(node, false);
  entResult(dp, 0);
  resResult(dp, ne.offset, 0);
  return HSTACK_ENTRY(HNFL_TRACED, up, true);
}

/* trace an application */
HStackEntry hgm_traceAp(Node* node, HPos* pos, HStackEntry* args){
  HNode* hnode;

  hnode = &node->hatNode;
  hnode->data = (HPos*)pos;
  return traceVAP(node, false, 1, args, NULL);
}

/* trace a primitive application */
HStackEntry hgm_tracePrimAp(HPos* pos, HStackEntry* args, PInfo* pinfo){
  HNode hnode;

  hnode.flags = HNFL_UNTRACED;
  hnode.data = (HPos*)pos;
  return traceVAP((Node*)&hnode, false, 1, args, pinfo);
}

/* trace a constructor */
HStackEntry hgm_traceCon(Node* node, HPos* pos, HStackEntry* args){
  HNode* hnode;

  hnode = &node->hatNode;
  hnode->data = (HPos*)pos;
  return traceCON(node, false, 1, args);
}

/* trace a forward */
HStackEntry hgm_traceForward(Node* node){
  FileOffset fp;
  HNode* hnode;
  Info* info;

  hnode = &node->hatNode;
  /* might be traced already */
  if (HN_IS_TRACED(hnode)){
    return HSTACK_ENTRY(hnode->flags, (FileOffset)hnode->data, hnode->hadUse);
  }
  /* check the info */
  info = NODE_INFO(node);
  if (info->tag == I_CINFO){
    /* trace constructors */
    fp = traceCON(node, false, 1, NULL).offset;
    hnode->flags = HNFL_TRACED;
  }else{
    /* everything else trace with forward */
    fp = mkForward(0);
    hnode->flags = HNFL_TRACED_FW;
  }
  hnode->hadUse = false;
  hnode->data = (void*)fp;
  return HSTACK_ENTRY(hnode->flags, fp, false);
}

/* trace an application */
HStackEntry hgm_traceApply(Node* node, HStackEntry f, Int nargs, HPos* pos, HStackEntry* args){
  FileOffset argTemps[HAT_MAX_ARGS];
  Int i;
  HNode* hnode = &node->hatNode;

  /* trace the src pos */
  FileOffset sp = traceSrcPos(pos);

  /* trace arguments */
  for (i = 0; i < nargs; i++){
    argTemps[i] = args[i].offset;
  }

  /* make application */
  FileOffset ap = mkAppN(G_cp.offset, sp, f.offset, nargs, argTemps);

  /* update the node */
  hnode->flags = HNFL_TRACED;
  hnode->hadUse = pos != NULL;
  hnode->data = (void*)ap;

  /* return the stack entry */
  return HSTACK_ENTRY(HNFL_TRACED, ap, pos != NULL);
}

/* called when a primitive application provides a result */
void hgm_primResult(HStackEntry ap, HStackEntry res){
  entResult(ap.offset, ap.hadUse);
  resResult(ap.offset, res.offset, ap.hadUse);
}

/* the G_vapptrMode is used to pass information between a RETURN_EVAL code and the
   following EVAL code (since RETURN_EVAL jumps to EVAL). EVAL needs to know whether the
   node under evaluation (vapptr) is trusted, unfortunately RETURN_EVAL will have just
   chomped over that information by updating it with an indirection. Hence we use
   G_vapptrMode to pass the information from one of the other.

   either vapptr was trusted, it was not trusted, or it's unknown because we didn't
   get to this EVAL via a RETURN_EVAL (in which case we can check the node itself) */
typedef enum { VAPPTR_TRUSTED, VAPPTR_UNTRUSTED, VAPPTR_UNKNOWN } VapptrMode;

static VapptrMode G_vapptrMode = VAPPTR_UNKNOWN;

/* called when we eval a node */
void hgm_eval(Node* vapptr, Node* nodeptr){
  Bool vapptrTrusted = false;
  HStackEntry newCP;
  HStackFrame* newFP;
#if _DEBUG_
  char* vname = vapptr ? PINFO_FINFO((PInfo*)NODE_INFO(vapptr))->name : 0;
  char* nname = nodeptr ? PINFO_FINFO((PInfo*)NODE_INFO(nodeptr))->name : 0;
#endif

  /* push stack frame */
  newFP = (HStackFrame*)(G_tp - HSTACK_FRAME_SIZE);
  newFP->fp = G_tfp;
  newFP->cp = G_cp;
  newFP->up = G_up;
  G_tp -= HSTACK_FRAME_SIZE;
  G_tfp = newFP;

  if (!vapptr){
    /* this happens when the root node is being evaluated, it never needs tracing ... */
    return;
  }
  SHOW(printf("hgm_eval(%p[%s], %p[%s])\n", vapptr, vname, nodeptr, nname));
  SHOW(hgm_printStack());
  switch (G_vapptrMode){
  case VAPPTR_TRUSTED: vapptrTrusted = true; break;
  case VAPPTR_UNTRUSTED: vapptrTrusted = false; break;
  case VAPPTR_UNKNOWN: vapptrTrusted = isTrusted(vapptr); break;
  }
  G_vapptrMode = VAPPTR_UNKNOWN;

  SHOW(printf("\tvapptr is %strusted\n", vapptrTrusted ? "" : "un"));

  if (vapptrTrusted){
    /* call from a trusted function */
    if (isTrusted(nodeptr)){
      /* to a trusted function */
      SHOW(printf("\teval trusted function\n"));
      if (isTraced(nodeptr)){
        FileOffset hid;
        /* enter the node if it's traced */
        enterNode(nodeptr);
        /* make a hidden node */
        if (nodeptr->hatNode.flags == HNFL_TRACED){
          /* only for non hidden, non forward nodes */
          if (!isPrimApply(nodeptr)){
            hid = mkHidden(G_cp.offset);
            resultNode(nodeptr, hid);
            entResult(hid, 0);
            nodeptr->hatNode.flags = HNFL_TRACED_HID;
            nodeptr->hatNode.data = (void*)hid;
            nodeptr->hatNode.hadUse = false;
          }
        }
      }
      /* no further tracing needed */
      G_cp.offset = 0;
      G_cp.hadUse = false;
    }else{
      SHOW(printf("\teval untrusted function\n"));
      /* to an untrusted function, ensure node is traced */
      hgm_traceNode(nodeptr, True);
      /* enter the node */
      enterNode(nodeptr);
      /* update the current parent */
      newCP = HSTACK_ENTRY(nodeptr->hatNode.flags, (FileOffset)nodeptr->hatNode.data, nodeptr->hatNode.hadUse);
      /* set the new update node */
      G_up = newCP;
      /* update CP if appropriate */
      if (!isInvisible(nodeptr)){
        G_cp = newCP;
      }
    }
  }else{
    /* call from untrusted function */
    if (!isTraced(nodeptr)){
      /* to an untraced function (= unimportant from a tracing point of view) */
      SHOW(printf("\node not traced\n"));
      return;
    }
    /* otherwise enter the node */
    enterNode(nodeptr);
    newCP = HSTACK_ENTRY(nodeptr->hatNode.flags, (FileOffset)nodeptr->hatNode.data, nodeptr->hatNode.hadUse);
    if (isTrusted(nodeptr)){
      /* call to trusted function, make intermediate 'hidden' node */
      SHOW(printf("\teval trusted function\n"));
      if (!isPrimApply(nodeptr)){
        FileOffset hid = mkHidden(newCP.offset);
        resultNode(nodeptr, hid);
        entResult(hid, 0);
        nodeptr->hatNode.flags = HNFL_TRACED_HID;
        nodeptr->hatNode.data = (void*)hid;
        nodeptr->hatNode.hadUse = false;
        newCP = HSTACK_ENTRY(HNFL_TRACED, hid, false);
      }
    }else{
      SHOW(printf("\teval untrusted function\n"));
      /* set the new update node */
      G_up = newCP;
    }
    /* update CP if appropriate */
    if (!isInvisible(nodeptr)){
      G_cp = newCP;
    }
  }
}

/* pop the frame on the top of the trace stack */
HStackEntry hgm_popStack(){
  HStackEntry oldCP = G_tfp->cp;
  G_up  = G_tfp->up;
  G_tp  = (HStackEntry*)(G_tfp) + HSTACK_FRAME_SIZE;
  G_tfp = G_tfp->fp;
  return oldCP;
}

/* called when we return a result */
Bool hgm_return(Node* vapptr, Node* nodeptr, Bool andEval){
  HStackEntry oldCP;

  SHOW(printf("hgm_return(%p, %p, %ld)\n", vapptr, nodeptr, andEval));
  SHOW(hgm_printStack());

  if (!isTrusted(vapptr)){
    /* untrusted return, give result */
    HStackEntry result = *G_tp++;

    SHOW(printf("\treturn from untrusted function\n"));
    SHOW(printf("\t flags %ld, hadUse %ld, offs %ld\n", (UInt)G_up.flags, (UInt)G_up.hadUse, (UInt)G_up.offset));

    resultHStackEntry(&G_up, result.offset);

    /* pop stack frame */
    oldCP = hgm_popStack();

    G_vapptrMode = andEval ? VAPPTR_UNTRUSTED : VAPPTR_UNKNOWN;
    G_cp = oldCP;
    return false;
  }else{
    /* pop stack frame */
    oldCP = hgm_popStack();

    G_vapptrMode = andEval ? VAPPTR_TRUSTED : VAPPTR_UNKNOWN;
    /* is it traced */
    if (!isTraced(vapptr)){
      /* not traced, result not wanted, therefore trace nothing */
      SHOW(printf("\tno result required\n"));
    }else{
      /* must be traced */
      if (isTrusted(nodeptr) && andEval){
        /* RETURN_EVAL to another trusted function ... 'hand-off' */
        nodeptr->hatNode.flags = vapptr->hatNode.flags;
        nodeptr->hatNode.data = vapptr->hatNode.data;
        SHOW(printf("\thand-off to %p\n", nodeptr));
      }else{
        /* something needs to be traced */
        HStackEntry fp;
        /* special hack for _Builtin._apply, trace the node as a forward only */
        if (isPrimApply(vapptr)){
          fp = hgm_traceForward(nodeptr);
        }else{
          fp = hgm_traceNode(nodeptr, True);
        }
        resultNode(vapptr, fp.offset);
        SHOW(printf("\trecursively tracing result\n"));
      }
    }
    G_cp = oldCP;
    return true;
  }
}

/* called when a case statement is traced */
#define HGM_TRACE_CASE(_ins) \
  HStackEntry hgm_trace##_ins(HStackEntry exp, HPos* pos){ \
    FileOffset pp = traceSrcPos(pos); \
    FileOffset cas = mk##_ins(G_cp.offset, pp, exp.offset); \
    resultHStackEntry(&G_up, cas); \
    entResult(cas, pp); \
    SHOW(printf("trace" #_ins " 0x%lx 0x%lx 0x%lx\n", (UInt)exp.offset, (UInt)pp, (UInt)G_up.offset)); \
    G_cp = G_up = HSTACK_ENTRY(HNFL_TRACED, cas, pos != NULL); \
    return G_cp; \
  }

HGM_TRACE_CASE(Case);
HGM_TRACE_CASE(If);
HGM_TRACE_CASE(Guard);

/* called to trace a projection */
HStackEntry hgm_traceProjection(HPos* pos, HStackEntry exp){
  FileOffset sp = traceSrcPos(pos);
  FileOffset fp = mkProjection(G_cp.offset, sp, exp.offset);
  return HSTACK_ENTRY(HNFL_TRACED, fp, true);
}

/*
HStackEntry hgm_traceCase(HStackEntry exp, HPos* pos){
  FileOffset pp = traceSrcPos(pos);
  FileOffset cas = mkCase(G_cp.offset, pp, exp.offset);
  resultHStackEntry(&G_up, cas);
  entResult(cas, pp);
  SHOW(printf("traceCase 0x%x 0x%x 0x%x\n", exp.offset, pp, G_up.offset));
  SHOW(hgm_printStack());
  G_cp = G_up = HSTACK_ENTRY(HNFL_TRACED, cas, pos != NULL);
  return G_cp;
}
*/

/*******************************************************************************************************

   SHELF

 *******************************************************************************************************/

#if 0



#endif


