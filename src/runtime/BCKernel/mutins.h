/* all the code for the mutator has been moved to here and is included by mutator.c,
   this makes both this code and the rest of the mutator much more readable */

/*-------------------------------------------------------------------------*/
Case(NEED_HEAP) i = (*ip++) * 32; n = 2; goto I_NeedHeap;
Case(NEED_HEAP_32) i = 32; n = 1;
  I_NeedHeap: {
      Bool needit;

      /* check for process switch */
      G_insBeforeSwitch--;
      if (G_insBeforeSwitch <= 0){
        /* move ip to rerun this instruction */
        ip -= n;

        /* switch */
        PRE_SWITCH();
        proc_switch(PM_READY);
        POST_SWITCH();
        Break;
      }

#        if ALWAYS_NEED_HEAP
      needit = true;
#        else
      needit = hp + i >= G_hpEnd;
#        endif

    if (needit){
      PRE_SWITCH();
      heap_gc(i);
      POST_SWITCH();
    }
#        if QUITE_PARANOID
      PRE_SWITCH();
      sanity_heap(false);
      POST_SWITCH();
#        endif
    Break;
  }

/*-------------------------------------------------------------------------*/
Case(PUSH_P1) nodeptr = sp[I_ARG_UB]; *--sp = nodeptr; ip += 1; Break;
Case(PUSH_P2) nodeptr = sp[I_ARG_US]; *--sp = nodeptr; ip += 2; Break;
Case(PUSH_0) nodeptr = sp[0]; *--sp = nodeptr; Break;
Case(PUSH_1) nodeptr = sp[1]; *--sp = nodeptr; Break;

/*-------------------------------------------------------------------------*/
Case(PUSH_ZAP_P1) i = I_ARG_UB; nodeptr = sp[i]; sp[i] = G_nodeZapStack; *--sp = nodeptr; ip += 1; Break;
Case(PUSH_ZAP_P2) i = I_ARG_US; nodeptr = sp[i]; sp[i] = G_nodeZapStack; *--sp = nodeptr; ip += 2; Break;
Case(PUSH_ZAP_0) nodeptr = sp[0]; sp[0] = G_nodeZapStack; *--sp = nodeptr; Break;
Case(PUSH_ZAP_1) nodeptr = sp[1]; sp[1] = G_nodeZapStack; *--sp = nodeptr; Break;
Case(PUSH_ZAP_2) nodeptr = sp[2]; sp[2] = G_nodeZapStack; *--sp = nodeptr; Break;
Case(PUSH_ZAP_3) nodeptr = sp[3]; sp[3] = G_nodeZapStack; *--sp = nodeptr; Break;

/*-------------------------------------------------------------------------*/
Case(ZAP_STACK_P1) sp[I_ARG_UB] = G_nodeZapStack; ip += 1; Break;
Case(ZAP_STACK_P2) sp[I_ARG_US] = G_nodeZapStack; ip += 2; Break;

/*-------------------------------------------------------------------------*/
Case(PUSH_ARG) *--sp = vapptr->args[I_ARG_UB]; ip += 1; Break;
Case(PUSH_ARG_0) *--sp = vapptr->args[0]; Break;
Case(PUSH_ARG_1) *--sp = vapptr->args[1]; Break;
Case(PUSH_ARG_2) *--sp = vapptr->args[2]; Break;
Case(PUSH_ARG_3) *--sp = vapptr->args[3]; Break;

/*-------------------------------------------------------------------------*/
Case(PUSH_ZAP_ARG) i = I_ARG_UB; *--sp = vapptr->args[i]; WHEN_ZAP(vapptr->args[i] = G_nodeZapArg); ip += 1; Break;
Case(PUSH_ZAP_ARG_0) *--sp = vapptr->args[0]; WHEN_ZAP(vapptr->args[0] = G_nodeZapArg); Break;
Case(PUSH_ZAP_ARG_1) *--sp = vapptr->args[1]; WHEN_ZAP(vapptr->args[1] = G_nodeZapArg); Break;
Case(PUSH_ZAP_ARG_2) *--sp = vapptr->args[2]; WHEN_ZAP(vapptr->args[2] = G_nodeZapArg); Break;
Case(PUSH_ZAP_ARG_3) *--sp = vapptr->args[3]; WHEN_ZAP(vapptr->args[3] = G_nodeZapArg); Break;

/*-------------------------------------------------------------------------*/
Case(ZAP_ARG) WHEN_ZAP(vapptr->args[I_ARG_UB] = G_nodeZapArg); ip += 1; Break;
Case(ZAP_ARG_0) WHEN_ZAP(vapptr->args[0] = G_nodeZapArg); Break;
Case(ZAP_ARG_1) WHEN_ZAP(vapptr->args[1] = G_nodeZapArg); Break;

/*-------------------------------------------------------------------------*/
Case(PUSH_CONST_P1) *--sp = (Node*)constptr[I_ARG_UB]; ip += 1; Break;
Case(PUSH_CONST_P2) *--sp = (Node*)constptr[I_ARG_US]; ip += 2; Break;
Case(PUSH_CONST_0) *--sp = (Node*)constptr[0]; Break;
Case(PUSH_CONST_1) *--sp = (Node*)constptr[1]; Break;
Case(PUSH_CONST_2) *--sp = (Node*)constptr[2]; Break;
Case(PUSH_CONST_3) *--sp = (Node*)constptr[3]; Break;
Case(PUSH_CONST_4) *--sp = (Node*)constptr[4]; Break;
Case(PUSH_CONST_5) *--sp = (Node*)constptr[5]; Break;
Case(PUSH_CONST_6) *--sp = (Node*)constptr[6]; Break;
Case(PUSH_CONST_7) *--sp = (Node*)constptr[7]; Break;

/*-------------------------------------------------------------------------*/
Case(PUSH_INT_P2) i = I_ARG_S; ip += 2; goto I_PushInt;
Case(PUSH_INT_0) i = 0; goto I_PushInt;
Case(PUSH_INT_1) i = 1; goto I_PushInt;
Case(PUSH_INT_P1) i = I_ARG_B; ip += 1;
  I_PushInt:{
    INode* in;
    MAKE_INT(in,i);
    *--sp = (Node*)in;
    Break;
  }

/*-------------------------------------------------------------------------*/
Case(PUSH_CHAR){
  INode* chr;
  i = I_ARG_UB; ip += 1;
  MAKE_CHAR(chr,i);
  *--sp = (Node*)chr;
  Break;
}

/*-------------------------------------------------------------------------*/
Case(MK_AP_P2) i = I_ARG_US; ip += 2; goto I_MkAp;
Case(MK_AP_0) i = 0; goto I_MkAp;
Case(MK_AP_1) i = 1; goto I_MkAp;
Case(MK_AP_2) i = 2; goto I_MkAp;
Case(MK_AP_3) i = 3; goto I_MkAp;
Case(MK_AP_4) i = 4; goto I_MkAp;
Case(MK_AP_5) i = 5; goto I_MkAp;
Case(MK_AP_6) i = 6; goto I_MkAp;
Case(MK_AP_7) i = 7; goto I_MkAp;
Case(MK_AP_8) i = 8; goto I_MkAp;
Case(MK_AP_9) i = 9; goto I_MkAp;
Case(MK_AP_10) i = 10; goto I_MkAp;
Case(MK_AP_11) i = 11; goto I_MkAp;
Case(MK_AP_12) i = 12; goto I_MkAp;
Case(MK_AP_13) i = 13; goto I_MkAp;
Case(MK_AP_14) i = 14; goto I_MkAp;
Case(MK_AP_15) i = 15; goto I_MkAp;
Case(MK_AP_P1) i = I_ARG_UB; ip += 1;
  I_MkAp:{
    FInfo* finfo = (FInfo*)constptr[i];
    PInfo* pinfo;
    Int j;

    i = finfo->arity;
    pinfo = &finfo->papTable[i];

    ASSERT(finfo->info.tag == I_FINFO || finfo->info.tag == I_XINFO);
    ASSERT(pinfo->info.tag == I_PINFO);

    nodeptr = (Node*)hp;
    MAKE_NODE(nodeptr, pinfo, N_NORMAL);
    INIT_HATNODE(nodeptr, NULL);
    hp += wordsof(Node) + i;
    for (j = 0; j < i; j++){
      Node* arg = *sp++;
      REMOVE_IND(arg, Node*);
      nodeptr->args[j] = arg;
      ASSERT(arg != G_nodeZapStack && arg != G_nodeZapArg);
    }
    *--sp = nodeptr;
    Break;
  }

/*-------------------------------------------------------------------------*/
Case(MK_PAP_P2) i = I_ARG_US; n = I_ARG_UB3; ip += 3; goto I_MkPap;
Case(MK_PAP_P1) i = I_ARG_UB; n = I_ARG_UB2; ip += 2;
  I_MkPap:{
    FInfo* finfo = (FInfo*)constptr[i];
    PInfo* pinfo;
    Int j;

    pinfo = &finfo->papTable[n];

    ASSERT(finfo->info.tag == I_FINFO);
    ASSERT(pinfo->info.tag == I_PINFO);

    nodeptr = (Node*)hp;
    MAKE_NODE(nodeptr, pinfo, N_NORMAL);
    INIT_HATNODE(nodeptr, NULL);
    hp += wordsof(Node) + n;
    for (j = 0; j < n; j++){
      Node* arg = *sp++;
      REMOVE_IND(arg, Node*);
      nodeptr->args[j] = arg;
    }
    *--sp = nodeptr;
    Break;
  }

/*-------------------------------------------------------------------------*/
Case(MK_CON_P2) i = I_ARG_US; ip += 2; goto I_MkCon;
Case(MK_CON_0) i = 0; goto I_MkCon;
Case(MK_CON_1) i = 1; goto I_MkCon;
Case(MK_CON_2) i = 2; goto I_MkCon;
Case(MK_CON_3) i = 3; goto I_MkCon;
Case(MK_CON_P1) i = I_ARG_UB; ip += 1;
  I_MkCon:{
    CInfo* cinfo = (CInfo*)constptr[i];
    Int j;

    ASSERT(cinfo->info.tag == I_CINFO);

    i = cinfo->size;
    nodeptr = (Node*)hp;
    MAKE_NODE(nodeptr, cinfo, N_NORMAL);
    INIT_HATNODE(nodeptr, NULL);
    hp += wordsof(Node) + i;
    for (j = 0; j < i; j++){
      Node* arg = *sp++;
      REMOVE_IND(arg, Node*);
      nodeptr->args[j] = arg;
    }
    *--sp = nodeptr;
    Break;
  }

/*-------------------------------------------------------------------------*/
Case(SLIDE_P1) nodeptr = *sp; sp += I_ARG_UB; sp[0] = nodeptr; ip += 1; Break;
Case(SLIDE_P2) nodeptr = *sp; sp += I_ARG_US; sp[0] = nodeptr; ip += 2; Break;
Case(SLIDE_1) nodeptr = *sp; sp += 1; sp[0] = nodeptr; Break;
Case(SLIDE_2) nodeptr = *sp; sp += 2; sp[0] = nodeptr; Break;

/*-------------------------------------------------------------------------*/
Case(POP_P1) i = I_ARG_UB; ip += 1; sp += i; Break;
Case(POP_P2) i = I_ARG_US; ip += 2; sp += i; Break;

/*-------------------------------------------------------------------------*/
Case(ALLOC_P2) i = I_ARG_US; ip += 2; goto I_Alloc;
Case(ALLOC_P1) i = I_ARG_UB; ip += 1;
  I_Alloc: {
    Node* inds;
    PInfo* pinfo;

    inds = (Node*)hp;
    hp += wordsof(Node) * i;
    pinfo = &G_infoBlackHole->papTable[0];
    for (;i > 0; i--, inds++){
      MAKE_NODE(inds, pinfo, N_HOLE);
      INIT_HATNODE(inds, NULL);
      *--sp = inds;
    }
  }
  Break;

/*-------------------------------------------------------------------------*/
Case(UPDATE_P2) i = I_ARG_US; ip += 2; goto I_Update;
Case(UPDATE_P1) i = I_ARG_UB; ip += 1;
  I_Update: {
    Node* update;

    nodeptr = *sp++;
    update = sp[i];
    REMOVE_IND(nodeptr, Node*);
    UPDATE_RESULT(update, nodeptr);
    sp[i] = nodeptr;
  }
  Break;

/*-------------------------------------------------------------------------*/
Case(UNPACK)
  CInfo* cinfo;

  nodeptr = *sp++;
  REMOVE_IND(nodeptr, Node*);
  cinfo = (CInfo*)NODE_INFO(nodeptr);

  ASSERT(cinfo->info.tag == I_CINFO);
  for (i = ((Int) cinfo->size) - 1; i >= 0; i--){
    *--sp = nodeptr->args[i];
  }
  Break;

/*-------------------------------------------------------------------------*/
Case(APPLY_1) i = 1; goto I_Apply;
Case(APPLY_2) i = 2; goto I_Apply;
Case(APPLY) i = I_ARG_UB; ip += 1;
  I_Apply:{
    PInfo* pinfo;
    Int num, size, want, words;
    Node* vap;

    num = i;
    nodeptr = sp[0];
    REMOVE_IND(nodeptr, Node*);

    pinfo = (PInfo*)NODE_INFO(nodeptr);
    size = pinfo->size;
    want = MIN(num, pinfo->need);
    num -= want;

    words = wordsof(Node) + (size+want) + num*(wordsof(Node)+2);
    /* we need to check the heap size dynamically, because it can't be done statically */
    if (hp + words >= G_hpEnd){
      PRE_SWITCH();
      heap_gc(words);
      POST_SWITCH();
      /* go round again, stack variables might have changed etc .. */
      goto I_Apply;
    }
    /* now that we've checked we can change the state */
    sp++;

    ASSERT(pinfo->info.tag == I_PINFO);

    vap = (Node*)hp;
    MAKE_NODE(vap, pinfo+want, N_NORMAL);
    INIT_HATNODE(vap, NULL);
    hp += wordsof(Node) + (size+want);
    for (i = 0; i < size; i++){
      vap->args[i] = nodeptr->args[i];
    }
    for (i = 0; i < want; i++){
      Node* arg = *sp++;
      REMOVE_IND(arg, Node*);
      vap->args[size+i] = arg;
    }
    while (num > 0){
      Node* apply = (Node*)hp;
      Node* arg;
      MAKE_NODE(apply, &G_infoApply->papTable[2], N_NORMAL);
      INIT_HATNODE(apply, NULL);
      hp += wordsof(Node) + 2;
      apply->args[0] = vap;
      arg = *sp++;
      REMOVE_IND(arg, Node*);
      apply->args[1] = arg;
      vap = apply;
      num--;
    }
    *--sp = vap;
    Break;
  }

/*-------------------------------------------------------------------------*/
Case(SELECTOR_EVAL)
  *--sp = vapptr->args[0];
  goto I_Eval;

/*-------------------------------------------------------------------------*/
Case(RETURN)
  nodeptr = *sp++;
  REMOVE_IND(nodeptr, Node*);
#ifdef HAT
  hgm_return(vapptr, nodeptr, false);
#endif
  UPDATE_RESULT(vapptr, nodeptr);
  POP_FRAME();
  Break;

/*-------------------------------------------------------------------------*/
Case(SELECT_0) i = 0; goto I_Select;
Case(SELECT_1) i = 1; goto I_Select;
Case(SELECT_P2) i = I_ARG_US; ip += 2; goto I_Select;
Case(SELECT_P1) i = I_ARG_UB; ip += 1;
  I_Select:
    nodeptr = sp[0];
    REMOVE_IND(nodeptr, Node*);

    ASSERT(NODE_INFO(nodeptr)->tag == I_CINFO);
    sp[0] = nodeptr->args[i];
/* Fall through */

/*-------------------------------------------------------------------------*/
Case(RETURN_EVAL)
  nodeptr = *sp++;
  REMOVE_IND(nodeptr, Node*);
#ifdef HAT
  hgm_return(vapptr, nodeptr, false);
#endif
  UPDATE_RESULT(vapptr, nodeptr);
  POP_FRAME();
  /* Fall through */

/*-------------------------------------------------------------------------*/
Case(EVAL)
  I_Eval:{
    Info* iinfo;
    Int need;

    nodeptr = sp[0];
    REMOVE_IND(nodeptr, Node*);
    sp[0] = nodeptr;
    iinfo = NODE_INFO(nodeptr);

    if (iinfo == (Info*)G_infoBlackHole){
      fprintf(stderr, "About to evaluate black hole\n");
      exit(-1);
    }

    if (iinfo->tag == I_CINFO){
      Break;
    }else if (iinfo->tag == I_PINFO){
      PInfo* pinfo = (PInfo*)iinfo;
      FInfo* finfo;

      if (NODE_FLAGS(nodeptr) == N_HOLE){
          /* FIXME: this is simply unfair in the case of concurrent Haskell,
          for example it could easily just mean that the other thread is
          already evaluating it. */
#if 0
        fprintf(stderr, "Evaluation of black hole\n");
        trace(ip-1,sp,fp,hp,vapptr);
        exit(-1);
#else
        PRE_SWITCH();
        proc_blockHoled(nodeptr);
        POST_SWITCH();
        Break;
#endif
      }
      if (pinfo->need){
        Break;
      }
      finfo = PINFO_FINFO(pinfo);
      ASSERT(finfo->info.tag == I_FINFO);

#     ifdef HAT
        hgm_eval(vapptr, nodeptr);
#     endif

      /* check we have enough stack, we need enough for two frames because
        a) we must always have enough room for a frame (because they can be pushed by GC for example).
        b) we know that we're going to use up one frame with the PUSH_FRAME below
        c) so we need enough space for 2 frames.
      */
      need = FRAME_SIZE * 2 + finfo->stack;

      /*SHOW(printf("Stack check: left=%d, finfo->stack=%d, need=%d\n",
            sp - G_spLimit, finfo->stack, need)); */
      if (G_spLimit + need >= sp){
        /* we need more stack! */
        PRE_SWITCH();
        proc_resizeStack(need);
        POST_SWITCH();
      }
      /* push new frame and go */
      PUSH_FRAME();
      NODE_SET_FLAGS(nodeptr, N_HOLE);

      vapptr = nodeptr;
      ip = finfo->code;
      constptr = finfo->constTable;
      Break;
    }
  }

/*-------------------------------------------------------------------------*/
Case(TABLE_SWITCH) {
    Int size = I_ARG_US;
    CInfo* cinfo;
    Int tag;
    UInt16* jTable;

    nodeptr = sp[0];
    REMOVE_IND(nodeptr, Node*);
    sp[0] = nodeptr;
    cinfo = (CInfo*)NODE_INFO(nodeptr);
    ASSERT(cinfo->info.tag == I_CINFO);
    tag = cinfo->number;

    jTable = (UInt16*)(ip+2);
    ASSERT(tag >= 0 && tag < size);
    ip += jTable[tag];
    Break;
}

/*-------------------------------------------------------------------------*/
Case(LOOKUP_SWITCH)
    Int size = I_ARG_US;
    Int def = I_ARG_US3;
    CInfo* cinfo;
    Int tag;

    LookupSwElem* elems;
    LookupSwElem* end;

    elems = (LookupSwElem*)(ip + 4);
    end   = elems + size;

    nodeptr = sp[0];
    REMOVE_IND(nodeptr, Node*);
    sp[0] = nodeptr;
    cinfo = (CInfo*)NODE_INFO(nodeptr);
    ASSERT(cinfo->info.tag == I_CINFO);
    tag = cinfo->number;

    while (elems < end){
      if (elems->tag == tag){
        ip += elems->jump;
        goto I_LookupExit;
      }
      elems++;
    }
    ip += def;
  I_LookupExit:
    Break;

/*-------------------------------------------------------------------------*/
Case(INT_SWITCH)
    Int size = I_ARG_US;
    Int def = I_ARG_US3;
    INode* node;
    Int tag;

    IntSwElem* elems;
    IntSwElem* end;

    elems = (IntSwElem*)(ip + 4);
    end   = elems + size;

    node = (INode*)sp[0];
    REMOVE_IND(node, INode*);
    sp[0] = (Node*)node;
    tag = node->value;

    while (elems < end){
      if (elems->tag == tag){
        ip += elems->jump;
        goto I_IntExit;
      }
      elems++;
    }
    ip += def;
  I_IntExit:
    Break;

/*-------------------------------------------------------------------------*/
Case(STRING)
    Node* ap;
    INode* chr;
    Node* con;
    StringNode* p;
    StringNode* q;

    p = (StringNode*)*sp++;
    REMOVE_IND(p, StringNode*);
    /*        ASSERT(NODE_INFO(p) == (Info*)&G_infoString); */

    if (!*p->string){
      *--sp = G_nodeNil;
      Break;
    }
    MAKE_CHAR(chr,*p->string);
    MAKE_STRING(q,p->string+1);
    MAKE_AP1(ap,&G_infoPrimCString->papTable[1],(Node*)q);
    MAKE_CONS(con,(Node*)chr,ap);
    *--sp = con;
    Break;

/*-------------------------------------------------------------------------*/
Case(JUMP_FALSE)
    nodeptr = *sp++;
    REMOVE_IND(nodeptr, Node*);
    if (nodeptr == G_nodeFalse){
      ip += I_ARG_US;
    }else{
      ASSERT(nodeptr == G_nodeTrue);
      ip += 2;
    }
    Break;

/*-------------------------------------------------------------------------*/
Case(JUMP) ip += I_ARG_US; Break;

/*-------------------------------------------------------------------------*/
Case(PRIMITIVE)
    XInfo* xinfo = (XInfo*)constptr[0];

    ASSERT(xinfo->info.tag == I_XINFO);
    ASSERT(xinfo->ffiFunc);

    ip--;  /* move IP back to this instruction in case we need to rerun */

    PRE_SWITCH();
    nodeptr = hsffi_call(xinfo->ffiFunc, vapptr);
    POST_SWITCH();
    if (nodeptr){
      /* if nodeptr was not NULL then the primitive action completed successfully */
      *--sp = nodeptr;
      ip++; /* okay it's safe to continue */
    }
    Break;

/*-------------------------------------------------------------------------*/
BINOP2_W(ADD_W, +);
BINOP2_W(SUB_W, -);
BINOP2_W(MUL_W, *);
BINOP2_W(DIV_W, /);
BINOP2_W(MOD_W, %);
BINOP1_W(NEG_W, -);

CMPOP2_W(EQ_W, ==);
CMPOP2_W(NE_W, !=);
CMPOP2_W(LE_W, <=);
CMPOP2_W(LT_W, <);
CMPOP2_W(GE_W, >=);
CMPOP2_W(GT_W, >);

BINOP2_F(ADD_F, +);
BINOP2_F(SUB_F, -);
BINOP2_F(MUL_F, *);
BINOP2_F(DIV_F, /);
BINOP1_F(NEG_F, -);

CMPOP2_F(EQ_F, ==);
CMPOP2_F(NE_F, !=);
CMPOP2_F(LE_F, <=);
CMPOP2_F(LT_F, <);
CMPOP2_F(GE_F, >=);
CMPOP2_F(GT_F, >);

BINOP2_D(ADD_D, +);
BINOP2_D(SUB_D, -);
BINOP2_D(MUL_D, *);
BINOP2_D(DIV_D, /);
BINOP1_D(NEG_D, -);

CMPOP2_D(EQ_D, ==);
CMPOP2_D(NE_D, !=);
CMPOP2_D(LE_D, <=);
CMPOP2_D(LT_D, <);
CMPOP2_D(GE_D, >=);
CMPOP2_D(GT_D, >);

/*-------------------------------------------------------------------------*/
Case(FROM_ENUM){
  Node* con = (Node*)*sp++;
  INode* in;
  CInfo* cinfo;

  REMOVE_IND(con, Node*);
  cinfo = (CInfo*)NODE_INFO(con);
  ASSERT(cinfo->info.tag == I_CINFO);

  MAKE_INT(in,cinfo->number);
  *--sp = (Node*)in;
  Break;
}

/*-------------------------------------------------------------------------*/
Case(CATCH_BEGIN){
    ExceptionHandlerNode* excep = (ExceptionHandlerNode*)hp;
    MAKE_NODE(excep, &G_infoExceptionHandler, N_NORMAL);
    excep->next = G_proc->stack->exceptionStack;
    excep->ip = ip + I_ARG_UB;
    excep->vapptr = vapptr;
    excep->fpOffs = G_spBase - (Node**)fp;
    excep->spOffs = G_spBase - sp;
    hp += wordsof(ExceptionHandlerNode);
    G_proc->stack->exceptionStack = excep;
    *--sp = (Node*)excep;
    ip++;
    Break;
  }

Case(CATCH_END){
    nodeptr = *sp++;
    ASSERT(G_proc->stack->exceptionStack != NULL);
    G_proc->stack->exceptionStack = G_proc->stack->exceptionStack->next;
    *sp = nodeptr;
    Break;
  }

Case (THROW)
I_Throw:{
    ExceptionHandlerNode* excep = G_proc->stack->exceptionStack;
    FInfo* finfo;

    /* are exceptions blocked? */
    if (G_excepBlocked){
        ip--;               /* allow this instruction to be run again */

        PRE_SWITCH();
        proc_blockOnExcep(G_proc);
        proc_switch(PM_BLOCKED);
        POST_SWITCH();
        Break;
    }

    SHOW(printf("Throwing exception to ip = %p\n", excep->ip));
    nodeptr = *sp++;

    sp = G_spBase - excep->spOffs;
    fp = (Frame*)(G_spBase - excep->fpOffs);
    ip = excep->ip;
    vapptr = excep->vapptr;
    finfo = PINFO_FINFO(NODE_INFO(excep->vapptr));
    constptr = finfo->constTable;
    G_proc->stack->exceptionStack = excep->next;
    *--sp = nodeptr;

    /** FIXME: what about 'block' and disabling exceptions? or can it just be done in Haskell? */
    Break;
  }

Case (THROW_TO){
    INode* nThreadId;
    ProcessID id;
    ExceptionHandlerNode* excep;
    Process* proc;
    Node** base;

    /* get the process id */
    nThreadId = (INode*)sp[0];
    REMOVE_IND(nThreadId, INode*);
    id = (ProcessID)nThreadId->value;
    if (G_proc->id == id){
        /* if we are throwing to ourselves then use the standard 'THROW' instruction */
        sp++;
        goto I_Throw;
    }

    /* find the right process */
    proc = proc_findByID(nThreadId->value);
    if (!proc){
        /* must be dead already */
        sp += 2;
        Break;
    }

    /* are exceptions blocked? */
    if (G_excepBlocked /* && !proc->isInterruptible */){ /* FIXME: interruptible ... */
        ip--;               /* allow this instruction to be run again */

        PRE_SWITCH();
        proc_blockOnExcep(G_proc);
        proc_switch(PM_BLOCKED);
        POST_SWITCH();
        Break;
    }

    /* extract the exception object from the stack */
    nodeptr = sp[1];

    /* get the exception stack for the process */
    excep = proc->stack->exceptionStack;
    if ((Node*)excep == G_nodeUnit){
        /* then it doesn't have an exception stack, so just kill it */
        sp += 2;
        proc_kill(proc);
        Break;
    }

    /* force the proc to be ready if possible */
    if (proc_forceReady(proc)){
        sp += 2;               /* remove thread id and exception object */

        /* switch to the new process */
        PRE_SWITCH();
        proc_forceSwitchTo(proc, PM_READY);
        POST_SWITCH();

        /* and call the throwing code */
        *--sp = nodeptr;
        goto I_Throw;
    }else{
        /* we've blocked waiting on the process to finish some ffi */
        ip--;   /* go back one instruction, so this code gets run again */

        /* switch to another process */
        PRE_SWITCH();
        proc_switch(PM_THROWING);
        POST_SWITCH();
    }
    Break;
  }

/****************************** TRACING ************************************/
/* Tracing */
/*-------------------------------------------------------------------------*/
#ifdef HAT

Case(TAP_P1) i = I_ARG_UB; ip += 1;
  I_TAp: {
    PInfo* pinfo;
    HStackEntry offs;
    HPos* pos;

    nodeptr = *sp;
    pos = (HPos*)constptr[i];
    pinfo = (PInfo*)NODE_INFO(nodeptr);
    offs = hgm_traceAp(nodeptr, pos, G_tp);
    G_tp += pinfo->size;
    *--G_tp = offs;
    Break;
  }
Case(TAP_P2) i = I_ARG_US; ip += 2; goto I_TAp;

/*-------------------------------------------------------------------------*/
Case(TCON_P1) i = I_ARG_UB; ip += 1;
  I_TCon: {
    CInfo* cinfo;
    HStackEntry offs;
    HPos* pos;

    nodeptr = *sp;
    pos = (HPos*)constptr[i];
    cinfo = (CInfo*)NODE_INFO(nodeptr);
    offs = hgm_traceCon(nodeptr, pos, G_tp);
    G_tp += cinfo->size;
    *--G_tp = offs;
    Break;
  }
Case(TCON_P2) i = I_ARG_US; ip += 2; goto I_TCon;

/*-------------------------------------------------------------------------*/
Case(TPRIMCON_P1) i = I_ARG_UB; ip += 1;
  I_TPrimCon: {
    HStackEntry offs;
    HPos* pos;

    nodeptr = *sp;
    pos = (HPos*)constptr[i];
    offs = hgm_traceCon(nodeptr, pos, G_tp);
    *--G_tp = offs;
    Break;
  }
Case(TPRIMCON_P2) i = I_ARG_US; ip += 2; goto I_TPrimCon;

/*-------------------------------------------------------------------------*/
Case(TAPPLY_P1) i = I_ARG_UB; n = I_ARG_UB2; ip += 2;
  I_TApply: {
    HStackEntry f, offs;
    HPos* pos;

    f = *G_tp++;
    pos = (HPos*)constptr[i];
    nodeptr = *sp;
    offs = hgm_traceApply(nodeptr, f, n, pos, G_tp);
    G_tp += n;
    *--G_tp = offs;
    Break;
  }
Case(TAPPLY_P2) i = I_ARG_US; n = I_ARG_UB3; ip += 3; goto I_TApply;

/*-------------------------------------------------------------------------*/
#define T_CASE_INS(_name, _fun) \
  Case(_name##_P1) i = I_ARG_UB; ip += 1; \
  I_##_name: { \
    HPos* pos; \
    HStackEntry exp; \
    \
    pos = (HPos*)constptr[i]; \
    exp = *G_tp++; \
    *--G_tp = _fun(exp, pos); \
    Break; \
  } \
  Case(_name##_P2) i = I_ARG_US; ip += 2; goto I_##_name

/*-------------------------------------------------------------------------*/
T_CASE_INS(TIF, hgm_traceIf);
T_CASE_INS(TGUARD, hgm_traceGuard);
T_CASE_INS(TCASE, hgm_traceCase);

/*-------------------------------------------------------------------------*/
Case(TPRIMAP_P1) i = I_ARG_UB; ip += 1;
  I_TPrimAp: {
    PInfo* pinfo;
    FInfo* finfo;
    HStackEntry offs;
    HPos* pos;
    Node* fake;

    printf("TPRIMAP!\n");
    fake = *sp++;                                         /* fake node on top gives info */
    pos = (HPos*)constptr[i];
    pinfo = (PInfo*)NODE_INFO(fake);                      /* fiddle around with the pinfo */
    finfo = PINFO_FINFO(pinfo);                           /* we want the saturated one */
    pinfo = &finfo->papTable[finfo->arity];
    offs = hgm_tracePrimAp(pos, G_tp, pinfo);
    G_tp += pinfo->size;
    *--G_tp = offs;
    Break;
  }
Case(TPRIMAP_P2) i = I_ARG_US; ip += 2; goto I_TPrimAp;

/*-------------------------------------------------------------------------*/
Case(TPRIMRESULT_P1) i = I_ARG_UB; ip += 1;
  I_TPrimResult: {
    HPos* pos;
    HStackEntry upd;
    HStackEntry conTrace;

    nodeptr = *sp;                                        /* constructor result */
    pos = (HPos*)constptr[i];                             /* src pos for constructor */
    upd = *G_tp;                                          /* trace node to update */
    conTrace = hgm_traceCon(nodeptr, pos, G_tp);          /* trace the constructor */
    hgm_primResult(upd, conTrace);                        /* give the ap its result */
    Break;
  }
Case(TPRIMRESULT_P2) i = I_ARG_US; ip += 2; goto I_TPrimResult;

/*-------------------------------------------------------------------------*/
Case(TRETURN) abort(); Break;

/*-------------------------------------------------------------------------*/
Case(TPUSH)
  *--G_tp = hgm_traceNode(*sp, false);
  Break;

/*-------------------------------------------------------------------------*/
Case(TPUSHVAR_P1) i = I_ARG_UB; ip += 1;
  I_TPushVar: {
    HVarDesc* desc;

    desc = (HVarDesc*)constptr[i];
    *--G_tp = hgm_traceVar(*sp, desc);
    Break;
  }
Case(TPUSHVAR_P2) i = I_ARG_US; ip += 2; goto I_TPushVar;

/*-------------------------------------------------------------------------*/
Case(TPROJECT_P1) i = I_ARG_UB; ip += 1;
  I_TProject: {
    HPos* pos;
    HStackEntry exp;

    exp = *G_tp;
    pos = (HPos*)constptr[i];
    *G_tp = hgm_traceProjection(pos, exp);
    Break;
  }
Case(TPROJECT_P2) i = I_ARG_US; ip += 2; goto I_TProject;

#else

Case(TAP_P1) goto Err_NoHat;
Case(TAP_P2) goto Err_NoHat;
Case(TCON_P1) goto Err_NoHat;
Case(TCON_P2) goto Err_NoHat;
Case(TPRIMCON_P1) goto Err_NoHat;
Case(TPRIMCON_P2) goto Err_NoHat;
Case(TAPPLY_P1) goto Err_NoHat;
Case(TAPPLY_P2) goto Err_NoHat;
Case(TIF_P1) goto Err_NoHat;
Case(TIF_P2) goto Err_NoHat;
Case(TGUARD_P1) goto Err_NoHat;
Case(TGUARD_P2) goto Err_NoHat;
Case(TCASE_P1) goto Err_NoHat;
Case(TCASE_P2) goto Err_NoHat;
Case(TPRIMAP_P1) goto Err_NoHat;
Case(TPRIMAP_P2) goto Err_NoHat;
Case(TPRIMRESULT_P1) goto Err_NoHat;
Case(TPRIMRESULT_P2) goto Err_NoHat;
Case(TRETURN) abort(); Break;
Case(TPUSH) goto Err_NoHat;
Case(TPUSHVAR_P1) goto Err_NoHat;
Case(TPUSHVAR_P2) goto Err_NoHat;
Case(TPROJECT_P1) goto Err_NoHat;
Case(TPROJECT_P2) goto Err_NoHat;

Err_NoHat:
  fprintf(stderr, "Error: executed code from a module which was compiled with hat support,\n"
                  "but this version of the interpretter was compiled without hat support\n");
  exit(1);

#endif
/* #endif */

/****************************** UNUSED* ************************************/
Case(END_CODE) abort(); Break;
Case(MOD_F) abort(); Break;
Case(MOD_D) abort(); Break;
Case(NEED_STACK_16) abort();
Case(NEED_STACK) abort();
Case(EXTERNAL) abort();

