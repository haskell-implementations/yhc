module TDependancy
  (gdeBindersOf,gdeValuesOf,gdeFreeVars,gdeFreeVarsOf,gdeDepthFirstSearch
    ,gdeSpanningSearch,gdeScc,gdeDependancy,gdeDepends,gdeElet) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 

gdeBindersOf ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List (T.Tuple2 a b)) (T.List a))

gdeBindersOf pdeBindersOf p =
  T.fun1 adeBindersOf pdeBindersOf p hdeBindersOf
  where
  
  hdeBindersOf fdefns p =
    T.ap1 p0v0 p
      (T.ap2 p14v22 p (TPrelude.g_foldr p14v22 p)
        (T.fun2 T.mkLambda p14v22 p
          (\ f_x f_y p ->
            T.ccase p0v0 p
              (let
                v0v0v1 (T.R (T.Tuple2 fname frhs) _) p =
                  T.ap1 p14v22 p (T.pa1 T.Cons T.cn1 p14v22 p T.aCons fname) f_y
                v0v0v1 _ p = T.projection p14v22 p f_y in (v0v0v1)) f_x))
        fdefns) (T.fromExpList p0v0 p [])
  

gdeValuesOf ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List (T.Tuple2 a b)) (T.List b))

gdeValuesOf pdeValuesOf p =
  T.fun1 adeValuesOf pdeValuesOf p hdeValuesOf
  where
  
  hdeValuesOf fdefns p =
    T.ap1 p0v0 p
      (T.ap2 p21v22 p (TPrelude.g_foldr p21v22 p)
        (T.fun2 T.mkLambda p21v22 p
          (\ f_x f_y p ->
            T.ccase p0v0 p
              (let
                v0v0v1 (T.R (T.Tuple2 fname frhs) _) p =
                  T.ap1 p21v22 p (T.pa1 T.Cons T.cn1 p21v22 p T.aCons frhs) f_y
                v0v0v1 _ p = T.projection p21v22 p f_y in (v0v0v1)) f_x))
        fdefns) (T.fromExpList p0v0 p [])
  

gdeFreeVars ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun CExpr (AnnExpr Naam (Set Naam)))

gdeFreeVars pdeFreeVars p =
  T.fun1 adeFreeVars pdeFreeVars p hdeFreeVars
  where
  
  hdeFreeVars (T.R (ENum fk) _) p =
    T.con2 p28v28 p T.Tuple2 T.aTuple2 (gutSetEmpty p28v29 p)
      (T.con1 p28v41 p ANum aANum fk)
  hdeFreeVars (T.R (EVar fv) _) p =
    T.con2 p29v28 p T.Tuple2 T.aTuple2
      (T.ap1 p29v29 p (gutSetSingleton p29v29 p) fv)
      (T.con1 p29v47 p AVar aAVar fv)
  hdeFreeVars (T.R (EConstr fn) _) p =
    T.con2 p30v28 p T.Tuple2 T.aTuple2 (gutSetEmpty p30v29 p)
      (T.con1 p30v41 p AConstr aAConstr fn)
  hdeFreeVars (T.R (EAp fe1 fe2) _) p =
    T.con2 p33v6 p T.Tuple2 T.aTuple2
      (T.ap2 p33v7 p (gutSetUnion p33v7 p)
        (T.ap1 p33v19 p (gdeFreeVarsOf p33v19 p) (ge1' p33v32 p))
        (T.ap1 p33v38 p (gdeFreeVarsOf p33v38 p) (ge2' p33v51 p)))
      (T.con2 p33v57 p AAp aAAp (ge1' p33v61 p) (ge2' p33v65 p))
    where
    
    ge1' pe1' p = T.constUse pe1' p se1'
    
    se1' =
      T.constDef p a34v12e1' (\ p -> T.ap1 p34v29 p (gdeFreeVars p34v29 p) fe1)
    
    ge2' pe2' p = T.constUse pe2' p se2'
    
    se2' =
      T.constDef p a35v12e2' (\ p -> T.ap1 p35v29 p (gdeFreeVars p35v29 p) fe2)
    
  hdeFreeVars (T.R (ECase fe falts) _) p =
    T.con2 p38v6 p T.Tuple2 T.aTuple2
      (T.ap2 p38v7 p (gutSetUnion p38v7 p)
        (T.ap1 p38v19 p (gdeFreeVarsOf p38v19 p) (ge' p38v32 p))
        (gfree p38v36 p))
      (T.con2 p38v42 p ACase aACase (ge' p38v48 p) (galts' p38v51 p))
    where
    
    ge' pe' p = T.constUse pe' p se'
    
    se' =
      T.constDef p a39v12e' (\ p -> T.ap1 p39v29 p (gdeFreeVars p39v29 p) fe)
    
    galts' palts' p = T.constUse palts' p salts'
    
    salts' =
      T.constDef p a40v12alts'
        (\ p ->
          T.ap1 p0v0 p
            (T.ap2 p40v29 p (TPrelude.g_foldr p40v29 p)
              (T.fun2 T.mkLambda p40v29 p
                (\ f_x f_y p ->
                  T.ccase p0v0 p
                    (let
                      v0v0v1 (T.R (T.Tuple2 ft (T.R (T.Tuple2 fns fe) _)) _) p =
                        T.ap1 p40v29 p
                          (T.pa1 T.Cons T.cn1 p40v29 p T.aCons
                            (T.con2 p40v30 p T.Tuple2 T.aTuple2 ft
                              (T.con2 p40v34 p T.Tuple2 T.aTuple2 fns
                                (T.ap1 p40v39 p (gdeFreeVars p40v39 p) fe))))
                          f_y
                      v0v0v1 _ p = T.projection p40v29 p f_y in (v0v0v1)) f_x))
              falts) (T.fromExpList p0v0 p []))
    
    gfree pfree p = T.constUse pfree p sfree
    
    sfree =
      T.constDef p a41v12free
        (\ p ->
          T.ap1 p41v29 p (gutSetUnionList p41v29 p)
            (T.ap2 p41v45 p (gmap p41v45 p) (gf p41v49 p) (galts' p41v51 p)))
    
    gf pf p =
      T.fun1 a42v12f pf p hf
      where
      
      hf (T.R (T.Tuple2 ft (T.R (T.Tuple2 fns fe) _)) _) p =
        T.ap2 p42v29 p (gutSetSubtraction p42v29 p)
          (T.ap1 p42v47 p (gdeFreeVarsOf p42v47 p) fe)
          (T.ap1 p42v64 p (gutSetFromList p42v64 p) fns)
      hf _ p = T.fatal p
      
    
  hdeFreeVars (T.R (ELam fargs fbody) _) p =
    T.con2 p45v6 p T.Tuple2 T.aTuple2
      (T.ap2 p45v7 p (gutSetSubtraction p45v7 p)
        (T.ap1 p45v25 p (gdeFreeVarsOf p45v25 p) (gbody' p45v38 p))
        (T.ap1 p45v46 p (gutSetFromList p45v46 p) fargs))
      (T.con2 p45v67 p ALam aALam fargs (gbody' p45v77 p))
    where
    
    gbody' pbody' p = T.constUse pbody' p sbody'
    
    sbody' =
      T.constDef p a46v12body'
        (\ p -> T.ap1 p46v29 p (gdeFreeVars p46v29 p) fbody)
    
  hdeFreeVars (T.R (ELet fisRec fdefns fbody) _) p =
    T.con2 p49v6 p T.Tuple2 T.aTuple2
      (T.ap2 p49v7 p (gutSetUnion p49v7 p) (gdefnsFree p49v18 p)
        (gbodyFree p49v28 p))
      (T.con3 p49v38 p ALet aALet fisRec (gdefns' p49v49 p) (gbody' p49v56 p))
    where
    
    gbinders pbinders p = T.constUse pbinders p sbinders
    
    sbinders =
      T.constDef p a50v12binders
        (\ p -> T.ap1 p50v29 p (gdeBindersOf p50v29 p) fdefns)
    
    gbinderSet pbinderSet p = T.constUse pbinderSet p sbinderSet
    
    sbinderSet =
      T.constDef p a51v12binderSet
        (\ p -> T.ap1 p51v29 p (gutSetFromList p51v29 p) (gbinders p51v43 p))
    
    gvalues' pvalues' p = T.constUse pvalues' p svalues'
    
    svalues' =
      T.constDef p a52v12values'
        (\ p ->
          T.ap2 p52v29 p (gmap p52v29 p) (gdeFreeVars p52v33 p)
            (T.ap1 p52v45 p (gdeValuesOf p52v45 p) fdefns))
    
    gdefns' pdefns' p = T.constUse pdefns' p sdefns'
    
    sdefns' =
      T.constDef p a53v12defns'
        (\ p ->
          T.ap2 p53v29 p (gzip p53v29 p) (gbinders p53v33 p)
            (gvalues' p53v41 p))
    
    gfreeInValues pfreeInValues p = T.constUse pfreeInValues p sfreeInValues
    
    sfreeInValues =
      T.constDef p a54v12freeInValues
        (\ p ->
          T.ap1 p54v29 p (gutSetUnionList p54v29 p)
            (T.ap2 p54v45 p (gmap p54v45 p) (gdeFreeVarsOf p54v49 p)
              (gvalues' p54v62 p)))
    
    gdefnsFree pdefnsFree p = T.constUse pdefnsFree p sdefnsFree
    
    sdefnsFree =
      T.constDef p a55v12defnsFree
        (\ p ->
          T.cguard p55v29 p fisRec
            (\ p ->
              T.ap2 p55v42 p (gutSetSubtraction p55v42 p)
                (gfreeInValues p55v59 p) (gbinderSet p55v72 p))
            (\ p ->
              T.cguard p56v29 p (gotherwise p56v29 p)
                (\ p -> gfreeInValues p56v42 p) (\ p -> T.fatal p)))
    
    gbody' pbody' p = T.constUse pbody' p sbody'
    
    sbody' =
      T.constDef p a57v12body'
        (\ p -> T.ap1 p57v29 p (gdeFreeVars p57v29 p) fbody)
    
    gbodyFree pbodyFree p = T.constUse pbodyFree p sbodyFree
    
    sbodyFree =
      T.constDef p a58v12bodyFree
        (\ p ->
          T.ap2 p58v29 p (gutSetSubtraction p58v29 p)
            (T.ap1 p58v47 p (gdeFreeVarsOf p58v47 p) (gbody' p58v60 p))
            (gbinderSet p58v67 p))
    
  hdeFreeVars _ p = T.fatal p
  

gdeFreeVarsOf ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (AnnExpr Naam (Set Naam)) (Set Naam))

gdeFreeVarsOf pdeFreeVarsOf p =
  T.fun1 adeFreeVarsOf pdeFreeVarsOf p hdeFreeVarsOf
  where
  
  hdeFreeVarsOf (T.R (T.Tuple2 ffree_vars fexpr) _) p =
    T.projection p65v34 p ffree_vars
  hdeFreeVarsOf _ p = T.fatal p
  

gdeDepthFirstSearch ::
  Ord a =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.List a))
          (T.Fun (T.Tuple2 (Set a) (T.List a))
            (T.Fun (T.List a) (T.Tuple2 (Set a) (T.List a)))))

sdeDepthFirstSearch ::
  Ord a =>
  T.R
    (T.Fun (T.Fun a (T.List a))
      (T.Fun (T.Tuple2 (Set a) (T.List a))
        (T.Fun (T.List a) (T.Tuple2 (Set a) (T.List a)))))

gdeDepthFirstSearch pdeDepthFirstSearch p =
  T.constUse pdeDepthFirstSearch p sdeDepthFirstSearch

sdeDepthFirstSearch =
  T.constDef T.mkRoot adeDepthFirstSearch
    (\ p ->
      let
        gsearch psearch p =
          T.fun3 a81v6search psearch p hsearch
          where
          
          hsearch frelation (T.R (T.Tuple2 fvisited fsequence) _) fvertex p =
            T.cguard p82v9 p
              (T.ap2 p82v9 p (gutSetElementOf p82v9 p) fvertex fvisited)
              (\ p -> T.con2 p82v43 p T.Tuple2 T.aTuple2 fvisited fsequence)
              (\ p ->
                T.cguard p83v9 p (gotherwise p83v9 p)
                  (\ p ->
                    T.con2 p83v43 p T.Tuple2 T.aTuple2 (gvisited' p83v44 p)
                      (T.con2 p83v60 p T.Cons T.aCons fvertex
                        (gsequence' p83v62 p))) (\ p -> T.fatal p))
            where
            
            gvisited' pvisited' p = T.constUse pvisited' p svisited'
            
            gsequence' pvisited' p = T.constUse pvisited' p ssequence'
            
            j85v9visited' =
              case
                T.ap3 p86v12 p (gdeDepthFirstSearch p86v12 p) frelation
                  (T.con2 p87v28 p T.Tuple2 T.aTuple2
                    (T.ap2 p87v29 p (gutSetUnion p87v29 p) fvisited
                      (T.ap1 p87v49 p (gutSetSingleton p87v49 p) fvertex))
                    fsequence) (T.ap1 p88v29 p frelation fvertex) of
                T.R (T.Tuple2 fvisited' fsequence') kvisited' ->
                  (kvisited',fvisited',fsequence')
                _ -> T.fatal p
            
            svisited' =
              T.constDef p a85v10visited'
                (\ _ ->
                  case j85v9visited' of
                    (kvisited',fvisited',fsequence') ->
                      T.projection p85v10 kvisited' fvisited')
            
            ssequence' =
              T.constDef p a85v20sequence'
                (\ _ ->
                  case j85v9visited' of
                    (kvisited',fvisited',fsequence') ->
                      T.projection p85v20 kvisited' fsequence')
            
          hsearch _ _ _ p = T.fatal p
           in
        (T.ap2 p79v12 p (p79v12 !. p) (gfoldl p79v6 p) (gsearch p79v14 p)))

gdeSpanningSearch ::
  Ord a =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.List a))
          (T.Fun (T.Tuple2 (Set a) (T.List (Set a)))
            (T.Fun (T.List a) (T.Tuple2 (Set a) (T.List (Set a))))))

sdeSpanningSearch ::
  Ord a =>
  T.R
    (T.Fun (T.Fun a (T.List a))
      (T.Fun (T.Tuple2 (Set a) (T.List (Set a)))
        (T.Fun (T.List a) (T.Tuple2 (Set a) (T.List (Set a))))))

gdeSpanningSearch pdeSpanningSearch p =
  T.constUse pdeSpanningSearch p sdeSpanningSearch

sdeSpanningSearch =
  T.constDef T.mkRoot adeSpanningSearch
    (\ p ->
      let
        gsearch psearch p =
          T.fun3 a104v6search psearch p hsearch
          where
          
          hsearch frelation (T.R (T.Tuple2 fvisited futSetSequence) _) fvertex
            p =
            T.cguard p105v9 p
              (T.ap2 p105v9 p (gutSetElementOf p105v9 p) fvertex fvisited)
              (\ p ->
                T.con2 p105v43 p T.Tuple2 T.aTuple2 fvisited futSetSequence)
              (\ p ->
                T.cguard p106v9 p (gotherwise p106v9 p)
                  (\ p ->
                    T.con2 p106v21 p T.Tuple2 T.aTuple2 (gvisited' p106v22 p)
                      (T.con2 p106v64 p T.Cons T.aCons
                        (T.ap1 p106v32 p (gutSetFromList p106v32 p)
                          (T.con2 p106v53 p T.Cons T.aCons fvertex
                            (gsequence p106v55 p))) futSetSequence))
                  (\ p -> T.fatal p))
            where
            
            gvisited' pvisited' p = T.constUse pvisited' p svisited'
            
            gsequence pvisited' p = T.constUse pvisited' p ssequence
            
            j108v10visited' =
              case
                T.ap3 p109v15 p (gdeDepthFirstSearch p109v15 p) frelation
                  (T.con2 p110v27 p T.Tuple2 T.aTuple2
                    (T.ap2 p110v28 p (gutSetUnion p110v28 p) fvisited
                      (T.ap1 p110v48 p (gutSetSingleton p110v48 p) fvertex))
                    (T.con0 p110v72 p T.List T.aList))
                  (T.ap1 p111v28 p frelation fvertex) of
                T.R (T.Tuple2 fvisited' fsequence) kvisited' ->
                  (kvisited',fvisited',fsequence)
                _ -> T.fatal p
            
            svisited' =
              T.constDef p a108v11visited'
                (\ _ ->
                  case j108v10visited' of
                    (kvisited',fvisited',fsequence) ->
                      T.projection p108v11 kvisited' fvisited')
            
            ssequence =
              T.constDef p a108v21sequence
                (\ _ ->
                  case j108v10visited' of
                    (kvisited',fvisited',fsequence) ->
                      T.projection p108v21 kvisited' fsequence)
            
          hsearch _ _ _ p = T.fatal p
           in
        (T.ap2 p102v12 p (p102v12 !. p) (gfoldl p102v6 p) (gsearch p102v14 p)))

gdeScc ::
  Ord a =>
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.List a))
          (T.Fun (T.Fun a (T.List a)) (T.Fun (T.List a) (T.List (Set a)))))

gdeScc pdeScc p =
  T.fun2 adeScc pdeScc p hdeScc
  where
  
  hdeScc fins fouts p =
    T.ap2 p124v15 p (p124v15 !. p) (gspanning p124v6 p) (gdepthFirst p124v17 p)
    where
    
    gdepthFirst pdepthFirst p = T.constUse pdepthFirst p sdepthFirst
    
    sdepthFirst =
      T.constDef p a125v12depthFirst
        (\ p ->
          T.ap2 p125v32 p (p125v32 !. p) (gsecond p125v25 p)
            (T.ap2 p125v34 p (gdeDepthFirstSearch p125v34 p) fouts
              (T.con2 p125v58 p T.Tuple2 T.aTuple2 (gutSetEmpty p125v59 p)
                (T.con0 p125v71 p T.List T.aList))))
    
    gspanning pspanning p = T.constUse pspanning p sspanning
    
    sspanning =
      T.constDef p a126v12spanning
        (\ p ->
          T.ap2 p126v32 p (p126v32 !. p) (gsecond p126v25 p)
            (T.ap2 p126v34 p (gdeSpanningSearch p126v34 p) fins
              (T.con2 p126v58 p T.Tuple2 T.aTuple2 (gutSetEmpty p126v59 p)
                (T.con0 p126v71 p T.List T.aList))))
    
  

gdeDependancy ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (CExprP Naam) (CExprP Naam))

sdeDependancy :: T.R (T.Fun (CExprP Naam) (CExprP Naam))

gdeDependancy pdeDependancy p = T.constUse pdeDependancy p sdeDependancy

sdeDependancy =
  T.constDef T.mkRoot adeDependancy
    (\ p ->
      T.ap2 p134v26 p (p134v26 !. p) (gdeDepends p134v16 p)
        (gdeFreeVars p134v28 p))

gdeDepends pdeDepends p =
  T.fun1 adeDepends pdeDepends p hdeDepends
  where
  
  hdeDepends (T.R (T.Tuple2 ffree (T.R (ANum fn) _)) _) p =
    T.con1 p140v37 p ENum aENum fn
  hdeDepends (T.R (T.Tuple2 ffree (T.R (AConstr fn) _)) _) p =
    T.con1 p141v37 p EConstr aEConstr fn
  hdeDepends (T.R (T.Tuple2 ffree (T.R (AVar fv) _)) _) p =
    T.con1 p142v37 p EVar aEVar fv
  hdeDepends (T.R (T.Tuple2 ffree (T.R (AAp fe1 fe2) _)) _) p =
    T.con2 p143v37 p EAp aEAp (T.ap1 p143v42 p (gdeDepends p143v42 p) fe1)
      (T.ap1 p143v57 p (gdeDepends p143v57 p) fe2)
  hdeDepends (T.R (T.Tuple2 ffree (T.R (ACase fbody falts) _)) _) p =
    T.con2 p145v37 p ECase aECase (T.ap1 p145v44 p (gdeDepends p145v44 p) fbody)
      (T.ap1 p0v0 p
        (T.ap2 p146v41 p (TPrelude.g_foldr p146v41 p)
          (T.fun2 T.mkLambda p146v41 p
            (\ f_x f_y p ->
              T.ccase p0v0 p
                (let
                  v0v0v1 (T.R (T.Tuple2 ft (T.R (T.Tuple2 fns fe) _)) _) p =
                    T.ap1 p146v41 p
                      (T.pa1 T.Cons T.cn1 p146v41 p T.aCons
                        (T.con2 p146v42 p T.Tuple2 T.aTuple2 ft
                          (T.con2 p146v46 p T.Tuple2 T.aTuple2 fns
                            (T.ap1 p146v51 p (gdeDepends p146v51 p) fe)))) f_y
                  v0v0v1 _ p = T.projection p146v41 p f_y in (v0v0v1)) f_x))
          falts) (T.fromExpList p0v0 p []))
  hdeDepends (T.R (T.Tuple2 ffree (T.R (ALam fns fbody) _)) _) p =
    T.con2 p149v37 p ELam aELam fns
      (T.ap1 p149v46 p (gdeDepends p149v46 p) fbody)
  hdeDepends (T.R (T.Tuple2 ffree (T.R (ALet fisRec fdefns fbody) _)) _) p =
    T.ap3 p152v6 p (gfoldr p152v6 p)
      (T.ap1 p152v13 p (gdeElet p152v13 p) fisRec)
      (T.ap1 p152v28 p (gdeDepends p152v28 p) fbody) (gdefnGroups p152v44 p)
    where
    
    gbinders pbinders p = T.constUse pbinders p sbinders
    
    sbinders =
      T.constDef p a154v6binders
        (\ p -> T.ap1 p154v19 p (gdeBindersOf p154v19 p) fdefns)
    
    gbinderSet pbinderSet p = T.constUse pbinderSet p sbinderSet
    
    sbinderSet =
      T.constDef p a155v6binderSet
        (\ p ->
          T.cguard p155v19 p fisRec
            (\ p ->
              T.ap1 p155v32 p (gutSetFromList p155v32 p) (gbinders p155v46 p))
            (\ p ->
              T.cguard p156v19 p (gotherwise p156v19 p)
                (\ p -> gutSetEmpty p156v32 p) (\ p -> T.fatal p)))
    
    gedges pedges p = T.constUse pedges p sedges
    
    sedges =
      T.constDef p a157v6edges
        (\ p ->
          T.ap1 p0v0 p
            (T.ap2 p157v19 p (TPrelude.g_foldr p157v19 p)
              (T.fun2 T.mkLambda p157v19 p
                (\ f_x f_y p ->
                  T.ccase p0v0 p
                    (let
                      v0v0v1 (T.R (T.Tuple2 fn (T.R (T.Tuple2 ffree fe) _)) _)
                        p =
                        T.ap1 p157v19 p
                          (T.ap2 p157v19 p (TPrelude.g_foldr p157v19 p)
                            (T.fun2 T.mkLambda p157v19 p
                              (\ f_x f_y p ->
                                T.ccase p0v0 p
                                  (let
                                    v0v0v1 ff p =
                                      T.ap1 p157v19 p
                                        (T.pa1 T.Cons T.cn1 p157v19 p T.aCons
                                          (T.con2 p157v20 p T.Tuple2 T.aTuple2
                                            fn ff)) f_y
                                    v0v0v1 _ p = T.projection p157v19 p f_y in
                                    (v0v0v1)) f_x))
                            (T.ap1 p158v34 p (gutSetToList p158v34 p)
                              (T.ap2 p158v47 p (gutSetIntersection p158v47 p)
                                ffree (gbinderSet p158v70 p)))) f_y
                      v0v0v1 _ p = T.projection p157v19 p f_y in (v0v0v1)) f_x))
              fdefns) (T.fromExpList p0v0 p []))
    
    gins pins p =
      T.fun1 a159v6ins pins p hins
      where
      
      hins fv p =
        T.ap1 p0v0 p
          (T.ap2 p159v19 p (TPrelude.g_foldr p159v19 p)
            (T.fun2 T.mkLambda p159v19 p
              (\ f_x f_y p ->
                T.ccase p0v0 p
                  (let
                    v0v0v1 (T.R (T.Tuple2 fu fw) _) p =
                      T.ap1 p159v19 p
                        (T.ap2 p159v19 p (TPrelude.g_filter p159v19 p)
                          (T.ap2 p159v41 p (p159v41 !== p) fv fw)
                          (T.pa1 T.Cons T.cn1 p159v19 p T.aCons fu)) f_y
                    v0v0v1 _ p = T.projection p159v19 p f_y in (v0v0v1)) f_x))
            (gedges p159v33 p)) (T.fromExpList p0v0 p [])
      
    
    gouts pouts p =
      T.fun1 a160v6outs pouts p houts
      where
      
      houts fv p =
        T.ap1 p0v0 p
          (T.ap2 p160v19 p (TPrelude.g_foldr p160v19 p)
            (T.fun2 T.mkLambda p160v19 p
              (\ f_x f_y p ->
                T.ccase p0v0 p
                  (let
                    v0v0v1 (T.R (T.Tuple2 fu fw) _) p =
                      T.ap1 p160v19 p
                        (T.ap2 p160v19 p (TPrelude.g_filter p160v19 p)
                          (T.ap2 p160v41 p (p160v41 !== p) fv fu)
                          (T.pa1 T.Cons T.cn1 p160v19 p T.aCons fw)) f_y
                    v0v0v1 _ p = T.projection p160v19 p f_y in (v0v0v1)) f_x))
            (gedges p160v33 p)) (T.fromExpList p0v0 p [])
      
    
    gcomponents pcomponents p = T.constUse pcomponents p scomponents
    
    scomponents =
      T.constDef p a161v6components
        (\ p ->
          T.ap2 p161v19 p (gmap p161v19 p) (gutSetToList p161v23 p)
            (T.ap3 p161v36 p (gdeScc p161v36 p) (gins p161v42 p)
              (gouts p161v46 p) (gbinders p161v51 p)))
    
    gdefnGroups pdefnGroups p = T.constUse pdefnGroups p sdefnGroups
    
    sdefnGroups =
      T.constDef p a162v6defnGroups
        (\ p ->
          T.ap1 p0v0 p
            (T.ap2 p162v19 p (TPrelude.g_foldr p162v19 p)
              (T.fun2 T.mkLambda p162v19 p
                (\ f_x f_y p ->
                  T.ccase p0v0 p
                    (let
                      v0v0v1 fns p =
                        T.ap1 p162v19 p
                          (T.pa1 T.Cons T.cn1 p162v19 p T.aCons
                            (T.ap1 p0v0 p
                              (T.ap2 p162v20 p (TPrelude.g_foldr p162v20 p)
                                (T.fun2 T.mkLambda p162v20 p
                                  (\ f_x f_y p ->
                                    T.ccase p0v0 p
                                      (let
                                        v0v0v1 fn p =
                                          T.ap1 p162v20 p
                                            (T.pa1 T.Cons T.cn1 p162v20 p
                                              T.aCons
                                              (T.con2 p162v21 p T.Tuple2
                                                T.aTuple2 fn
                                                (T.ap3 p162v25 p
                                                  (gutSureLookup p162v25 p)
                                                  fdefns
                                                  (T.fromLitString p162v44 p
                                                    "depends`defnGroups") fn)))
                                            f_y
                                        v0v0v1 _ p = T.projection p162v20 p f_y
                                        in (v0v0v1)) f_x)) fns)
                              (T.fromExpList p0v0 p []))) f_y
                      v0v0v1 _ p = T.projection p162v19 p f_y in (v0v0v1)) f_x))
              (gcomponents p163v61 p)) (T.fromExpList p0v0 p []))
    
  hdeDepends _ p = T.fatal p
  

gdeElet pdeElet p =
  T.fun3 adeElet pdeElet p hdeElet
  where
  
  hdeElet fisRec fdfs fe p =
    T.cif p170v6 p
      (T.ap2 p170v19 p (p170v19 !|| p) (T.ap1 p170v9 p (gnot p170v9 p) fisRec)
        (T.ap1 p170v22 p (gnonRec p170v22 p) fdfs))
      (\ p ->
        T.con3 p171v14 p ELet aELet (T.con0 p171v19 p False aFalse)
          (gdfs' p171v25 p) fe)
      (\ p ->
        T.con3 p172v14 p ELet aELet (T.con0 p172v19 p True aTrue)
          (gdfs' p172v25 p) fe)
    where
    
    gdfs' pdfs' p = T.constUse pdfs' p sdfs'
    
    sdfs' =
      T.constDef p a173v12dfs'
        (\ p ->
          T.ap1 p0v0 p
            (T.ap2 p173v19 p (TPrelude.g_foldr p173v19 p)
              (T.fun2 T.mkLambda p173v19 p
                (\ f_x f_y p ->
                  T.ccase p0v0 p
                    (let
                      v0v0v1 (T.R (T.Tuple2 fn fe) _) p =
                        T.ap1 p173v19 p
                          (T.pa1 T.Cons T.cn1 p173v19 p T.aCons
                            (T.con2 p173v20 p T.Tuple2 T.aTuple2 fn
                              (T.ap1 p173v24 p (gdeDepends p173v24 p) fe))) f_y
                      v0v0v1 _ p = T.projection p173v19 p f_y in (v0v0v1)) f_x))
              fdfs) (T.fromExpList p0v0 p []))
    
    gnonRec pnonRec p =
      T.fun1 a174v12nonRec pnonRec p hnonRec
      where
      
      hnonRec
        (T.R
          (T.Cons (T.R (T.Tuple2 fn (T.R (T.Tuple2 ffree fe) _)) _)
            (T.R T.List _)) _) p =
        T.ap1 p174v38 p (gnot p174v38 p)
          (T.ap2 p174v43 p (gutSetElementOf p174v43 p) fn ffree)
      hnonRec fdfs p = T.con0 p175v38 p False aFalse
      
    
  

tDependancy = T.mkModule "Dependancy" "Dependancy.hs" Prelude.True

adeBindersOf = T.mkVariable tDependancy 140001 3 1 "deBindersOf" Prelude.False

adeValuesOf = T.mkVariable tDependancy 210001 3 1 "deValuesOf" Prelude.False

adeFreeVars = T.mkVariable tDependancy 280001 3 1 "deFreeVars" Prelude.False

adeFreeVarsOf = T.mkVariable tDependancy 650001 3 1 "deFreeVarsOf" Prelude.False

adeDepthFirstSearch =
  T.mkVariable tDependancy 780001 3 0 "deDepthFirstSearch" Prelude.False

adeSpanningSearch =
  T.mkVariable tDependancy 1010001 3 0 "deSpanningSearch" Prelude.False

adeScc = T.mkVariable tDependancy 1230001 3 2 "deScc" Prelude.False

adeDependancy =
  T.mkVariable tDependancy 1340001 3 0 "deDependancy" Prelude.False

adeDepends = T.mkVariable tDependancy 1400001 3 1 "deDepends" Prelude.False

adeElet = T.mkVariable tDependancy 1690001 3 3 "deElet" Prelude.False

a34v12e1' = T.mkVariable tDependancy 340012 3 0 "e1'" Prelude.True

a35v12e2' = T.mkVariable tDependancy 350012 3 0 "e2'" Prelude.True

a39v12e' = T.mkVariable tDependancy 390012 3 0 "e'" Prelude.True

a40v12alts' = T.mkVariable tDependancy 400012 3 0 "alts'" Prelude.True

a41v12free = T.mkVariable tDependancy 410012 3 0 "free" Prelude.True

a42v12f = T.mkVariable tDependancy 420012 3 1 "f" Prelude.True

a46v12body' = T.mkVariable tDependancy 460012 3 0 "body'" Prelude.True

a50v12binders = T.mkVariable tDependancy 500012 3 0 "binders" Prelude.True

a51v12binderSet = T.mkVariable tDependancy 510012 3 0 "binderSet" Prelude.True

a52v12values' = T.mkVariable tDependancy 520012 3 0 "values'" Prelude.True

a53v12defns' = T.mkVariable tDependancy 530012 3 0 "defns'" Prelude.True

a54v12freeInValues =
  T.mkVariable tDependancy 540012 3 0 "freeInValues" Prelude.True

a55v12defnsFree = T.mkVariable tDependancy 550012 3 0 "defnsFree" Prelude.True

a57v12body' = T.mkVariable tDependancy 570012 3 0 "body'" Prelude.True

a58v12bodyFree = T.mkVariable tDependancy 580012 3 0 "bodyFree" Prelude.True

a81v6search = T.mkVariable tDependancy 810006 3 3 "search" Prelude.True

a85v10visited' = T.mkVariable tDependancy 850010 3 0 "visited'" Prelude.True

a85v20sequence' = T.mkVariable tDependancy 850020 3 0 "sequence'" Prelude.True

a104v6search = T.mkVariable tDependancy 1040006 3 3 "search" Prelude.True

a108v11visited' = T.mkVariable tDependancy 1080011 3 0 "visited'" Prelude.True

a108v21sequence = T.mkVariable tDependancy 1080021 3 0 "sequence" Prelude.True

a125v12depthFirst =
  T.mkVariable tDependancy 1250012 3 0 "depthFirst" Prelude.True

a126v12spanning = T.mkVariable tDependancy 1260012 3 0 "spanning" Prelude.True

a154v6binders = T.mkVariable tDependancy 1540006 3 0 "binders" Prelude.True

a155v6binderSet = T.mkVariable tDependancy 1550006 3 0 "binderSet" Prelude.True

a157v6edges = T.mkVariable tDependancy 1570006 3 0 "edges" Prelude.True

a159v6ins = T.mkVariable tDependancy 1590006 3 1 "ins" Prelude.True

a160v6outs = T.mkVariable tDependancy 1600006 3 1 "outs" Prelude.True

a161v6components =
  T.mkVariable tDependancy 1610006 3 0 "components" Prelude.True

a162v6defnGroups =
  T.mkVariable tDependancy 1620006 3 0 "defnGroups" Prelude.True

a173v12dfs' = T.mkVariable tDependancy 1730012 3 0 "dfs'" Prelude.True

a174v12nonRec = T.mkVariable tDependancy 1740012 3 1 "nonRec" Prelude.True

p14v1 = T.mkSrcPos tDependancy 140001

p0v0 = T.mkSrcPos tDependancy 0

p14v22 = T.mkSrcPos tDependancy 140022

p21v1 = T.mkSrcPos tDependancy 210001

p21v22 = T.mkSrcPos tDependancy 210022

p28v1 = T.mkSrcPos tDependancy 280001

p28v28 = T.mkSrcPos tDependancy 280028

p28v29 = T.mkSrcPos tDependancy 280029

p28v41 = T.mkSrcPos tDependancy 280041

p29v28 = T.mkSrcPos tDependancy 290028

p29v29 = T.mkSrcPos tDependancy 290029

p29v47 = T.mkSrcPos tDependancy 290047

p30v28 = T.mkSrcPos tDependancy 300028

p30v29 = T.mkSrcPos tDependancy 300029

p30v41 = T.mkSrcPos tDependancy 300041

p34v12 = T.mkSrcPos tDependancy 340012

p34v29 = T.mkSrcPos tDependancy 340029

p35v12 = T.mkSrcPos tDependancy 350012

p35v29 = T.mkSrcPos tDependancy 350029

p33v6 = T.mkSrcPos tDependancy 330006

p33v7 = T.mkSrcPos tDependancy 330007

p33v19 = T.mkSrcPos tDependancy 330019

p33v32 = T.mkSrcPos tDependancy 330032

p33v38 = T.mkSrcPos tDependancy 330038

p33v51 = T.mkSrcPos tDependancy 330051

p33v57 = T.mkSrcPos tDependancy 330057

p33v61 = T.mkSrcPos tDependancy 330061

p33v65 = T.mkSrcPos tDependancy 330065

p39v12 = T.mkSrcPos tDependancy 390012

p39v29 = T.mkSrcPos tDependancy 390029

p40v12 = T.mkSrcPos tDependancy 400012

p40v29 = T.mkSrcPos tDependancy 400029

p40v30 = T.mkSrcPos tDependancy 400030

p40v34 = T.mkSrcPos tDependancy 400034

p40v39 = T.mkSrcPos tDependancy 400039

p41v12 = T.mkSrcPos tDependancy 410012

p41v29 = T.mkSrcPos tDependancy 410029

p41v45 = T.mkSrcPos tDependancy 410045

p41v49 = T.mkSrcPos tDependancy 410049

p41v51 = T.mkSrcPos tDependancy 410051

p42v12 = T.mkSrcPos tDependancy 420012

p42v29 = T.mkSrcPos tDependancy 420029

p42v47 = T.mkSrcPos tDependancy 420047

p42v64 = T.mkSrcPos tDependancy 420064

p38v6 = T.mkSrcPos tDependancy 380006

p38v7 = T.mkSrcPos tDependancy 380007

p38v19 = T.mkSrcPos tDependancy 380019

p38v32 = T.mkSrcPos tDependancy 380032

p38v36 = T.mkSrcPos tDependancy 380036

p38v42 = T.mkSrcPos tDependancy 380042

p38v48 = T.mkSrcPos tDependancy 380048

p38v51 = T.mkSrcPos tDependancy 380051

p46v12 = T.mkSrcPos tDependancy 460012

p46v29 = T.mkSrcPos tDependancy 460029

p45v6 = T.mkSrcPos tDependancy 450006

p45v7 = T.mkSrcPos tDependancy 450007

p45v25 = T.mkSrcPos tDependancy 450025

p45v38 = T.mkSrcPos tDependancy 450038

p45v46 = T.mkSrcPos tDependancy 450046

p45v67 = T.mkSrcPos tDependancy 450067

p45v77 = T.mkSrcPos tDependancy 450077

p50v12 = T.mkSrcPos tDependancy 500012

p50v29 = T.mkSrcPos tDependancy 500029

p51v12 = T.mkSrcPos tDependancy 510012

p51v29 = T.mkSrcPos tDependancy 510029

p51v43 = T.mkSrcPos tDependancy 510043

p52v12 = T.mkSrcPos tDependancy 520012

p52v29 = T.mkSrcPos tDependancy 520029

p52v33 = T.mkSrcPos tDependancy 520033

p52v45 = T.mkSrcPos tDependancy 520045

p53v12 = T.mkSrcPos tDependancy 530012

p53v29 = T.mkSrcPos tDependancy 530029

p53v33 = T.mkSrcPos tDependancy 530033

p53v41 = T.mkSrcPos tDependancy 530041

p54v12 = T.mkSrcPos tDependancy 540012

p54v29 = T.mkSrcPos tDependancy 540029

p54v45 = T.mkSrcPos tDependancy 540045

p54v49 = T.mkSrcPos tDependancy 540049

p54v62 = T.mkSrcPos tDependancy 540062

p55v12 = T.mkSrcPos tDependancy 550012

p55v29 = T.mkSrcPos tDependancy 550029

p55v42 = T.mkSrcPos tDependancy 550042

p55v59 = T.mkSrcPos tDependancy 550059

p55v72 = T.mkSrcPos tDependancy 550072

p56v29 = T.mkSrcPos tDependancy 560029

p56v42 = T.mkSrcPos tDependancy 560042

p57v12 = T.mkSrcPos tDependancy 570012

p57v29 = T.mkSrcPos tDependancy 570029

p58v12 = T.mkSrcPos tDependancy 580012

p58v29 = T.mkSrcPos tDependancy 580029

p58v47 = T.mkSrcPos tDependancy 580047

p58v60 = T.mkSrcPos tDependancy 580060

p58v67 = T.mkSrcPos tDependancy 580067

p49v6 = T.mkSrcPos tDependancy 490006

p49v7 = T.mkSrcPos tDependancy 490007

p49v18 = T.mkSrcPos tDependancy 490018

p49v28 = T.mkSrcPos tDependancy 490028

p49v38 = T.mkSrcPos tDependancy 490038

p49v49 = T.mkSrcPos tDependancy 490049

p49v56 = T.mkSrcPos tDependancy 490056

p65v1 = T.mkSrcPos tDependancy 650001

p65v34 = T.mkSrcPos tDependancy 650034

p78v1 = T.mkSrcPos tDependancy 780001

p79v12 = T.mkSrcPos tDependancy 790012

p79v6 = T.mkSrcPos tDependancy 790006

p79v14 = T.mkSrcPos tDependancy 790014

p81v6 = T.mkSrcPos tDependancy 810006

p85v10 = T.mkSrcPos tDependancy 850010

p85v20 = T.mkSrcPos tDependancy 850020

p86v12 = T.mkSrcPos tDependancy 860012

p87v28 = T.mkSrcPos tDependancy 870028

p87v29 = T.mkSrcPos tDependancy 870029

p87v49 = T.mkSrcPos tDependancy 870049

p88v29 = T.mkSrcPos tDependancy 880029

p82v9 = T.mkSrcPos tDependancy 820009

p82v43 = T.mkSrcPos tDependancy 820043

p83v9 = T.mkSrcPos tDependancy 830009

p83v43 = T.mkSrcPos tDependancy 830043

p83v44 = T.mkSrcPos tDependancy 830044

p83v60 = T.mkSrcPos tDependancy 830060

p83v62 = T.mkSrcPos tDependancy 830062

p101v1 = T.mkSrcPos tDependancy 1010001

p102v12 = T.mkSrcPos tDependancy 1020012

p102v6 = T.mkSrcPos tDependancy 1020006

p102v14 = T.mkSrcPos tDependancy 1020014

p104v6 = T.mkSrcPos tDependancy 1040006

p108v11 = T.mkSrcPos tDependancy 1080011

p108v21 = T.mkSrcPos tDependancy 1080021

p109v15 = T.mkSrcPos tDependancy 1090015

p110v27 = T.mkSrcPos tDependancy 1100027

p110v28 = T.mkSrcPos tDependancy 1100028

p110v48 = T.mkSrcPos tDependancy 1100048

p110v72 = T.mkSrcPos tDependancy 1100072

p111v28 = T.mkSrcPos tDependancy 1110028

p105v9 = T.mkSrcPos tDependancy 1050009

p105v43 = T.mkSrcPos tDependancy 1050043

p106v9 = T.mkSrcPos tDependancy 1060009

p106v21 = T.mkSrcPos tDependancy 1060021

p106v22 = T.mkSrcPos tDependancy 1060022

p106v64 = T.mkSrcPos tDependancy 1060064

p106v32 = T.mkSrcPos tDependancy 1060032

p106v53 = T.mkSrcPos tDependancy 1060053

p106v55 = T.mkSrcPos tDependancy 1060055

p123v1 = T.mkSrcPos tDependancy 1230001

p125v12 = T.mkSrcPos tDependancy 1250012

p125v32 = T.mkSrcPos tDependancy 1250032

p125v25 = T.mkSrcPos tDependancy 1250025

p125v34 = T.mkSrcPos tDependancy 1250034

p125v58 = T.mkSrcPos tDependancy 1250058

p125v59 = T.mkSrcPos tDependancy 1250059

p125v71 = T.mkSrcPos tDependancy 1250071

p126v12 = T.mkSrcPos tDependancy 1260012

p126v32 = T.mkSrcPos tDependancy 1260032

p126v25 = T.mkSrcPos tDependancy 1260025

p126v34 = T.mkSrcPos tDependancy 1260034

p126v58 = T.mkSrcPos tDependancy 1260058

p126v59 = T.mkSrcPos tDependancy 1260059

p126v71 = T.mkSrcPos tDependancy 1260071

p124v15 = T.mkSrcPos tDependancy 1240015

p124v6 = T.mkSrcPos tDependancy 1240006

p124v17 = T.mkSrcPos tDependancy 1240017

p134v1 = T.mkSrcPos tDependancy 1340001

p134v26 = T.mkSrcPos tDependancy 1340026

p134v16 = T.mkSrcPos tDependancy 1340016

p134v28 = T.mkSrcPos tDependancy 1340028

p140v1 = T.mkSrcPos tDependancy 1400001

p140v37 = T.mkSrcPos tDependancy 1400037

p141v37 = T.mkSrcPos tDependancy 1410037

p142v37 = T.mkSrcPos tDependancy 1420037

p143v37 = T.mkSrcPos tDependancy 1430037

p143v42 = T.mkSrcPos tDependancy 1430042

p143v57 = T.mkSrcPos tDependancy 1430057

p145v37 = T.mkSrcPos tDependancy 1450037

p145v44 = T.mkSrcPos tDependancy 1450044

p146v41 = T.mkSrcPos tDependancy 1460041

p146v42 = T.mkSrcPos tDependancy 1460042

p146v46 = T.mkSrcPos tDependancy 1460046

p146v51 = T.mkSrcPos tDependancy 1460051

p149v37 = T.mkSrcPos tDependancy 1490037

p149v46 = T.mkSrcPos tDependancy 1490046

p154v6 = T.mkSrcPos tDependancy 1540006

p154v19 = T.mkSrcPos tDependancy 1540019

p155v6 = T.mkSrcPos tDependancy 1550006

p155v19 = T.mkSrcPos tDependancy 1550019

p155v32 = T.mkSrcPos tDependancy 1550032

p155v46 = T.mkSrcPos tDependancy 1550046

p156v19 = T.mkSrcPos tDependancy 1560019

p156v32 = T.mkSrcPos tDependancy 1560032

p157v6 = T.mkSrcPos tDependancy 1570006

p157v19 = T.mkSrcPos tDependancy 1570019

p157v20 = T.mkSrcPos tDependancy 1570020

p158v34 = T.mkSrcPos tDependancy 1580034

p158v47 = T.mkSrcPos tDependancy 1580047

p158v70 = T.mkSrcPos tDependancy 1580070

p159v6 = T.mkSrcPos tDependancy 1590006

p159v19 = T.mkSrcPos tDependancy 1590019

p159v41 = T.mkSrcPos tDependancy 1590041

p159v33 = T.mkSrcPos tDependancy 1590033

p160v6 = T.mkSrcPos tDependancy 1600006

p160v19 = T.mkSrcPos tDependancy 1600019

p160v41 = T.mkSrcPos tDependancy 1600041

p160v33 = T.mkSrcPos tDependancy 1600033

p161v6 = T.mkSrcPos tDependancy 1610006

p161v19 = T.mkSrcPos tDependancy 1610019

p161v23 = T.mkSrcPos tDependancy 1610023

p161v36 = T.mkSrcPos tDependancy 1610036

p161v42 = T.mkSrcPos tDependancy 1610042

p161v46 = T.mkSrcPos tDependancy 1610046

p161v51 = T.mkSrcPos tDependancy 1610051

p162v6 = T.mkSrcPos tDependancy 1620006

p162v19 = T.mkSrcPos tDependancy 1620019

p162v20 = T.mkSrcPos tDependancy 1620020

p162v21 = T.mkSrcPos tDependancy 1620021

p162v25 = T.mkSrcPos tDependancy 1620025

p162v44 = T.mkSrcPos tDependancy 1620044

p163v61 = T.mkSrcPos tDependancy 1630061

p152v6 = T.mkSrcPos tDependancy 1520006

p152v13 = T.mkSrcPos tDependancy 1520013

p152v28 = T.mkSrcPos tDependancy 1520028

p152v44 = T.mkSrcPos tDependancy 1520044

p169v1 = T.mkSrcPos tDependancy 1690001

p173v12 = T.mkSrcPos tDependancy 1730012

p173v19 = T.mkSrcPos tDependancy 1730019

p173v20 = T.mkSrcPos tDependancy 1730020

p173v24 = T.mkSrcPos tDependancy 1730024

p174v12 = T.mkSrcPos tDependancy 1740012

p174v38 = T.mkSrcPos tDependancy 1740038

p174v43 = T.mkSrcPos tDependancy 1740043

p175v38 = T.mkSrcPos tDependancy 1750038

p170v6 = T.mkSrcPos tDependancy 1700006

p170v19 = T.mkSrcPos tDependancy 1700019

p170v9 = T.mkSrcPos tDependancy 1700009

p170v22 = T.mkSrcPos tDependancy 1700022

p171v14 = T.mkSrcPos tDependancy 1710014

p171v19 = T.mkSrcPos tDependancy 1710019

p171v25 = T.mkSrcPos tDependancy 1710025

p172v14 = T.mkSrcPos tDependancy 1720014

p172v19 = T.mkSrcPos tDependancy 1720019

p172v25 = T.mkSrcPos tDependancy 1720025
