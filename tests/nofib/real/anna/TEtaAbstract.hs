module TEtaAbstract
  (geaEtaAbstract,geaMain,geaMakeApChain,geaMakeNewArgs,geaCurry
    ,geaUncurry) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 

geaEtaAbstract ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (AnnExpr Naam TExpr) (AnnExpr Naam TExpr))

geaEtaAbstract peaEtaAbstract p =
  T.fun1 aeaEtaAbstract peaEtaAbstract p heaEtaAbstract
  where
  
  heaEtaAbstract (fae@(T.R (T.Tuple2 ftau (T.R (AVar fv) _)) _)) p =
    T.projection p21v38 p fae
  heaEtaAbstract (fae@(T.R (T.Tuple2 ftau (T.R (ANum fn) _)) _)) p =
    T.projection p22v38 p fae
  heaEtaAbstract (fae@(T.R (T.Tuple2 ftau (T.R (AConstr fc) _)) _)) p =
    T.projection p23v38 p fae
  heaEtaAbstract (fae@(T.R (T.Tuple2 ftau (T.R (AAp fe1 fe2) _)) _)) p =
    T.con2 p25v6 p T.Tuple2 T.aTuple2 ftau
      (T.con2 p25v12 p AAp aAAp (T.ap1 p25v17 p (geaEtaAbstract p25v17 p) fe1)
        (T.ap1 p25v36 p (geaEtaAbstract p25v36 p) fe2))
  heaEtaAbstract (fae@(T.R (T.Tuple2 ftau (T.R (ACase fsw falts) _)) _)) p =
    T.con2 p27v6 p T.Tuple2 T.aTuple2 ftau
      (T.con2 p27v12 p ACase aACase
        (T.ap1 p27v19 p (geaEtaAbstract p27v19 p) fsw)
        (T.ap1 p0v0 p
          (T.ap2 p28v18 p (TPrelude.g_foldr p28v18 p)
            (T.fun2 T.mkLambda p28v18 p
              (\ f_x f_y p ->
                T.ccase p0v0 p
                  (let
                    v0v0v1 (T.R (T.Tuple2 fn (T.R (T.Tuple2 fps frhs) _)) _) p =
                      T.ap1 p28v18 p
                        (T.pa1 T.Cons T.cn1 p28v18 p T.aCons
                          (T.con2 p28v19 p T.Tuple2 T.aTuple2 fn
                            (T.con2 p28v23 p T.Tuple2 T.aTuple2 fps
                              (T.ap1 p28v28 p (geaEtaAbstract p28v28 p) frhs))))
                        f_y
                    v0v0v1 _ p = T.projection p28v18 p f_y in (v0v0v1)) f_x))
            falts) (T.fromExpList p0v0 p [])))
  heaEtaAbstract (fae@(T.R (T.Tuple2 ftau (T.R (ALam fvs fe) _)) _)) p =
    T.con2 p30v6 p T.Tuple2 T.aTuple2 ftau
      (T.con2 p30v12 p ALam aALam fvs
        (T.ap1 p30v21 p (geaEtaAbstract p30v21 p) fe))
  heaEtaAbstract (fae@(T.R (T.Tuple2 ftau (T.R (ALet frf fdefs fbody) _)) _))
    p =
    let
      gtypeInfo ptypeInfo p = T.constUse ptypeInfo p stypeInfo
      stypeInfo =
        T.constDef p a33v10typeInfo
          (\ p ->
            T.ap1 p0v0 p
              (T.ap2 p33v21 p (TPrelude.g_foldr p33v21 p)
                (T.fun2 T.mkLambda p33v21 p
                  (\ f_x f_y p ->
                    T.ccase p0v0 p
                      (let
                        v0v0v1 (T.R (T.Tuple2 fn (T.R (T.Tuple2 fty frhs) _)) _)
                          p =
                          T.ap1 p33v21 p
                            (T.pa1 T.Cons T.cn1 p33v21 p T.aCons
                              (T.ap1 p33v22 p (geaUncurry p33v22 p) fty)) f_y
                        v0v0v1 _ p = T.projection p33v21 p f_y in (v0v0v1))
                      f_x)) fdefs) (T.fromExpList p0v0 p []))
      gmergedDefs pmergedDefs p = T.constUse pmergedDefs p smergedDefs
      smergedDefs =
        T.constDef p a34v10mergedDefs
          (\ p -> T.ap2 p34v23 p (gmap2nd p34v23 p) (gmergeLams p34v30 p) fdefs)
      gfixedDefs pfixedDefs p = T.constUse pfixedDefs p sfixedDefs
      sfixedDefs =
        T.constDef p a35v10fixedDefs
          (\ p ->
            T.ap3 p35v22 p (gmyZipWith2 p35v22 p) (gfixOne p35v33 p)
              (gmergedDefs p35v40 p) (gtypeInfo p35v51 p))
      gfixOne pfixOne p =
        T.fun2 a36v10fixOne pfixOne p hfixOne
        where
        
        hfixOne
          (fsc@(T.R (T.Tuple2 fn (T.R (T.Tuple2 ftau (T.R (ALam fvs fe) _)) _))
              _)) (z2fixOne@(T.R (T.Tuple2 fargTs fresT) _)) p =
          T.cguard p37v25 p
            (T.ap2 p37v25 p (p37v25 !== p)
              (T.ap1 p37v15 p (glength p37v15 p) fvs)
              (T.ap1 p37v28 p (glength p37v28 p) fargTs))
            (\ p -> T.projection p37v44 p fsc)
            (\ p ->
              T.cguard p38v25 p
                (T.ap2 p38v25 p (p38v25 !> p)
                  (T.ap1 p38v15 p (glength p38v15 p) fvs)
                  (T.ap1 p38v28 p (glength p38v28 p) fargTs))
                (\ p ->
                  T.ap1 p38v44 p (gpanic p38v44 p)
                    (T.fromLitString p38v50 p "eaEtaAbstract"))
                (\ p ->
                  T.cguard p39v25 p
                    (T.ap2 p39v25 p (p39v25 !< p)
                      (T.ap1 p39v15 p (glength p39v15 p) fvs)
                      (T.ap1 p39v28 p (glength p39v28 p) fargTs))
                    (\ p -> T.ap3 p39v44 p (geaMain p39v44 p) fsc fargTs fresT)
                    (\ p -> y1fixOne fsc z2fixOne p)))
        hfixOne fsc z2fixOne p = y1fixOne fsc z2fixOne p
        
        y1fixOne (fsc@(T.R (T.Tuple2 fn (T.R (T.Tuple2 ftau fnon_lam_b) _)) _))
          (T.R (T.Tuple2 fargTs fresT) _) p =
          T.cguard p41v15 p (T.ap1 p41v15 p (gnull p41v15 p) fargTs)
            (\ p -> T.projection p41v29 p fsc)
            (\ p ->
              T.cguard p42v15 p (gotherwise p42v15 p)
                (\ p ->
                  T.ap3 p42v29 p (geaMain p42v29 p)
                    (T.con2 p42v36 p T.Tuple2 T.aTuple2 fn
                      (T.con2 p42v40 p T.Tuple2 T.aTuple2 ftau
                        (T.con2 p42v46 p ALam aALam
                          (T.con0 p42v51 p T.List T.aList)
                          (T.con2 p42v54 p T.Tuple2 T.aTuple2 ftau
                            fnon_lam_b)))) fargTs fresT) (\ p -> T.fatal p))
        y1fixOne _ _ p = T.fatal p
        
      gmergeLams pmergeLams p =
        T.fun1 a43v10mergeLams pmergeLams p hmergeLams
        where
        
        hmergeLams
          (fae@(T.R
              (T.Tuple2 ftau
                (T.R (ALam fvs (T.R (T.Tuple2 ftau2 (T.R (ALam fvs2 fe) _)) _))
                  _)) _)) p =
          T.ap1 p44v15 p (gmergeLams p44v15 p)
            (T.con2 p44v25 p T.Tuple2 T.aTuple2 ftau
              (T.con2 p44v31 p ALam aALam
                (T.ap2 p44v39 p (p44v39 !++ p) fvs fvs2) fe))
        hmergeLams fanyThingElse p = T.projection p45v35 p fanyThingElse
         in
      (T.con2 p46v9 p T.Tuple2 T.aTuple2 ftau
        (T.con3 p46v15 p ALet aALet frf (gfixedDefs p46v23 p)
          (T.ap1 p46v34 p (geaEtaAbstract p46v34 p) fbody)))
  heaEtaAbstract _ p = T.fatal p
  

geaMain ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Tuple2 Naam (AnnExpr Naam TExpr))
          (T.Fun (T.List TExpr)
            (T.Fun TExpr (T.Tuple2 Naam (AnnExpr Naam TExpr)))))

geaMain peaMain p =
  T.fun3 aeaMain peaMain p heaMain
  where
  
  heaMain
    (T.R
      (T.Tuple2 fscname
        (T.R (T.Tuple2 ftau (T.R (ALam fvs (T.R (T.Tuple2 ftau2 frhs) _)) _))
          _)) _) fargTs fresT p =
    let
      gactualArity pactualArity p = T.constUse pactualArity p sactualArity
      sactualArity =
        T.constDef p a57v10actualArity
          (\ p -> T.ap1 p57v25 p (glength p57v25 p) fvs)
      greqdArity preqdArity p = T.constUse preqdArity p sreqdArity
      sreqdArity =
        T.constDef p a58v10reqdArity
          (\ p -> T.ap1 p58v25 p (glength p58v25 p) fargTs)
      gnewArgsReqd pnewArgsReqd p = T.constUse pnewArgsReqd p snewArgsReqd
      snewArgsReqd =
        T.constDef p a59v10newArgsReqd
          (\ p ->
            T.ap2 p59v35 p (p59v35 !- p) (greqdArity p59v25 p)
              (gactualArity p59v37 p))
      gnewArgs pnewArgs p = T.constUse pnewArgs p snewArgs
      snewArgs =
        T.constDef p a60v10newArgs
          (\ p ->
            T.ap2 p60v25 p (geaMakeNewArgs p60v25 p) (gnewArgsReqd p60v39 p)
              fvs)
      gnewArgsTypes pnewArgsTypes p = T.constUse pnewArgsTypes p snewArgsTypes
      snewArgsTypes =
        T.constDef p a61v10newArgsTypes
          (\ p ->
            T.ap2 p61v25 p (gmyZip2 p61v25 p) (gnewArgs p61v32 p)
              (T.ap2 p61v41 p (gdrop p61v41 p) (gactualArity p61v46 p) fargTs))
      gappArgTLists pappArgTLists p = T.constUse pappArgTLists p sappArgTLists
      sappArgTLists =
        T.constDef p a62v10appArgTLists
          (\ p ->
            T.ap2 p62v25 p (gmap p62v25 p)
              (T.ap1 p62v31 p (T.ap1 p62v31 p (gflip p62v31 p) (gdrop p62v36 p))
                fargTs)
              (T.ap2 p63v43 p (gmyIntsFromTo p63v43 p) (gactualArity p63v30 p)
                (T.ap2 p63v67 p (p63v67 !- p) (greqdArity p63v58 p)
                  (T.ap1 p63v68 p (TPreludeBasic.gfromInteger p63v68 p)
                    (T.conInteger p63v68 p 1)))))
      gappTypes pappTypes p = T.constUse pappTypes p sappTypes
      sappTypes =
        T.constDef p a64v10appTypes
          (\ p ->
            T.ap2 p64v25 p (gmap p64v25 p)
              (T.ap1 p64v30 p (geaCurry p64v30 p) fresT)
              (gappArgTLists p64v44 p))
      gnewBody pnewBody p = T.constUse pnewBody p snewBody
      snewBody =
        T.constDef p a65v10newBody
          (\ p ->
            T.ap2 p65v25 p (geaMakeApChain p65v25 p)
              (T.ap2 p65v40 p (gmyZip2 p65v40 p) (gnewArgsTypes p65v47 p)
                (gappTypes p65v60 p))
              (T.con2 p65v70 p T.Tuple2 T.aTuple2 ftau2 frhs)) in
      (T.con2 p66v9 p T.Tuple2 T.aTuple2 fscname
        (T.con2 p66v18 p T.Tuple2 T.aTuple2 ftau
          (T.con2 p66v24 p ALam aALam
            (T.ap2 p66v32 p (p66v32 !++ p) fvs (gnewArgs p66v34 p))
            (gnewBody p66v43 p))))
  heaMain _ _ _ p = T.fatal p
  

geaMakeApChain ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.List (T.Tuple2 (T.Tuple2 Naam TExpr) TExpr))
          (T.Fun (AnnExpr Naam TExpr) (AnnExpr Naam TExpr)))

geaMakeApChain peaMakeApChain p =
  T.fun2 aeaMakeApChain peaMakeApChain p heaMakeApChain
  where
  
  heaMakeApChain (T.R T.List _) fapp p = T.projection p75v24 p fapp
  heaMakeApChain
    (T.R (T.Cons (T.R (T.Tuple2 (T.R (T.Tuple2 fv fvtype) _) fvaptype) _) frest)
      _) fapp p =
    T.ap2 p77v6 p (geaMakeApChain p77v6 p) frest
      (T.con2 p77v25 p T.Tuple2 T.aTuple2 fvaptype
        (T.con2 p77v35 p AAp aAAp fapp
          (T.con2 p77v43 p T.Tuple2 T.aTuple2 fvtype
            (T.con1 p77v51 p AVar aAVar fv))))
  heaMakeApChain _ _ p = T.fatal p
  

geaMakeNewArgs ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int (T.Fun (T.List Naam) (T.List Naam)))

geaMakeNewArgs peaMakeNewArgs p =
  T.fun2 aeaMakeNewArgs peaMakeNewArgs p heaMakeNewArgs
  where
  
  heaMakeNewArgs fn fvs p =
    let
      gleadingvs pleadingvs p = T.constUse pleadingvs p sleadingvs
      sleadingvs =
        T.constDef p a85v10leadingvs
          (\ p ->
            T.ap2 p85v22 p (gfilter p85v22 p)
              (T.ap2 p85v33 p (p85v33 !. p) (gnot p85v30 p) (gnull p85v34 p))
              (T.ap2 p85v41 p (gmap p85v41 p)
                (T.ap1 p85v46 p (gtakeWhile p85v46 p)
                  (T.ap2 p85v57 p (TPrelude.gflip p85v57 p) (p85v57 !== p)
                    (T.conChar p85v60 p 'v'))) fvs))
      groot proot p = T.constUse proot p sroot
      sroot =
        T.constDef p a86v10root
          (\ p ->
            T.ap2 p86v44 p (p86v44 !++ p)
              (T.ap1 p86v17 p (glast p86v17 p)
                (T.ap1 p86v23 p (gsort p86v23 p)
                  (T.con2 p86v31 p T.Cons T.aCons (T.fromLitString p86v29 p "")
                    (gleadingvs p86v32 p)))) (T.fromLitString p86v47 p "v"))
      gnewNames pnewNames p = T.constUse pnewNames p snewNames
      snewNames =
        T.constDef p a87v10newNames
          (\ p ->
            T.ap2 p87v21 p (gmap p87v21 p) (gf p87v25 p)
              (T.ap2 p87v31 p (gmyIntsFromTo p87v31 p)
                (T.ap1 p87v28 p (TPreludeBasic.gfromInteger p87v28 p)
                  (T.conInteger p87v28 p 1)) fn))
      gf pf p =
        T.fun1 a88v10f pf p hf
        where
        
        hf fn p =
          T.ap2 p88v21 p (p88v21 !++ p) (groot p88v16 p)
            (T.ap1 p88v24 p (gshow p88v24 p) (fn :: T.R Int))
         in (gnewNames p89v9 p)
  

geaCurry ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun TExpr (T.Fun (T.List TExpr) TExpr))

geaCurry peaCurry p =
  T.fun2 aeaCurry peaCurry p heaCurry
  where
  
  heaCurry fresT (T.R T.List _) p = T.projection p96v29 p fresT
  heaCurry fresT (T.R (T.Cons fargT fargTs) _) p =
    T.con2 p97v29 p TArr aTArr fargT
      (T.ap2 p97v40 p (geaCurry p97v40 p) fresT fargTs)
  heaCurry _ _ p = T.fatal p
  

geaUncurry ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun TExpr (T.Tuple2 (T.List TExpr) TExpr))

geaUncurry peaUncurry p =
  T.fun1 aeaUncurry peaUncurry p heaUncurry
  where
  
  heaUncurry (T.R (TVar ftv) _) p =
    T.con2 p104v23 p T.Tuple2 T.aTuple2 (T.con0 p104v24 p T.List T.aList)
      (T.con1 p104v28 p TVar aTVar ftv)
  heaUncurry (T.R (TArr ft1 ft2) _) p =
    let
      grest prest p = T.constUse prest p srest
      gfinal prest p = T.constUse prest p sfinal
      j107v10rest =
        case T.ap1 p107v26 p (geaUncurry p107v26 p) ft2 of
          T.R (T.Tuple2 frest ffinal) krest -> (krest,frest,ffinal)
          _ -> T.fatal p
      srest =
        T.constDef p a107v11rest
          (\ _ ->
            case j107v10rest of
              (krest,frest,ffinal) -> T.projection p107v11 krest frest)
      sfinal =
        T.constDef p a107v17final
          (\ _ ->
            case j107v10rest of
              (krest,frest,ffinal) -> T.projection p107v17 krest ffinal) in
      (T.con2 p108v9 p T.Tuple2 T.aTuple2
        (T.con2 p108v12 p T.Cons T.aCons ft1 (grest p108v13 p))
        (gfinal p108v19 p))
  heaUncurry (T.R (TCons ftcon ftargs) _) p =
    T.con2 p111v6 p T.Tuple2 T.aTuple2 (T.con0 p111v7 p T.List T.aList)
      (T.con2 p111v11 p TCons aTCons ftcon ftargs)
  heaUncurry _ p = T.fatal p
  

tEtaAbstract = T.mkModule "EtaAbstract" "EtaAbstract.hs" Prelude.True

aeaEtaAbstract =
  T.mkVariable tEtaAbstract 210001 3 1 "eaEtaAbstract" Prelude.False

aeaMain = T.mkVariable tEtaAbstract 560001 3 3 "eaMain" Prelude.False

aeaMakeApChain =
  T.mkVariable tEtaAbstract 750001 3 2 "eaMakeApChain" Prelude.False

aeaMakeNewArgs =
  T.mkVariable tEtaAbstract 840001 3 2 "eaMakeNewArgs" Prelude.False

aeaCurry = T.mkVariable tEtaAbstract 960001 3 2 "eaCurry" Prelude.False

aeaUncurry = T.mkVariable tEtaAbstract 1040001 3 1 "eaUncurry" Prelude.False

a33v10typeInfo = T.mkVariable tEtaAbstract 330010 3 0 "typeInfo" Prelude.True

a34v10mergedDefs =
  T.mkVariable tEtaAbstract 340010 3 0 "mergedDefs" Prelude.True

a35v10fixedDefs = T.mkVariable tEtaAbstract 350010 3 0 "fixedDefs" Prelude.True

a36v10fixOne = T.mkVariable tEtaAbstract 360010 3 2 "fixOne" Prelude.True

a43v10mergeLams = T.mkVariable tEtaAbstract 430010 3 1 "mergeLams" Prelude.True

a57v10actualArity =
  T.mkVariable tEtaAbstract 570010 3 0 "actualArity" Prelude.True

a58v10reqdArity = T.mkVariable tEtaAbstract 580010 3 0 "reqdArity" Prelude.True

a59v10newArgsReqd =
  T.mkVariable tEtaAbstract 590010 3 0 "newArgsReqd" Prelude.True

a60v10newArgs = T.mkVariable tEtaAbstract 600010 3 0 "newArgs" Prelude.True

a61v10newArgsTypes =
  T.mkVariable tEtaAbstract 610010 3 0 "newArgsTypes" Prelude.True

a62v10appArgTLists =
  T.mkVariable tEtaAbstract 620010 3 0 "appArgTLists" Prelude.True

a64v10appTypes = T.mkVariable tEtaAbstract 640010 3 0 "appTypes" Prelude.True

a65v10newBody = T.mkVariable tEtaAbstract 650010 3 0 "newBody" Prelude.True

a85v10leadingvs = T.mkVariable tEtaAbstract 850010 3 0 "leadingvs" Prelude.True

a86v10root = T.mkVariable tEtaAbstract 860010 3 0 "root" Prelude.True

a87v10newNames = T.mkVariable tEtaAbstract 870010 3 0 "newNames" Prelude.True

a88v10f = T.mkVariable tEtaAbstract 880010 3 1 "f" Prelude.True

a107v11rest = T.mkVariable tEtaAbstract 1070011 3 0 "rest" Prelude.True

a107v17final = T.mkVariable tEtaAbstract 1070017 3 0 "final" Prelude.True

p21v1 = T.mkSrcPos tEtaAbstract 210001

p21v38 = T.mkSrcPos tEtaAbstract 210038

p22v38 = T.mkSrcPos tEtaAbstract 220038

p23v38 = T.mkSrcPos tEtaAbstract 230038

p25v6 = T.mkSrcPos tEtaAbstract 250006

p25v12 = T.mkSrcPos tEtaAbstract 250012

p25v17 = T.mkSrcPos tEtaAbstract 250017

p25v36 = T.mkSrcPos tEtaAbstract 250036

p27v6 = T.mkSrcPos tEtaAbstract 270006

p27v12 = T.mkSrcPos tEtaAbstract 270012

p27v19 = T.mkSrcPos tEtaAbstract 270019

p0v0 = T.mkSrcPos tEtaAbstract 0

p28v18 = T.mkSrcPos tEtaAbstract 280018

p28v19 = T.mkSrcPos tEtaAbstract 280019

p28v23 = T.mkSrcPos tEtaAbstract 280023

p28v28 = T.mkSrcPos tEtaAbstract 280028

p30v6 = T.mkSrcPos tEtaAbstract 300006

p30v12 = T.mkSrcPos tEtaAbstract 300012

p30v21 = T.mkSrcPos tEtaAbstract 300021

p33v10 = T.mkSrcPos tEtaAbstract 330010

p33v21 = T.mkSrcPos tEtaAbstract 330021

p33v22 = T.mkSrcPos tEtaAbstract 330022

p34v10 = T.mkSrcPos tEtaAbstract 340010

p34v23 = T.mkSrcPos tEtaAbstract 340023

p34v30 = T.mkSrcPos tEtaAbstract 340030

p35v10 = T.mkSrcPos tEtaAbstract 350010

p35v22 = T.mkSrcPos tEtaAbstract 350022

p35v33 = T.mkSrcPos tEtaAbstract 350033

p35v40 = T.mkSrcPos tEtaAbstract 350040

p35v51 = T.mkSrcPos tEtaAbstract 350051

p36v10 = T.mkSrcPos tEtaAbstract 360010

p37v25 = T.mkSrcPos tEtaAbstract 370025

p37v15 = T.mkSrcPos tEtaAbstract 370015

p37v28 = T.mkSrcPos tEtaAbstract 370028

p37v44 = T.mkSrcPos tEtaAbstract 370044

p38v25 = T.mkSrcPos tEtaAbstract 380025

p38v15 = T.mkSrcPos tEtaAbstract 380015

p38v28 = T.mkSrcPos tEtaAbstract 380028

p38v44 = T.mkSrcPos tEtaAbstract 380044

p38v50 = T.mkSrcPos tEtaAbstract 380050

p39v25 = T.mkSrcPos tEtaAbstract 390025

p39v15 = T.mkSrcPos tEtaAbstract 390015

p39v28 = T.mkSrcPos tEtaAbstract 390028

p39v44 = T.mkSrcPos tEtaAbstract 390044

p41v15 = T.mkSrcPos tEtaAbstract 410015

p41v29 = T.mkSrcPos tEtaAbstract 410029

p42v15 = T.mkSrcPos tEtaAbstract 420015

p42v29 = T.mkSrcPos tEtaAbstract 420029

p42v36 = T.mkSrcPos tEtaAbstract 420036

p42v40 = T.mkSrcPos tEtaAbstract 420040

p42v46 = T.mkSrcPos tEtaAbstract 420046

p42v51 = T.mkSrcPos tEtaAbstract 420051

p42v54 = T.mkSrcPos tEtaAbstract 420054

p43v10 = T.mkSrcPos tEtaAbstract 430010

p44v15 = T.mkSrcPos tEtaAbstract 440015

p44v25 = T.mkSrcPos tEtaAbstract 440025

p44v31 = T.mkSrcPos tEtaAbstract 440031

p44v39 = T.mkSrcPos tEtaAbstract 440039

p45v35 = T.mkSrcPos tEtaAbstract 450035

p46v9 = T.mkSrcPos tEtaAbstract 460009

p46v15 = T.mkSrcPos tEtaAbstract 460015

p46v23 = T.mkSrcPos tEtaAbstract 460023

p46v34 = T.mkSrcPos tEtaAbstract 460034

p56v1 = T.mkSrcPos tEtaAbstract 560001

p57v10 = T.mkSrcPos tEtaAbstract 570010

p57v25 = T.mkSrcPos tEtaAbstract 570025

p58v10 = T.mkSrcPos tEtaAbstract 580010

p58v25 = T.mkSrcPos tEtaAbstract 580025

p59v10 = T.mkSrcPos tEtaAbstract 590010

p59v35 = T.mkSrcPos tEtaAbstract 590035

p59v25 = T.mkSrcPos tEtaAbstract 590025

p59v37 = T.mkSrcPos tEtaAbstract 590037

p60v10 = T.mkSrcPos tEtaAbstract 600010

p60v25 = T.mkSrcPos tEtaAbstract 600025

p60v39 = T.mkSrcPos tEtaAbstract 600039

p61v10 = T.mkSrcPos tEtaAbstract 610010

p61v25 = T.mkSrcPos tEtaAbstract 610025

p61v32 = T.mkSrcPos tEtaAbstract 610032

p61v41 = T.mkSrcPos tEtaAbstract 610041

p61v46 = T.mkSrcPos tEtaAbstract 610046

p62v10 = T.mkSrcPos tEtaAbstract 620010

p62v25 = T.mkSrcPos tEtaAbstract 620025

p62v31 = T.mkSrcPos tEtaAbstract 620031

p62v36 = T.mkSrcPos tEtaAbstract 620036

p63v43 = T.mkSrcPos tEtaAbstract 630043

p63v30 = T.mkSrcPos tEtaAbstract 630030

p63v67 = T.mkSrcPos tEtaAbstract 630067

p63v58 = T.mkSrcPos tEtaAbstract 630058

p63v68 = T.mkSrcPos tEtaAbstract 630068

p64v10 = T.mkSrcPos tEtaAbstract 640010

p64v25 = T.mkSrcPos tEtaAbstract 640025

p64v30 = T.mkSrcPos tEtaAbstract 640030

p64v44 = T.mkSrcPos tEtaAbstract 640044

p65v10 = T.mkSrcPos tEtaAbstract 650010

p65v25 = T.mkSrcPos tEtaAbstract 650025

p65v40 = T.mkSrcPos tEtaAbstract 650040

p65v47 = T.mkSrcPos tEtaAbstract 650047

p65v60 = T.mkSrcPos tEtaAbstract 650060

p65v70 = T.mkSrcPos tEtaAbstract 650070

p66v9 = T.mkSrcPos tEtaAbstract 660009

p66v18 = T.mkSrcPos tEtaAbstract 660018

p66v24 = T.mkSrcPos tEtaAbstract 660024

p66v32 = T.mkSrcPos tEtaAbstract 660032

p66v34 = T.mkSrcPos tEtaAbstract 660034

p66v43 = T.mkSrcPos tEtaAbstract 660043

p75v1 = T.mkSrcPos tEtaAbstract 750001

p75v24 = T.mkSrcPos tEtaAbstract 750024

p77v6 = T.mkSrcPos tEtaAbstract 770006

p77v25 = T.mkSrcPos tEtaAbstract 770025

p77v35 = T.mkSrcPos tEtaAbstract 770035

p77v43 = T.mkSrcPos tEtaAbstract 770043

p77v51 = T.mkSrcPos tEtaAbstract 770051

p84v1 = T.mkSrcPos tEtaAbstract 840001

p85v10 = T.mkSrcPos tEtaAbstract 850010

p85v22 = T.mkSrcPos tEtaAbstract 850022

p85v33 = T.mkSrcPos tEtaAbstract 850033

p85v30 = T.mkSrcPos tEtaAbstract 850030

p85v34 = T.mkSrcPos tEtaAbstract 850034

p85v41 = T.mkSrcPos tEtaAbstract 850041

p85v46 = T.mkSrcPos tEtaAbstract 850046

p85v57 = T.mkSrcPos tEtaAbstract 850057

p85v60 = T.mkSrcPos tEtaAbstract 850060

p86v10 = T.mkSrcPos tEtaAbstract 860010

p86v44 = T.mkSrcPos tEtaAbstract 860044

p86v17 = T.mkSrcPos tEtaAbstract 860017

p86v23 = T.mkSrcPos tEtaAbstract 860023

p86v31 = T.mkSrcPos tEtaAbstract 860031

p86v29 = T.mkSrcPos tEtaAbstract 860029

p86v32 = T.mkSrcPos tEtaAbstract 860032

p86v47 = T.mkSrcPos tEtaAbstract 860047

p87v10 = T.mkSrcPos tEtaAbstract 870010

p87v21 = T.mkSrcPos tEtaAbstract 870021

p87v25 = T.mkSrcPos tEtaAbstract 870025

p87v31 = T.mkSrcPos tEtaAbstract 870031

p87v28 = T.mkSrcPos tEtaAbstract 870028

p88v10 = T.mkSrcPos tEtaAbstract 880010

p88v21 = T.mkSrcPos tEtaAbstract 880021

p88v16 = T.mkSrcPos tEtaAbstract 880016

p88v24 = T.mkSrcPos tEtaAbstract 880024

p89v9 = T.mkSrcPos tEtaAbstract 890009

p96v1 = T.mkSrcPos tEtaAbstract 960001

p96v29 = T.mkSrcPos tEtaAbstract 960029

p97v29 = T.mkSrcPos tEtaAbstract 970029

p97v40 = T.mkSrcPos tEtaAbstract 970040

p104v1 = T.mkSrcPos tEtaAbstract 1040001

p104v23 = T.mkSrcPos tEtaAbstract 1040023

p104v24 = T.mkSrcPos tEtaAbstract 1040024

p104v28 = T.mkSrcPos tEtaAbstract 1040028

p107v11 = T.mkSrcPos tEtaAbstract 1070011

p107v17 = T.mkSrcPos tEtaAbstract 1070017

p107v26 = T.mkSrcPos tEtaAbstract 1070026

p108v9 = T.mkSrcPos tEtaAbstract 1080009

p108v12 = T.mkSrcPos tEtaAbstract 1080012

p108v13 = T.mkSrcPos tEtaAbstract 1080013

p108v19 = T.mkSrcPos tEtaAbstract 1080019

p111v6 = T.mkSrcPos tEtaAbstract 1110006

p111v7 = T.mkSrcPos tEtaAbstract 1110007

p111v11 = T.mkSrcPos tEtaAbstract 1110011
