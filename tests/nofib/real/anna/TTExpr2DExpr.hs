module TTExpr2DExpr (gtxGetInstantiations,gtx2dxAnnTree,gtx2dx,gtx2dx_aux) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 
import TDomainExpr 
import TMakeDomains 
import TTypeCheck5 
import TList  (gnub)

gtxGetInstantiations ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun DExpr (T.Fun DExpr (AList Naam Domain)))

gtxGetInstantiations ptxGetInstantiations p =
  T.fun2 atxGetInstantiations ptxGetInstantiations p htxGetInstantiations
  where
  
  htxGetInstantiations fsimplest fusage p =
    T.ap2 p30v6 p (gconsistent p30v6 p) (T.con0 p30v17 p T.List T.aList)
      (T.ap2 p30v21 p (ggi p30v21 p) fsimplest fusage)
    where
    
    ggi pgi p =
      T.fun2 a32v9gi pgi p hgi
      where
      
      hgi (T.R (DXVar fv) _) fdexpr p =
        T.fromExpList p32v45 p
          [T.con2 p32v46 p T.Tuple2 T.aTuple2 fv
              (T.ap1 p32v50 p (gdxApplyDSubst_2 p32v50 p) fdexpr)]
      hgi (T.R DXTwo _) (T.R DXTwo _) p = T.con0 p33v45 p T.List T.aList
      hgi (T.R (DXLift1 fdxs1) _) (T.R (DXLift1 fdxs2) _) p =
        T.ap1 p34v45 p (gconcat p34v45 p)
          (T.ap3 p34v53 p (gmyZipWith2 p34v53 p) (ggi p34v64 p) fdxs1 fdxs2)
      hgi (T.R (DXLift2 fdxs1) _) (T.R (DXLift2 fdxs2) _) p =
        T.ap1 p35v45 p (gconcat p35v45 p)
          (T.ap3 p35v53 p (gmyZipWith2 p35v53 p) (ggi p35v64 p) fdxs1 fdxs2)
      hgi (T.R (DXFunc fdxss1 fdxt1) _) (T.R (DXFunc fdxss2 fdxt2) _) p =
        let
          gbasis_arity pbasis_arity p = T.constUse pbasis_arity p sbasis_arity
          sbasis_arity =
            T.constDef p a37v17basis_arity
              (\ p -> T.ap1 p37v31 p (glength p37v31 p) fdxss1)
          gusage_arity pusage_arity p = T.constUse pusage_arity p susage_arity
          susage_arity =
            T.constDef p a38v17usage_arity
              (\ p -> T.ap1 p38v31 p (glength p38v31 p) fdxss2)
          gnew_dxss2 pnew_dxss2 p = T.constUse pnew_dxss2 p snew_dxss2
          gnew_dxt2 pnew_dxss2 p = T.constUse pnew_dxss2 p snew_dxt2
          j39v17new_dxss2 =
            case
              T.cif p40v20 p
                (T.ap2 p40v35 p (p40v35 !> p) (gusage_arity p40v23 p)
                  (gbasis_arity p40v37 p))
                (\ p ->
                  T.con2 p41v25 p T.Tuple2 T.aTuple2
                    (T.ap2 p41v26 p (gtake p41v26 p) (gbasis_arity p41v31 p)
                      fdxss2)
                    (T.con2 p42v26 p DXFunc aDXFunc
                      (T.ap2 p42v34 p (gdrop p42v34 p) (gbasis_arity p42v39 p)
                        fdxss2) fdxt2))
                (\ p -> T.con2 p43v25 p T.Tuple2 T.aTuple2 fdxss2 fdxt2) of
              T.R (T.Tuple2 fnew_dxss2 fnew_dxt2) knew_dxss2 ->
                (knew_dxss2,fnew_dxss2,fnew_dxt2)
              _ -> T.fatal p
          snew_dxss2 =
            T.constDef p a39v18new_dxss2
              (\ _ ->
                case j39v17new_dxss2 of
                  (knew_dxss2,fnew_dxss2,fnew_dxt2) ->
                    T.projection p39v18 knew_dxss2 fnew_dxss2)
          snew_dxt2 =
            T.constDef p a39v29new_dxt2
              (\ _ ->
                case j39v17new_dxss2 of
                  (knew_dxss2,fnew_dxss2,fnew_dxt2) ->
                    T.projection p39v29 knew_dxss2 fnew_dxt2) in
          (T.ap2 p44v34 p (p44v34 !++ p)
            (T.ap2 p44v17 p (ggi p44v17 p) fdxt1 (gnew_dxt2 p44v25 p))
            (T.ap1 p44v37 p (gconcat p44v37 p)
              (T.ap3 p44v45 p (gmyZipWith2 p44v45 p) (ggi p44v56 p) fdxss1
                (gnew_dxss2 p44v65 p))))
      hgi _ _ p = T.fatal p
      
    
    gconsistent pconsistent p =
      T.fun2 a46v9consistent pconsistent p hconsistent
      where
      
      hconsistent facc (T.R T.List _) p = T.projection p46v29 p facc
      hconsistent facc (T.R (T.Cons (T.R (T.Tuple2 fv fdx) _) frest) _) p =
        T.ccase p48v14 p
          (let
            v48v14v1 (T.R Nothing _) p =
              T.ap2 p49v28 p (gconsistent p49v28 p)
                (T.con2 p49v46 p T.Cons T.aCons
                  (T.con2 p49v40 p T.Tuple2 T.aTuple2 fv fdx) facc) frest
            v48v14v1 (T.R (Just fdy) _) p =
              T.cif p50v28 p (T.ap2 p50v34 p (p50v34 !== p) fdx fdy)
                (\ p -> T.ap2 p51v33 p (gconsistent p51v33 p) facc frest)
                (\ p ->
                  T.ap1 p52v33 p (gpanic p52v33 p)
                    (T.fromLitString p52v39 p "txGetInstantiations"))
            v48v14v1 _ p = T.fatal p in (v48v14v1))
          (T.ap2 p48v19 p (gutLookup p48v19 p) facc fv)
      hconsistent _ _ p = T.fatal p
      
    
  

gtx2dxAnnTree ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun TypeDependancy (T.Fun (AnnExpr Naam TExpr) (AnnExpr Naam DExpr)))

gtx2dxAnnTree ptx2dxAnnTree p =
  T.fun2 atx2dxAnnTree ptx2dxAnnTree p htx2dxAnnTree
  where
  
  htx2dxAnnTree ftd ftree p =
    T.ap2 p61v24 p (gtcMapAnnExpr p61v24 p)
      (T.ap1 p61v38 p (gtx2dx p61v38 p) ftd) ftree
  

gtx2dx ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun TypeDependancy (T.Fun TExpr DExpr))

gtx2dx ptx2dx p =
  T.fun2 atx2dx ptx2dx p htx2dx
  where
  
  htx2dx ftd ftexpr p =
    let
      gtypeVars ptypeVars p = T.constUse ptypeVars p stypeVars
      stypeVars =
        T.constDef p a69v10typeVars
          (\ p ->
            T.ap1 p69v21 p (gsort p69v21 p)
              (T.ap1 p69v27 p (gnub p69v27 p)
                (T.ap1 p69v32 p (gtcTvars_in p69v32 p) ftexpr)))
      gdVarEnv pdVarEnv p = T.constUse pdVarEnv p sdVarEnv
      sdVarEnv =
        T.constDef p a70v10dVarEnv
          (\ p ->
            T.ap2 p70v20 p (gzip p70v20 p) (gtypeVars p70v24 p)
              (T.ap1 p0v0 p
                (T.ap2 p70v33 p (TPrelude.g_foldr p70v33 p)
                  (T.fun2 T.mkLambda p70v33 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 fx p =
                            T.ap1 p70v33 p
                              (T.pa1 T.Cons T.cn1 p70v33 p T.aCons
                                (T.fromExpList p70v34 p [fx])) f_y
                          v0v0v1 _ p = T.projection p70v33 p f_y in (v0v0v1))
                        f_x))
                  (T.fromLitString p70v45 p "abcdefghijklmnopqrstuvwxyz"))
                (T.fromExpList p0v0 p []))) in
      (T.cif p71v10 p
        (T.ap2 p71v29 p (p71v29 !> p)
          (T.ap1 p71v13 p (glength p71v13 p) (gtypeVars p71v20 p))
          (T.ap1 p71v31 p (TPreludeBasic.gfromInteger p71v31 p)
            (T.conInteger p71v31 p 26)))
        (\ p ->
          T.ap1 p72v15 p (gpanic p72v15 p) (T.fromLitString p72v21 p "tx2dx"))
        (\ p ->
          T.ap1 p73v15 p (gdxNormaliseDExpr p73v15 p)
            (T.ap3 p73v33 p (gtx2dx_aux p73v33 p) ftd (gdVarEnv p73v46 p)
              ftexpr)))
  

gtx2dx_aux ptx2dx_aux p =
  T.fun3 atx2dx_aux ptx2dx_aux p htx2dx_aux
  where
  
  htx2dx_aux ftd fenv (T.R (TVar fv) _) p =
    T.con1 p76v6 p DXVar aDXVar
      (T.ap3 p76v13 p (gutSureLookup p76v13 p) fenv
        (T.fromLitString p76v30 p "tx2dx_aux(1)") fv)
  htx2dx_aux ftd fenv
    (T.R
      (TCons
        (T.R
          (T.Cons (T.R 'i' _)
            (T.R
              (T.Cons (T.R 'n' _) (T.R (T.Cons (T.R 't' _) (T.R T.List _)) _))
              _)) _) (T.R T.List _)) _) p =
    T.con0 p78v6 p DXTwo aDXTwo
  htx2dx_aux ftd fenv
    (T.R
      (TCons
        (T.R
          (T.Cons (T.R 'c' _)
            (T.R
              (T.Cons (T.R 'h' _)
                (T.R
                  (T.Cons (T.R 'a' _)
                    (T.R (T.Cons (T.R 'r' _) (T.R T.List _)) _)) _)) _)) _)
        (T.R T.List _)) _) p =
    T.con0 p80v6 p DXTwo aDXTwo
  htx2dx_aux ftd fenv (T.R (TArr ft1 ft2) _) p =
    T.con2 p82v6 p DXFunc aDXFunc
      (T.fromExpList p82v13 p
        [T.ap3 p82v14 p (gtx2dx_aux p82v14 p) ftd fenv ft1])
      (T.ap3 p82v36 p (gtx2dx_aux p82v36 p) ftd fenv ft2)
  htx2dx_aux ftd fenv (T.R (TCons ftname ftargs) _) p =
    T.cif p84v6 p (T.ap2 p84v9 p (gmdIsRecursiveType p84v9 p) ftd ftname)
      (\ p ->
        T.con1 p85v11 p DXLift2 aDXLift2
          (T.ap2 p85v20 p (gmap p85v20 p)
            (T.ap2 p85v25 p (gtx2dx_aux p85v25 p) ftd fenv) ftargs))
      (\ p ->
        T.con1 p86v11 p DXLift1 aDXLift1
          (T.ap2 p86v20 p (gmap p86v20 p)
            (T.ap2 p86v25 p (gtx2dx_aux p86v25 p) ftd fenv) ftargs))
  htx2dx_aux _ _ _ p = T.fatal p
  

tTExpr2DExpr = T.mkModule "TExpr2DExpr" "TExpr2DExpr.hs" Prelude.True

atxGetInstantiations =
  T.mkVariable tTExpr2DExpr 290001 3 2 "txGetInstantiations" Prelude.False

atx2dxAnnTree =
  T.mkVariable tTExpr2DExpr 610001 3 2 "tx2dxAnnTree" Prelude.False

atx2dx = T.mkVariable tTExpr2DExpr 680001 3 2 "tx2dx" Prelude.False

atx2dx_aux = T.mkVariable tTExpr2DExpr 750001 3 3 "tx2dx_aux" Prelude.False

a32v9gi = T.mkVariable tTExpr2DExpr 320009 3 2 "gi" Prelude.True

a46v9consistent = T.mkVariable tTExpr2DExpr 460009 3 2 "consistent" Prelude.True

a37v17basis_arity =
  T.mkVariable tTExpr2DExpr 370017 3 0 "basis_arity" Prelude.True

a38v17usage_arity =
  T.mkVariable tTExpr2DExpr 380017 3 0 "usage_arity" Prelude.True

a39v18new_dxss2 = T.mkVariable tTExpr2DExpr 390018 3 0 "new_dxss2" Prelude.True

a39v29new_dxt2 = T.mkVariable tTExpr2DExpr 390029 3 0 "new_dxt2" Prelude.True

a69v10typeVars = T.mkVariable tTExpr2DExpr 690010 3 0 "typeVars" Prelude.True

a70v10dVarEnv = T.mkVariable tTExpr2DExpr 700010 3 0 "dVarEnv" Prelude.True

p29v1 = T.mkSrcPos tTExpr2DExpr 290001

p32v9 = T.mkSrcPos tTExpr2DExpr 320009

p32v45 = T.mkSrcPos tTExpr2DExpr 320045

p32v46 = T.mkSrcPos tTExpr2DExpr 320046

p32v50 = T.mkSrcPos tTExpr2DExpr 320050

p33v45 = T.mkSrcPos tTExpr2DExpr 330045

p34v45 = T.mkSrcPos tTExpr2DExpr 340045

p34v53 = T.mkSrcPos tTExpr2DExpr 340053

p34v64 = T.mkSrcPos tTExpr2DExpr 340064

p35v45 = T.mkSrcPos tTExpr2DExpr 350045

p35v53 = T.mkSrcPos tTExpr2DExpr 350053

p35v64 = T.mkSrcPos tTExpr2DExpr 350064

p37v17 = T.mkSrcPos tTExpr2DExpr 370017

p37v31 = T.mkSrcPos tTExpr2DExpr 370031

p38v17 = T.mkSrcPos tTExpr2DExpr 380017

p38v31 = T.mkSrcPos tTExpr2DExpr 380031

p39v18 = T.mkSrcPos tTExpr2DExpr 390018

p39v29 = T.mkSrcPos tTExpr2DExpr 390029

p40v20 = T.mkSrcPos tTExpr2DExpr 400020

p40v35 = T.mkSrcPos tTExpr2DExpr 400035

p40v23 = T.mkSrcPos tTExpr2DExpr 400023

p40v37 = T.mkSrcPos tTExpr2DExpr 400037

p41v25 = T.mkSrcPos tTExpr2DExpr 410025

p41v26 = T.mkSrcPos tTExpr2DExpr 410026

p41v31 = T.mkSrcPos tTExpr2DExpr 410031

p42v26 = T.mkSrcPos tTExpr2DExpr 420026

p42v34 = T.mkSrcPos tTExpr2DExpr 420034

p42v39 = T.mkSrcPos tTExpr2DExpr 420039

p43v25 = T.mkSrcPos tTExpr2DExpr 430025

p44v34 = T.mkSrcPos tTExpr2DExpr 440034

p44v17 = T.mkSrcPos tTExpr2DExpr 440017

p44v25 = T.mkSrcPos tTExpr2DExpr 440025

p44v37 = T.mkSrcPos tTExpr2DExpr 440037

p44v45 = T.mkSrcPos tTExpr2DExpr 440045

p44v56 = T.mkSrcPos tTExpr2DExpr 440056

p44v65 = T.mkSrcPos tTExpr2DExpr 440065

p46v9 = T.mkSrcPos tTExpr2DExpr 460009

p46v29 = T.mkSrcPos tTExpr2DExpr 460029

p48v14 = T.mkSrcPos tTExpr2DExpr 480014

p48v19 = T.mkSrcPos tTExpr2DExpr 480019

p49v28 = T.mkSrcPos tTExpr2DExpr 490028

p49v46 = T.mkSrcPos tTExpr2DExpr 490046

p49v40 = T.mkSrcPos tTExpr2DExpr 490040

p50v28 = T.mkSrcPos tTExpr2DExpr 500028

p50v34 = T.mkSrcPos tTExpr2DExpr 500034

p51v33 = T.mkSrcPos tTExpr2DExpr 510033

p52v33 = T.mkSrcPos tTExpr2DExpr 520033

p52v39 = T.mkSrcPos tTExpr2DExpr 520039

p30v6 = T.mkSrcPos tTExpr2DExpr 300006

p30v17 = T.mkSrcPos tTExpr2DExpr 300017

p30v21 = T.mkSrcPos tTExpr2DExpr 300021

p61v1 = T.mkSrcPos tTExpr2DExpr 610001

p61v24 = T.mkSrcPos tTExpr2DExpr 610024

p61v38 = T.mkSrcPos tTExpr2DExpr 610038

p68v1 = T.mkSrcPos tTExpr2DExpr 680001

p69v10 = T.mkSrcPos tTExpr2DExpr 690010

p69v21 = T.mkSrcPos tTExpr2DExpr 690021

p69v27 = T.mkSrcPos tTExpr2DExpr 690027

p69v32 = T.mkSrcPos tTExpr2DExpr 690032

p70v10 = T.mkSrcPos tTExpr2DExpr 700010

p70v20 = T.mkSrcPos tTExpr2DExpr 700020

p70v24 = T.mkSrcPos tTExpr2DExpr 700024

p0v0 = T.mkSrcPos tTExpr2DExpr 0

p70v33 = T.mkSrcPos tTExpr2DExpr 700033

p70v34 = T.mkSrcPos tTExpr2DExpr 700034

p70v45 = T.mkSrcPos tTExpr2DExpr 700045

p71v10 = T.mkSrcPos tTExpr2DExpr 710010

p71v29 = T.mkSrcPos tTExpr2DExpr 710029

p71v13 = T.mkSrcPos tTExpr2DExpr 710013

p71v20 = T.mkSrcPos tTExpr2DExpr 710020

p71v31 = T.mkSrcPos tTExpr2DExpr 710031

p72v15 = T.mkSrcPos tTExpr2DExpr 720015

p72v21 = T.mkSrcPos tTExpr2DExpr 720021

p73v15 = T.mkSrcPos tTExpr2DExpr 730015

p73v33 = T.mkSrcPos tTExpr2DExpr 730033

p73v46 = T.mkSrcPos tTExpr2DExpr 730046

p75v1 = T.mkSrcPos tTExpr2DExpr 750001

p76v6 = T.mkSrcPos tTExpr2DExpr 760006

p76v13 = T.mkSrcPos tTExpr2DExpr 760013

p76v30 = T.mkSrcPos tTExpr2DExpr 760030

p78v6 = T.mkSrcPos tTExpr2DExpr 780006

p80v6 = T.mkSrcPos tTExpr2DExpr 800006

p82v6 = T.mkSrcPos tTExpr2DExpr 820006

p82v13 = T.mkSrcPos tTExpr2DExpr 820013

p82v14 = T.mkSrcPos tTExpr2DExpr 820014

p82v36 = T.mkSrcPos tTExpr2DExpr 820036

p84v6 = T.mkSrcPos tTExpr2DExpr 840006

p84v9 = T.mkSrcPos tTExpr2DExpr 840009

p85v11 = T.mkSrcPos tTExpr2DExpr 850011

p85v20 = T.mkSrcPos tTExpr2DExpr 850020

p85v25 = T.mkSrcPos tTExpr2DExpr 850025

p86v11 = T.mkSrcPos tTExpr2DExpr 860011

p86v20 = T.mkSrcPos tTExpr2DExpr 860020

p86v25 = T.mkSrcPos tTExpr2DExpr 860025
