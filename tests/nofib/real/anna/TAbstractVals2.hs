module TAbstractVals2
  (gavUncurry,gavTopR,gavTopR_aux_2,gavTopR_aux,gavBottomR,gavBottomR_aux_2
    ,gavBottomR_aux,gavIsBottomR,gavIsBottomRep,gavIsTopR,gavIsTopRep,(!<<)
    ,gavLEQR_list,gavBelowEQfrel,gavBelowEQfrontier,gavBelowEQrep,(!\/)
    ,gavLUBfrel,gavLUBfrontier,gavLUBrep,gavLUBmin1frontier,gavLUBmax0frontier
    ,(!/\),gavGLBfrel,gavGLBfrontier,gavGLBrep,gavGLBmax0frontier
    ,gavGLBmin1frontier,gavBelowMax0frel,gavAboveMin1frel,gavMinAddPtfrel
    ,gavMaxAddPtfrel,gavMinfrel,gavMaxfrel,gavBelowMax0R,gavAboveMin1R
    ,gavMinAddPtR,gavMaxAddPtR,gavMinR,gavMaxR,gavBelowMax0rep,gavAboveMin1rep
    ,gavMinAddPtrep,gavMaxAddPtrep,gavMinrep,gavMaxrep) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 

gavUncurry ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List Domain) (T.Fun Domain Domain))

gavUncurry pavUncurry p =
  T.fun2 aavUncurry pavUncurry p havUncurry
  where
  
  havUncurry fdss (T.R (Func fdss2 fdt) _) p =
    T.con2 p28v32 p Func aFunc (T.ap2 p28v41 p (p28v41 !++ p) fdss fdss2) fdt
  havUncurry fdss fnon_func_dom p =
    T.con2 p29v32 p Func aFunc fdss fnon_func_dom
  

gavTopR :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain Route)

gavTopR pavTopR p =
  T.fun1 aavTopR pavTopR p havTopR
  where
  
  havTopR (T.R Two _) p = T.con0 p36v28 p One aOne
  havTopR (T.R (Lift1 fds) _) p =
    T.con1 p37v28 p Up1 aUp1
      (T.ap2 p37v33 p (gmap p37v33 p) (gavTopR p37v37 p) fds)
  havTopR (T.R (Lift2 fds) _) p =
    T.con1 p38v28 p UpUp2 aUpUp2
      (T.ap2 p38v35 p (gmap p38v35 p) (gavTopR p38v39 p) fds)
  havTopR (fd@(T.R (Func fdss fdt) _)) p =
    T.con1 p39v28 p Rep aRep (T.ap1 p39v33 p (gavTopR_aux p39v33 p) fd)
  havTopR _ p = T.fatal p
  

gavTopR_aux_2 :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List Domain) Frontier)

gavTopR_aux_2 pavTopR_aux_2 p =
  T.fun1 aavTopR_aux_2 pavTopR_aux_2 p havTopR_aux_2
  where
  
  havTopR_aux_2 fdss p =
    T.con3 p47v6 p Min1Max0 aMin1Max0 (T.ap1 p47v16 p (glength p47v16 p) fdss)
      (T.fromExpList p47v28 p
        [T.con1 p47v29 p MkFrel aMkFrel
            (T.ap2 p47v37 p (gmap p47v37 p) (gavBottomR p47v41 p) fdss)])
      (T.con0 p47v57 p T.List T.aList)
  

gavTopR_aux :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain Rep)

gavTopR_aux pavTopR_aux p =
  T.fun1 aavTopR_aux pavTopR_aux p havTopR_aux
  where
  
  havTopR_aux (T.R (Func fdss (T.R Two _)) _) p =
    T.con1 p55v6 p RepTwo aRepTwo (T.ap1 p55v14 p (gavTopR_aux_2 p55v14 p) fdss)
  havTopR_aux (T.R (Func fdss (T.R (Lift1 fdts) _)) _) p =
    let
      glf plf p = T.constUse plf p slf
      slf =
        T.constDef p a58v10lf
          (\ p -> T.ap1 p58v15 p (gavTopR_aux_2 p58v15 p) fdss)
      ghf_domains phf_domains p = T.constUse phf_domains p shf_domains
      shf_domains =
        T.constDef p a59v10hf_domains
          (\ p ->
            T.ap2 p59v23 p (gmap p59v23 p)
              (T.ap1 p59v28 p (gavUncurry p59v28 p) fdss) fdts)
      ghfs phfs p = T.constUse phfs p shfs
      shfs =
        T.constDef p a60v10hfs
          (\ p ->
            T.ap2 p60v16 p (gmap p60v16 p) (gavTopR_aux p60v20 p)
              (ghf_domains p60v31 p)) in
      (T.con2 p62v10 p Rep1 aRep1 (glf p62v15 p) (ghfs p62v18 p))
  havTopR_aux (T.R (Func fdss (T.R (Lift2 fdts) _)) _) p =
    let
      glf plf p = T.constUse plf p slf
      slf =
        T.constDef p a65v10lf
          (\ p -> T.ap1 p65v15 p (gavTopR_aux_2 p65v15 p) fdss)
      ghf_domains phf_domains p = T.constUse phf_domains p shf_domains
      shf_domains =
        T.constDef p a66v10hf_domains
          (\ p ->
            T.ap2 p66v23 p (gmap p66v23 p)
              (T.ap1 p66v28 p (gavUncurry p66v28 p) fdss) fdts)
      ghfs phfs p = T.constUse phfs p shfs
      shfs =
        T.constDef p a67v10hfs
          (\ p ->
            T.ap2 p67v16 p (gmap p67v16 p) (gavTopR_aux p67v20 p)
              (ghf_domains p67v31 p)) in
      (T.con3 p69v10 p Rep2 aRep2 (glf p69v15 p) (glf p69v18 p) (ghfs p69v21 p))
  havTopR_aux _ p = T.fatal p
  

gavBottomR :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain Route)

gavBottomR pavBottomR p =
  T.fun1 aavBottomR pavBottomR p havBottomR
  where
  
  havBottomR (T.R Two _) p = T.con0 p76v31 p Zero aZero
  havBottomR (T.R (Lift1 fds) _) p = T.con0 p77v31 p Stop1 aStop1
  havBottomR (T.R (Lift2 fds) _) p = T.con0 p78v31 p Stop2 aStop2
  havBottomR (fd@(T.R (Func fdss fdt) _)) p =
    T.con1 p79v31 p Rep aRep (T.ap1 p79v36 p (gavBottomR_aux p79v36 p) fd)
  havBottomR _ p = T.fatal p
  

gavBottomR_aux_2 ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List Domain) Frontier)

gavBottomR_aux_2 pavBottomR_aux_2 p =
  T.fun1 aavBottomR_aux_2 pavBottomR_aux_2 p havBottomR_aux_2
  where
  
  havBottomR_aux_2 fdss p =
    T.con3 p87v6 p Min1Max0 aMin1Max0 (T.ap1 p87v16 p (glength p87v16 p) fdss)
      (T.con0 p87v28 p T.List T.aList)
      (T.fromExpList p87v31 p
        [T.con1 p87v32 p MkFrel aMkFrel
            (T.ap2 p87v40 p (gmap p87v40 p) (gavTopR p87v44 p) fdss)])
  

gavBottomR_aux :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain Rep)

gavBottomR_aux pavBottomR_aux p =
  T.fun1 aavBottomR_aux pavBottomR_aux p havBottomR_aux
  where
  
  havBottomR_aux (T.R (Func fdss (T.R Two _)) _) p =
    T.con1 p95v6 p RepTwo aRepTwo
      (T.ap1 p95v14 p (gavBottomR_aux_2 p95v14 p) fdss)
  havBottomR_aux (T.R (Func fdss (T.R (Lift1 fdts) _)) _) p =
    let
      glf plf p = T.constUse plf p slf
      slf =
        T.constDef p a98v10lf
          (\ p -> T.ap1 p98v15 p (gavBottomR_aux_2 p98v15 p) fdss)
      ghf_domains phf_domains p = T.constUse phf_domains p shf_domains
      shf_domains =
        T.constDef p a99v10hf_domains
          (\ p ->
            T.ap2 p99v23 p (gmap p99v23 p)
              (T.ap1 p99v28 p (gavUncurry p99v28 p) fdss) fdts)
      ghfs phfs p = T.constUse phfs p shfs
      shfs =
        T.constDef p a100v10hfs
          (\ p ->
            T.ap2 p100v16 p (gmap p100v16 p) (gavBottomR_aux p100v20 p)
              (ghf_domains p100v34 p)) in
      (T.con2 p102v10 p Rep1 aRep1 (glf p102v15 p) (ghfs p102v18 p))
  havBottomR_aux (T.R (Func fdss (T.R (Lift2 fdts) _)) _) p =
    let
      glf plf p = T.constUse plf p slf
      slf =
        T.constDef p a105v10lf
          (\ p -> T.ap1 p105v15 p (gavBottomR_aux_2 p105v15 p) fdss)
      ghf_domains phf_domains p = T.constUse phf_domains p shf_domains
      shf_domains =
        T.constDef p a106v10hf_domains
          (\ p ->
            T.ap2 p106v23 p (gmap p106v23 p)
              (T.ap1 p106v28 p (gavUncurry p106v28 p) fdss) fdts)
      ghfs phfs p = T.constUse phfs p shfs
      shfs =
        T.constDef p a107v10hfs
          (\ p ->
            T.ap2 p107v16 p (gmap p107v16 p) (gavBottomR_aux p107v20 p)
              (ghf_domains p107v34 p)) in
      (T.con3 p109v10 p Rep2 aRep2 (glf p109v15 p) (glf p109v18 p)
        (ghfs p109v21 p))
  havBottomR_aux _ p = T.fatal p
  

gavIsBottomR :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route Bool)

gavIsBottomR pavIsBottomR p =
  T.fun1 aavIsBottomR pavIsBottomR p havIsBottomR
  where
  
  havIsBottomR (T.R Zero _) p = T.con0 p116v27 p True aTrue
  havIsBottomR (T.R Stop1 _) p = T.con0 p117v27 p True aTrue
  havIsBottomR (T.R Stop2 _) p = T.con0 p118v27 p True aTrue
  havIsBottomR (T.R One _) p = T.con0 p119v27 p False aFalse
  havIsBottomR (T.R (Up1 _) _) p = T.con0 p120v27 p False aFalse
  havIsBottomR (T.R Up2 _) p = T.con0 p121v27 p False aFalse
  havIsBottomR (T.R (UpUp2 _) _) p = T.con0 p122v27 p False aFalse
  havIsBottomR (T.R (Rep fr) _) p =
    T.ap1 p123v27 p (gavIsBottomRep p123v27 p) fr
  havIsBottomR _ p = T.fatal p
  

gavIsBottomRep :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Rep Bool)

gavIsBottomRep pavIsBottomRep p =
  T.fun1 aavIsBottomRep pavIsBottomRep p havIsBottomRep
  where
  
  havIsBottomRep (T.R (RepTwo (T.R (Min1Max0 far ff1 ff0) _)) _) p =
    T.ap1 p131v6 p (gnull p131v6 p) ff1
  havIsBottomRep (T.R (Rep1 (T.R (Min1Max0 flf_ar flf_f1 flf_f0) _) fhfs) _) p =
    T.ap1 p133v6 p (gnull p133v6 p) flf_f1
  havIsBottomRep (T.R (Rep2 (T.R (Min1Max0 flf_ar flf_f1 flf_f0) _) fmf fhfs) _)
    p =
    T.ap1 p135v6 p (gnull p135v6 p) flf_f1
  havIsBottomRep _ p = T.fatal p
  

gavIsTopR :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route Bool)

gavIsTopR pavIsTopR p =
  T.fun1 aavIsTopR pavIsTopR p havIsTopR
  where
  
  havIsTopR (T.R Zero _) p = T.con0 p143v25 p False aFalse
  havIsTopR (T.R One _) p = T.con0 p144v25 p True aTrue
  havIsTopR (T.R Stop1 _) p = T.con0 p145v25 p False aFalse
  havIsTopR (T.R (Up1 frs) _) p =
    T.ap2 p146v25 p (gmyAll p146v25 p) (gavIsTopR p146v31 p) frs
  havIsTopR (T.R Stop2 _) p = T.con0 p147v25 p False aFalse
  havIsTopR (T.R Up2 _) p = T.con0 p148v25 p False aFalse
  havIsTopR (T.R (UpUp2 frs) _) p =
    T.ap2 p149v25 p (gmyAll p149v25 p) (gavIsTopR p149v31 p) frs
  havIsTopR (T.R (Rep fr) _) p = T.ap1 p150v25 p (gavIsTopRep p150v25 p) fr
  havIsTopR _ p = T.fatal p
  

gavIsTopRep :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Rep Bool)

gavIsTopRep pavIsTopRep p =
  T.fun1 aavIsTopRep pavIsTopRep p havIsTopRep
  where
  
  havIsTopRep (T.R (RepTwo (T.R (Min1Max0 far ff1 ff0) _)) _) p =
    T.ap1 p158v6 p (gnull p158v6 p) ff0
  havIsTopRep (T.R (Rep1 flf fhfs) _) p =
    T.ap2 p160v6 p (gmyAll p160v6 p) (gavIsTopRep p160v12 p) fhfs
  havIsTopRep (T.R (Rep2 flf fmf fhfs) _) p =
    T.ap2 p162v6 p (gmyAll p162v6 p) (gavIsTopRep p162v12 p) fhfs
  havIsTopRep _ p = T.fatal p
  

(!<<) :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route (T.Fun Route Bool))

(!<<) (%<<) p =
  T.fun2 (+<<) (%<<) p (*<<)
  where
  
  (*<<) (T.R Zero _) _ p = T.con0 p175v33 p True aTrue
  (*<<) (T.R One _) (T.R One _) p = T.con0 p176v33 p True aTrue
  (*<<) (T.R One _) (T.R Zero _) p = T.con0 p177v33 p False aFalse
  (*<<) (T.R Stop1 _) _ p = T.con0 p179v33 p True aTrue
  (*<<) (T.R (Up1 frs1) _) (T.R (Up1 frs2) _) p =
    T.ap2 p180v33 p (gavLEQR_list p180v33 p) frs1 frs2
  (*<<) (T.R (Up1 frs1) _) _ p = T.con0 p181v33 p False aFalse
  (*<<) (T.R Stop2 _) _ p = T.con0 p183v33 p True aTrue
  (*<<) (T.R Up2 _) (T.R Stop2 _) p = T.con0 p184v33 p False aFalse
  (*<<) (T.R Up2 _) _ p = T.con0 p185v33 p True aTrue
  (*<<) (T.R (UpUp2 frs1) _) (T.R (UpUp2 frs2) _) p =
    T.ap2 p186v33 p (gavLEQR_list p186v33 p) frs1 frs2
  (*<<) (T.R (UpUp2 frs1) _) _ p = T.con0 p187v33 p False aFalse
  (*<<) (T.R (Rep frep1) _) (T.R (Rep frep2) _) p =
    T.ap2 p189v33 p (gavBelowEQrep p189v33 p) frep1 frep2
  (*<<) _ _ p = T.fatal p
  

gavLEQR_list ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (T.List Route) (T.Fun (T.List Route) Bool))

gavLEQR_list pavLEQR_list p =
  T.fun2 aavLEQR_list pavLEQR_list p havLEQR_list
  where
  
  havLEQR_list (T.R T.List _) (T.R T.List _) p = T.con0 p199v6 p True aTrue
  havLEQR_list (T.R (T.Cons fa1 (T.R T.List _)) _)
    (T.R (T.Cons fb1 (T.R T.List _)) _) p =
    T.ap2 p202v9 p (p202v9 !<< p) fa1 fb1
  havLEQR_list (T.R (T.Cons fa1 (T.R (T.Cons fa2 (T.R T.List _)) _)) _)
    (T.R (T.Cons fb1 (T.R (T.Cons fb2 (T.R T.List _)) _)) _) p =
    T.cif p205v6 p (T.ap2 p205v17 p (p205v17 !<< p) fa1 fb1)
      (\ p -> T.ap2 p206v17 p (p206v17 !<< p) fa2 fb2)
      (\ p -> T.con0 p207v14 p False aFalse)
  havLEQR_list
    (T.R (T.Cons fa1 (T.R (T.Cons fa2 (T.R (T.Cons fa3 (T.R T.List _)) _)) _))
      _)
    (T.R (T.Cons fb1 (T.R (T.Cons fb2 (T.R (T.Cons fb3 (T.R T.List _)) _)) _))
      _) p =
    T.cif p210v6 p (T.ap2 p210v17 p (p210v17 !<< p) fa1 fb1)
      (\ p ->
        T.cif p211v14 p (T.ap2 p211v25 p (p211v25 !<< p) fa2 fb2)
          (\ p -> T.ap2 p212v25 p (p212v25 !<< p) fa3 fb3)
          (\ p -> T.con0 p213v22 p False aFalse))
      (\ p -> T.con0 p214v14 p False aFalse)
  havLEQR_list
    (T.R
      (T.Cons fa1
        (T.R
          (T.Cons fa2 (T.R (T.Cons fa3 (T.R (T.Cons fa4 (T.R T.List _)) _)) _))
          _)) _)
    (T.R
      (T.Cons fb1
        (T.R
          (T.Cons fb2 (T.R (T.Cons fb3 (T.R (T.Cons fb4 (T.R T.List _)) _)) _))
          _)) _) p =
    T.cif p217v6 p (T.ap2 p217v17 p (p217v17 !<< p) fa1 fb1)
      (\ p ->
        T.cif p218v14 p (T.ap2 p218v25 p (p218v25 !<< p) fa2 fb2)
          (\ p ->
            T.cif p219v22 p (T.ap2 p219v33 p (p219v33 !<< p) fa3 fb3)
              (\ p -> T.ap2 p220v33 p (p220v33 !<< p) fa4 fb4)
              (\ p -> T.con0 p221v30 p False aFalse))
          (\ p -> T.con0 p222v22 p False aFalse))
      (\ p -> T.con0 p223v14 p False aFalse)
  havLEQR_list
    (T.R
      (T.Cons fa1
        (T.R
          (T.Cons fa2
            (T.R (T.Cons fa3 (T.R (T.Cons fa4 (fas@(T.R (T.Cons _ _) _))) _))
              _)) _)) _)
    (T.R
      (T.Cons fb1
        (T.R
          (T.Cons fb2
            (T.R (T.Cons fb3 (T.R (T.Cons fb4 (fbs@(T.R (T.Cons _ _) _))) _))
              _)) _)) _) p =
    T.cif p227v6 p (T.ap2 p227v17 p (p227v17 !<< p) fa1 fb1)
      (\ p ->
        T.cif p228v14 p (T.ap2 p228v25 p (p228v25 !<< p) fa2 fb2)
          (\ p ->
            T.cif p229v22 p (T.ap2 p229v33 p (p229v33 !<< p) fa3 fb3)
              (\ p ->
                T.cif p230v30 p (T.ap2 p230v41 p (p230v41 !<< p) fa4 fb4)
                  (\ p -> T.ap2 p231v38 p (gavLEQR_list p231v38 p) fas fbs)
                  (\ p -> T.con0 p232v38 p False aFalse))
              (\ p -> T.con0 p233v30 p False aFalse))
          (\ p -> T.con0 p234v22 p False aFalse))
      (\ p -> T.con0 p235v14 p False aFalse)
  havLEQR_list _ _ p =
    T.ap1 p239v30 p (gpanic p239v30 p)
      (T.fromLitString p239v36 p "avLEQR_list: unequal lists")
  

gavBelowEQfrel ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun FrontierElem (T.Fun FrontierElem Bool))

gavBelowEQfrel pavBelowEQfrel p =
  T.fun2 aavBelowEQfrel pavBelowEQfrel p havBelowEQfrel
  where
  
  havBelowEQfrel (T.R (MkFrel frs1) _) (T.R (MkFrel frs2) _) p =
    T.ap2 p247v6 p (gavLEQR_list p247v6 p) frs1 frs2
  havBelowEQfrel _ _ p = T.fatal p
  

gavBelowEQfrontier ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Frontier (T.Fun Frontier Bool))

gavBelowEQfrontier pavBelowEQfrontier p =
  T.fun2 aavBelowEQfrontier pavBelowEQfrontier p havBelowEQfrontier
  where
  
  havBelowEQfrontier (T.R (Min1Max0 far1 ff1a ff0a) _)
    (T.R (Min1Max0 far2 ff1b ff0b) _) p =
    let
      gouter pouter p =
        T.fun1 a262v10outer pouter p houter
        where
        
        houter (T.R T.List _) p = T.con0 p262v28 p True aTrue
        houter (T.R (T.Cons fx fxs) _) p =
          T.cif p263v28 p (T.ap2 p263v31 p (ginner p263v31 p) fx ff1b)
            (\ p -> T.ap1 p263v48 p (gouter p263v48 p) fxs)
            (\ p -> T.con0 p263v62 p False aFalse)
        houter _ p = T.fatal p
        
      ginner pinner p =
        T.fun2 a264v10inner pinner p hinner
        where
        
        hinner fy (T.R T.List _) p = T.con0 p264v28 p False aFalse
        hinner fy (T.R (T.Cons fz fzs) _) p =
          T.cif p265v28 p (T.ap2 p265v34 p (gavBelowEQfrel p265v34 p) fz fy)
            (\ p -> T.con0 p265v56 p True aTrue)
            (\ p -> T.ap2 p265v66 p (ginner p265v66 p) fy fzs)
        hinner _ _ p = T.fatal p
         in (T.ap1 p267v10 p (gouter p267v10 p) ff1a)
  havBelowEQfrontier _ _ p = T.fatal p
  

gavBelowEQrep :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Rep (T.Fun Rep Bool))

gavBelowEQrep pavBelowEQrep p =
  T.fun2 aavBelowEQrep pavBelowEQrep p havBelowEQrep
  where
  
  havBelowEQrep (T.R (RepTwo ffr1) _) (T.R (RepTwo ffr2) _) p =
    T.ap2 p276v6 p (gavBelowEQfrontier p276v6 p) ffr1 ffr2
  havBelowEQrep (T.R (Rep1 flf1 fhfs1) _) (T.R (Rep1 flf2 fhfs2) _) p =
    T.ap2 p279v32 p (p279v32 !&& p)
      (T.ap2 p279v6 p (gavBelowEQfrontier p279v6 p) flf1 flf2)
      (T.ap3 p280v6 p (gmyAndWith2 p280v6 p) (gavBelowEQrep p280v17 p) fhfs1
        fhfs2)
  havBelowEQrep (T.R (Rep2 flf1 fmf1 fhfs1) _) (T.R (Rep2 flf2 fmf2 fhfs2) _)
    p =
    T.ap2 p283v32 p (p283v32 !&& p)
      (T.ap2 p283v6 p (gavBelowEQfrontier p283v6 p) flf1 flf2)
      (T.ap2 p284v32 p (p284v32 !&& p)
        (T.ap2 p284v6 p (gavBelowEQfrontier p284v6 p) fmf1 fmf2)
        (T.ap3 p285v6 p (gmyAndWith2 p285v6 p) (gavBelowEQrep p285v17 p) fhfs1
          fhfs2))
  havBelowEQrep _ _ p = T.fatal p
  

(!\/) :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route (T.Fun Route Route))

(!\/) (%\/) p =
  T.fun2 (+\/) (%\/) p (*\/)
  where
  
  (*\/) (fp@(T.R Zero _)) fq p = T.projection p298v37 p fq
  (*\/) (fp@(T.R One _)) fq p = T.projection p299v37 p fp
  (*\/) (fp@(T.R Stop1 _)) fq p = T.projection p301v37 p fq
  (*\/) (fp@(T.R (Up1 frs1) _)) (T.R Stop1 _) p = T.projection p302v37 p fp
  (*\/) (fp@(T.R (Up1 frs1) _)) (T.R (Up1 frs2) _) p =
    T.con1 p303v37 p Up1 aUp1
      (T.ap3 p303v42 p (gmyZipWith2 p303v42 p) (p303v54 !\/ p) frs1 frs2)
  (*\/) (fp@(T.R Stop2 _)) fq p = T.projection p305v37 p fq
  (*\/) (fp@(T.R Up2 _)) (T.R Stop2 _) p = T.projection p306v37 p fp
  (*\/) (fp@(T.R Up2 _)) fq p = T.projection p307v37 p fq
  (*\/) (fp@(T.R (UpUp2 frs1) _)) (T.R (UpUp2 frs2) _) p =
    T.con1 p308v37 p UpUp2 aUpUp2
      (T.ap3 p308v44 p (gmyZipWith2 p308v44 p) (p308v56 !\/ p) frs1 frs2)
  (*\/) (fp@(T.R (UpUp2 frs1) _)) fq p = T.projection p309v37 p fp
  (*\/) (fp@(T.R (Rep frep1) _)) (fq@(T.R (Rep frep2) _)) p =
    T.con1 p311v37 p Rep aRep
      (T.ap2 p311v42 p (gavLUBrep p311v42 p) frep1 frep2)
  (*\/) _ _ p = T.fatal p
  

gavLUBfrel ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun FrontierElem (T.Fun FrontierElem FrontierElem))

gavLUBfrel pavLUBfrel p =
  T.fun2 aavLUBfrel pavLUBfrel p havLUBfrel
  where
  
  havLUBfrel (T.R (MkFrel frs1) _) (T.R (MkFrel frs2) _) p =
    T.con1 p319v6 p MkFrel aMkFrel
      (T.ap3 p319v14 p (gmyZipWith2 p319v14 p) (p319v26 !\/ p) frs1 frs2)
  havLUBfrel _ _ p = T.fatal p
  

gavLUBfrontier ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Frontier (T.Fun Frontier Frontier))

gavLUBfrontier pavLUBfrontier p =
  T.fun2 aavLUBfrontier pavLUBfrontier p havLUBfrontier
  where
  
  havLUBfrontier (T.R (Min1Max0 far1 ff1a ff0a) _)
    (T.R (Min1Max0 far2 ff1b ff0b) _) p =
    T.con3 p328v6 p Min1Max0 aMin1Max0 far1
      (T.ap2 p328v20 p (gavLUBmin1frontier p328v20 p) ff1a ff1b)
      (T.ap2 p328v48 p (gavLUBmax0frontier p328v48 p) ff0a ff0b)
  havLUBfrontier _ _ p = T.fatal p
  

gavLUBrep :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Rep (T.Fun Rep Rep))

gavLUBrep pavLUBrep p =
  T.fun2 aavLUBrep pavLUBrep p havLUBrep
  where
  
  havLUBrep (T.R (RepTwo ffr1) _) (T.R (RepTwo ffr2) _) p =
    T.con1 p336v6 p RepTwo aRepTwo
      (T.ap2 p336v14 p (gavLUBfrontier p336v14 p) ffr1 ffr2)
  havLUBrep (T.R (Rep1 flf1 fhfs1) _) (T.R (Rep1 flf2 fhfs2) _) p =
    T.con2 p338v6 p Rep1 aRep1
      (T.ap2 p338v12 p (gavLUBfrontier p338v12 p) flf1 flf2)
      (T.ap3 p338v36 p (gmyZipWith2 p338v36 p) (gavLUBrep p338v47 p) fhfs1
        fhfs2)
  havLUBrep (T.R (Rep2 flf1 fmf1 fhfs1) _) (T.R (Rep2 flf2 fmf2 fhfs2) _) p =
    T.con3 p340v6 p Rep2 aRep2
      (T.ap2 p340v12 p (gavLUBfrontier p340v12 p) flf1 flf2)
      (T.ap2 p340v36 p (gavLUBfrontier p340v36 p) fmf1 fmf2)
      (T.ap3 p341v12 p (gmyZipWith2 p341v12 p) (gavLUBrep p341v23 p) fhfs1
        fhfs2)
  havLUBrep _ _ p = T.fatal p
  

gavLUBmin1frontier ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.List FrontierElem)
          (T.Fun (T.List FrontierElem) (T.List FrontierElem)))

gavLUBmin1frontier pavLUBmin1frontier p =
  T.fun2 aavLUBmin1frontier pavLUBmin1frontier p havLUBmin1frontier
  where
  
  havLUBmin1frontier ff1a ff1b p =
    T.ap1 p349v6 p (gsort p349v6 p)
      (T.ap3 p349v12 p (gfoldr p349v12 p) (gavMinAddPtfrel p349v18 p) ff1a ff1b)
  

gavLUBmax0frontier ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.List FrontierElem)
          (T.Fun (T.List FrontierElem) (T.List FrontierElem)))

gavLUBmax0frontier pavLUBmax0frontier p =
  T.fun2 aavLUBmax0frontier pavLUBmax0frontier p havLUBmax0frontier
  where
  
  havLUBmax0frontier ff0a ff0b p =
    T.ap1 p357v6 p (gsort p357v6 p)
      (T.ap1 p357v12 p (gavMaxfrel p357v12 p)
        (T.ap1 p0v0 p
          (T.ap2 p357v22 p (TPrelude.g_foldr p357v22 p)
            (T.fun2 T.mkLambda p357v22 p
              (\ f_x f_y p ->
                T.ccase p0v0 p
                  (let
                    v0v0v1 fx p =
                      T.ap1 p357v22 p
                        (T.ap2 p357v22 p (TPrelude.g_foldr p357v22 p)
                          (T.fun2 T.mkLambda p357v22 p
                            (\ f_x f_y p ->
                              T.ccase p0v0 p
                                (let
                                  v0v0v1 fy p =
                                    T.ap1 p357v22 p
                                      (T.pa1 T.Cons T.cn1 p357v22 p T.aCons
                                        (T.ap2 p357v27 p (gavGLBfrel p357v27 p)
                                          fx fy)) f_y
                                  v0v0v1 _ p = T.projection p357v22 p f_y in
                                  (v0v0v1)) f_x)) ff0b) f_y
                    v0v0v1 _ p = T.projection p357v22 p f_y in (v0v0v1)) f_x))
            ff0a) (T.fromExpList p0v0 p [])))
  

(!/\) :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route (T.Fun Route Route))

(!/\) (%/\) p =
  T.fun2 (+/\) (%/\) p (*/\)
  where
  
  (*/\) (fp@(T.R Zero _)) fq p = T.projection p364v39 p fp
  (*/\) (fp@(T.R One _)) fq p = T.projection p365v39 p fq
  (*/\) (fp@(T.R Stop1 _)) fq p = T.projection p367v39 p fp
  (*/\) (fp@(T.R (Up1 frs1) _)) (T.R (Up1 frs2) _) p =
    T.con1 p368v39 p Up1 aUp1
      (T.ap3 p368v44 p (gmyZipWith2 p368v44 p) (p368v56 !/\ p) frs1 frs2)
  (*/\) (fp@(T.R (Up1 frs1) _)) fq p = T.projection p369v39 p fq
  (*/\) (fp@(T.R Stop2 _)) fq p = T.projection p371v39 p fp
  (*/\) (fp@(T.R Up2 _)) (fq@(T.R Stop2 _)) p = T.projection p372v39 p fq
  (*/\) (fp@(T.R Up2 _)) fq p = T.projection p373v39 p fp
  (*/\) (fp@(T.R (UpUp2 frs1) _)) (fq@(T.R (UpUp2 frs2) _)) p =
    T.con1 p374v39 p UpUp2 aUpUp2
      (T.ap3 p374v46 p (gmyZipWith2 p374v46 p) (p374v58 !/\ p) frs1 frs2)
  (*/\) (fp@(T.R (UpUp2 frs1) _)) fq p = T.projection p375v39 p fq
  (*/\) (fp@(T.R (Rep frep1) _)) (fq@(T.R (Rep frep2) _)) p =
    T.con1 p377v39 p Rep aRep
      (T.ap2 p377v44 p (gavGLBrep p377v44 p) frep1 frep2)
  (*/\) _ _ p = T.fatal p
  

gavGLBfrel ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun FrontierElem (T.Fun FrontierElem FrontierElem))

gavGLBfrel pavGLBfrel p =
  T.fun2 aavGLBfrel pavGLBfrel p havGLBfrel
  where
  
  havGLBfrel (T.R (MkFrel frs1) _) (T.R (MkFrel frs2) _) p =
    T.con1 p385v6 p MkFrel aMkFrel
      (T.ap3 p385v14 p (gmyZipWith2 p385v14 p) (p385v26 !/\ p) frs1 frs2)
  havGLBfrel _ _ p = T.fatal p
  

gavGLBfrontier ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Frontier (T.Fun Frontier Frontier))

gavGLBfrontier pavGLBfrontier p =
  T.fun2 aavGLBfrontier pavGLBfrontier p havGLBfrontier
  where
  
  havGLBfrontier (T.R (Min1Max0 far1 ff1a ff0a) _)
    (T.R (Min1Max0 far2 ff1b ff0b) _) p =
    T.con3 p394v6 p Min1Max0 aMin1Max0 far1
      (T.ap2 p394v20 p (gavGLBmin1frontier p394v20 p) ff1a ff1b)
      (T.ap2 p394v48 p (gavGLBmax0frontier p394v48 p) ff0a ff0b)
  havGLBfrontier _ _ p = T.fatal p
  

gavGLBrep :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Rep (T.Fun Rep Rep))

gavGLBrep pavGLBrep p =
  T.fun2 aavGLBrep pavGLBrep p havGLBrep
  where
  
  havGLBrep (T.R (RepTwo ffr1) _) (T.R (RepTwo ffr2) _) p =
    T.con1 p402v6 p RepTwo aRepTwo
      (T.ap2 p402v14 p (gavGLBfrontier p402v14 p) ffr1 ffr2)
  havGLBrep (T.R (Rep1 flf1 fhfs1) _) (T.R (Rep1 flf2 fhfs2) _) p =
    T.con2 p404v6 p Rep1 aRep1
      (T.ap2 p404v12 p (gavGLBfrontier p404v12 p) flf1 flf2)
      (T.ap3 p404v36 p (gmyZipWith2 p404v36 p) (gavGLBrep p404v47 p) fhfs1
        fhfs2)
  havGLBrep (T.R (Rep2 flf1 fmf1 fhfs1) _) (T.R (Rep2 flf2 fmf2 fhfs2) _) p =
    T.con3 p406v6 p Rep2 aRep2
      (T.ap2 p406v12 p (gavGLBfrontier p406v12 p) flf1 flf2)
      (T.ap2 p406v36 p (gavGLBfrontier p406v36 p) fmf1 fmf2)
      (T.ap3 p407v12 p (gmyZipWith2 p407v12 p) (gavGLBrep p407v23 p) fhfs1
        fhfs2)
  havGLBrep _ _ p = T.fatal p
  

gavGLBmax0frontier ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.List FrontierElem)
          (T.Fun (T.List FrontierElem) (T.List FrontierElem)))

gavGLBmax0frontier pavGLBmax0frontier p =
  T.fun2 aavGLBmax0frontier pavGLBmax0frontier p havGLBmax0frontier
  where
  
  havGLBmax0frontier ff0a ff0b p =
    T.ap1 p415v6 p (gsort p415v6 p)
      (T.ap3 p415v12 p (gfoldr p415v12 p) (gavMaxAddPtfrel p415v18 p) ff0a ff0b)
  

gavGLBmin1frontier ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.List FrontierElem)
          (T.Fun (T.List FrontierElem) (T.List FrontierElem)))

gavGLBmin1frontier pavGLBmin1frontier p =
  T.fun2 aavGLBmin1frontier pavGLBmin1frontier p havGLBmin1frontier
  where
  
  havGLBmin1frontier ff1a ff1b p =
    T.ap1 p423v6 p (gsort p423v6 p)
      (T.ap1 p423v12 p (gavMinfrel p423v12 p)
        (T.ap1 p0v0 p
          (T.ap2 p423v22 p (TPrelude.g_foldr p423v22 p)
            (T.fun2 T.mkLambda p423v22 p
              (\ f_x f_y p ->
                T.ccase p0v0 p
                  (let
                    v0v0v1 fx p =
                      T.ap1 p423v22 p
                        (T.ap2 p423v22 p (TPrelude.g_foldr p423v22 p)
                          (T.fun2 T.mkLambda p423v22 p
                            (\ f_x f_y p ->
                              T.ccase p0v0 p
                                (let
                                  v0v0v1 fy p =
                                    T.ap1 p423v22 p
                                      (T.pa1 T.Cons T.cn1 p423v22 p T.aCons
                                        (T.ap2 p423v27 p (gavLUBfrel p423v27 p)
                                          fx fy)) f_y
                                  v0v0v1 _ p = T.projection p423v22 p f_y in
                                  (v0v0v1)) f_x)) ff1b) f_y
                    v0v0v1 _ p = T.projection p423v22 p f_y in (v0v0v1)) f_x))
            ff1a) (T.fromExpList p0v0 p [])))
  

gavBelowMax0frel pavBelowMax0frel p =
  T.fun2 aavBelowMax0frel pavBelowMax0frel p havBelowMax0frel
  where
  
  havBelowMax0frel fpt ff p =
    T.ap2 p434v26 p (gmyAny p434v26 p)
      (T.ap1 p434v37 p (gavBelowEQfrel p434v37 p) fpt) ff
  

gavAboveMin1frel pavAboveMin1frel p =
  T.fun2 aavAboveMin1frel pavAboveMin1frel p havAboveMin1frel
  where
  
  havAboveMin1frel fpt ff p =
    T.ap2 p436v26 p (gmyAny p436v26 p)
      (T.ap2 p436v34 p (TPrelude.gflip p436v34 p) (gavBelowEQfrel p436v34 p)
        fpt) ff
  

gavMinAddPtfrel pavMinAddPtfrel p =
  T.fun2 aavMinAddPtfrel pavMinAddPtfrel p havMinAddPtfrel
  where
  
  havMinAddPtfrel fx fys p =
    T.cguard p439v9 p (T.ap2 p439v9 p (gavAboveMin1frel p439v9 p) fx fys)
      (\ p -> T.projection p439v31 p fys)
      (\ p ->
        T.cguard p440v6 p (gotherwise p440v6 p)
          (\ p ->
            T.con2 p440v19 p T.Cons T.aCons fx
              (T.ap1 p0v0 p
                (T.ap2 p440v20 p (TPrelude.g_foldr p440v20 p)
                  (T.fun2 T.mkLambda p440v20 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 fy p =
                            T.ap1 p440v20 p
                              (T.ap2 p440v20 p (TPrelude.g_filter p440v20 p)
                                (T.ap1 p440v34 p (gnot p440v34 p)
                                  (T.ap2 p440v42 p (gavBelowEQfrel p440v42 p) fx
                                    fy))
                                (T.pa1 T.Cons T.cn1 p440v20 p T.aCons fy)) f_y
                          v0v0v1 _ p = T.projection p440v20 p f_y in (v0v0v1))
                        f_x)) fys) (T.fromExpList p0v0 p [])))
          (\ p -> T.fatal p))
  

gavMaxAddPtfrel pavMaxAddPtfrel p =
  T.fun2 aavMaxAddPtfrel pavMaxAddPtfrel p havMaxAddPtfrel
  where
  
  havMaxAddPtfrel fx fys p =
    T.cguard p443v9 p (T.ap2 p443v9 p (gavBelowMax0frel p443v9 p) fx fys)
      (\ p -> T.projection p443v31 p fys)
      (\ p ->
        T.cguard p444v6 p (gotherwise p444v6 p)
          (\ p ->
            T.con2 p444v19 p T.Cons T.aCons fx
              (T.ap1 p0v0 p
                (T.ap2 p444v20 p (TPrelude.g_foldr p444v20 p)
                  (T.fun2 T.mkLambda p444v20 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 fy p =
                            T.ap1 p444v20 p
                              (T.ap2 p444v20 p (TPrelude.g_filter p444v20 p)
                                (T.ap1 p444v34 p (gnot p444v34 p)
                                  (T.ap2 p444v42 p (gavBelowEQfrel p444v42 p) fy
                                    fx))
                                (T.pa1 T.Cons T.cn1 p444v20 p T.aCons fy)) f_y
                          v0v0v1 _ p = T.projection p444v20 p f_y in (v0v0v1))
                        f_x)) fys) (T.fromExpList p0v0 p [])))
          (\ p -> T.fatal p))
  

gavMinfrel pavMinfrel p = T.constUse pavMinfrel p savMinfrel

savMinfrel =
  T.constDef T.mkRoot aavMinfrel
    (\ p ->
      T.ap2 p446v13 p (gfoldr p446v13 p) (gavMinAddPtfrel p446v19 p)
        (T.con0 p446v34 p T.List T.aList))

gavMaxfrel pavMaxfrel p = T.constUse pavMaxfrel p savMaxfrel

savMaxfrel =
  T.constDef T.mkRoot aavMaxfrel
    (\ p ->
      T.ap2 p448v13 p (gfoldr p448v13 p) (gavMaxAddPtfrel p448v19 p)
        (T.con0 p448v34 p T.List T.aList))

gavBelowMax0R pavBelowMax0R p =
  T.fun2 aavBelowMax0R pavBelowMax0R p havBelowMax0R
  where
  
  havBelowMax0R fpt ff p =
    T.ap2 p457v23 p (gmyAny p457v23 p) (T.ap1 p457v33 p (p457v33 !<< p) fpt) ff
  

gavAboveMin1R pavAboveMin1R p =
  T.fun2 aavAboveMin1R pavAboveMin1R p havAboveMin1R
  where
  
  havAboveMin1R fpt ff p =
    T.ap2 p459v23 p (gmyAny p459v23 p)
      (T.ap2 p459v30 p (TPrelude.gflip p459v30 p) (p459v30 !<< p) fpt) ff
  

gavMinAddPtR pavMinAddPtR p =
  T.fun2 aavMinAddPtR pavMinAddPtR p havMinAddPtR
  where
  
  havMinAddPtR fx fys p =
    T.cguard p462v9 p (T.ap2 p462v9 p (gavAboveMin1R p462v9 p) fx fys)
      (\ p -> T.projection p462v28 p fys)
      (\ p ->
        T.cguard p463v6 p (gotherwise p463v6 p)
          (\ p ->
            T.con2 p463v19 p T.Cons T.aCons fx
              (T.ap1 p0v0 p
                (T.ap2 p463v20 p (TPrelude.g_foldr p463v20 p)
                  (T.fun2 T.mkLambda p463v20 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 fy p =
                            T.ap1 p463v20 p
                              (T.ap2 p463v20 p (TPrelude.g_filter p463v20 p)
                                (T.ap1 p463v34 p (gnot p463v34 p)
                                  (T.ap2 p463v41 p (p463v41 !<< p) fx fy))
                                (T.pa1 T.Cons T.cn1 p463v20 p T.aCons fy)) f_y
                          v0v0v1 _ p = T.projection p463v20 p f_y in (v0v0v1))
                        f_x)) fys) (T.fromExpList p0v0 p [])))
          (\ p -> T.fatal p))
  

gavMaxAddPtR pavMaxAddPtR p =
  T.fun2 aavMaxAddPtR pavMaxAddPtR p havMaxAddPtR
  where
  
  havMaxAddPtR fx fys p =
    T.cguard p466v9 p (T.ap2 p466v9 p (gavBelowMax0R p466v9 p) fx fys)
      (\ p -> T.projection p466v28 p fys)
      (\ p ->
        T.cguard p467v6 p (gotherwise p467v6 p)
          (\ p ->
            T.con2 p467v19 p T.Cons T.aCons fx
              (T.ap1 p0v0 p
                (T.ap2 p467v20 p (TPrelude.g_foldr p467v20 p)
                  (T.fun2 T.mkLambda p467v20 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 fy p =
                            T.ap1 p467v20 p
                              (T.ap2 p467v20 p (TPrelude.g_filter p467v20 p)
                                (T.ap1 p467v34 p (gnot p467v34 p)
                                  (T.ap2 p467v41 p (p467v41 !<< p) fy fx))
                                (T.pa1 T.Cons T.cn1 p467v20 p T.aCons fy)) f_y
                          v0v0v1 _ p = T.projection p467v20 p f_y in (v0v0v1))
                        f_x)) fys) (T.fromExpList p0v0 p [])))
          (\ p -> T.fatal p))
  

gavMinR pavMinR p = T.constUse pavMinR p savMinR

savMinR =
  T.constDef T.mkRoot aavMinR
    (\ p ->
      T.ap2 p469v10 p (gfoldr p469v10 p) (gavMinAddPtR p469v16 p)
        (T.con0 p469v28 p T.List T.aList))

gavMaxR pavMaxR p = T.constUse pavMaxR p savMaxR

savMaxR =
  T.constDef T.mkRoot aavMaxR
    (\ p ->
      T.ap2 p471v10 p (gfoldr p471v10 p) (gavMaxAddPtR p471v16 p)
        (T.con0 p471v28 p T.List T.aList))

gavBelowMax0rep pavBelowMax0rep p =
  T.fun2 aavBelowMax0rep pavBelowMax0rep p havBelowMax0rep
  where
  
  havBelowMax0rep fpt ff p =
    T.ap2 p480v25 p (gmyAny p480v25 p)
      (T.ap1 p480v36 p (gavBelowEQrep p480v36 p) fpt) ff
  

gavAboveMin1rep pavAboveMin1rep p =
  T.fun2 aavAboveMin1rep pavAboveMin1rep p havAboveMin1rep
  where
  
  havAboveMin1rep fpt ff p =
    T.ap2 p482v25 p (gmyAny p482v25 p)
      (T.ap2 p482v33 p (TPrelude.gflip p482v33 p) (gavBelowEQrep p482v33 p) fpt)
      ff
  

gavMinAddPtrep pavMinAddPtrep p =
  T.fun2 aavMinAddPtrep pavMinAddPtrep p havMinAddPtrep
  where
  
  havMinAddPtrep fx fys p =
    T.cguard p485v9 p (T.ap2 p485v9 p (gavAboveMin1rep p485v9 p) fx fys)
      (\ p -> T.projection p485v30 p fys)
      (\ p ->
        T.cguard p486v6 p (gotherwise p486v6 p)
          (\ p ->
            T.con2 p486v19 p T.Cons T.aCons fx
              (T.ap1 p0v0 p
                (T.ap2 p486v20 p (TPrelude.g_foldr p486v20 p)
                  (T.fun2 T.mkLambda p486v20 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 fy p =
                            T.ap1 p486v20 p
                              (T.ap2 p486v20 p (TPrelude.g_filter p486v20 p)
                                (T.ap1 p486v34 p (gnot p486v34 p)
                                  (T.ap2 p486v42 p (gavBelowEQrep p486v42 p) fx
                                    fy))
                                (T.pa1 T.Cons T.cn1 p486v20 p T.aCons fy)) f_y
                          v0v0v1 _ p = T.projection p486v20 p f_y in (v0v0v1))
                        f_x)) fys) (T.fromExpList p0v0 p [])))
          (\ p -> T.fatal p))
  

gavMaxAddPtrep pavMaxAddPtrep p =
  T.fun2 aavMaxAddPtrep pavMaxAddPtrep p havMaxAddPtrep
  where
  
  havMaxAddPtrep fx fys p =
    T.cguard p489v9 p (T.ap2 p489v9 p (gavBelowMax0rep p489v9 p) fx fys)
      (\ p -> T.projection p489v30 p fys)
      (\ p ->
        T.cguard p490v6 p (gotherwise p490v6 p)
          (\ p ->
            T.con2 p490v19 p T.Cons T.aCons fx
              (T.ap1 p0v0 p
                (T.ap2 p490v20 p (TPrelude.g_foldr p490v20 p)
                  (T.fun2 T.mkLambda p490v20 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 fy p =
                            T.ap1 p490v20 p
                              (T.ap2 p490v20 p (TPrelude.g_filter p490v20 p)
                                (T.ap1 p490v34 p (gnot p490v34 p)
                                  (T.ap2 p490v42 p (gavBelowEQrep p490v42 p) fy
                                    fx))
                                (T.pa1 T.Cons T.cn1 p490v20 p T.aCons fy)) f_y
                          v0v0v1 _ p = T.projection p490v20 p f_y in (v0v0v1))
                        f_x)) fys) (T.fromExpList p0v0 p [])))
          (\ p -> T.fatal p))
  

gavMinrep pavMinrep p = T.constUse pavMinrep p savMinrep

savMinrep =
  T.constDef T.mkRoot aavMinrep
    (\ p ->
      T.ap2 p492v12 p (gfoldr p492v12 p) (gavMinAddPtrep p492v18 p)
        (T.con0 p492v32 p T.List T.aList))

gavMaxrep pavMaxrep p = T.constUse pavMaxrep p savMaxrep

savMaxrep =
  T.constDef T.mkRoot aavMaxrep
    (\ p ->
      T.ap2 p494v12 p (gfoldr p494v12 p) (gavMaxAddPtrep p494v18 p)
        (T.con0 p494v32 p T.List T.aList))

tAbstractVals2 = T.mkModule "AbstractVals2" "AbstractVals2.hs" Prelude.True

aavUncurry = T.mkVariable tAbstractVals2 280001 3 2 "avUncurry" Prelude.False

aavTopR = T.mkVariable tAbstractVals2 360001 3 1 "avTopR" Prelude.False

aavTopR_aux_2 =
  T.mkVariable tAbstractVals2 460001 3 1 "avTopR_aux_2" Prelude.False

aavTopR_aux = T.mkVariable tAbstractVals2 540001 3 1 "avTopR_aux" Prelude.False

aavBottomR = T.mkVariable tAbstractVals2 760001 3 1 "avBottomR" Prelude.False

aavBottomR_aux_2 =
  T.mkVariable tAbstractVals2 860001 3 1 "avBottomR_aux_2" Prelude.False

aavBottomR_aux =
  T.mkVariable tAbstractVals2 940001 3 1 "avBottomR_aux" Prelude.False

aavIsBottomR =
  T.mkVariable tAbstractVals2 1160001 3 1 "avIsBottomR" Prelude.False

aavIsBottomRep =
  T.mkVariable tAbstractVals2 1300001 3 1 "avIsBottomRep" Prelude.False

aavIsTopR = T.mkVariable tAbstractVals2 1430001 3 1 "avIsTopR" Prelude.False

aavIsTopRep = T.mkVariable tAbstractVals2 1570001 3 1 "avIsTopRep" Prelude.False

(+<<) = T.mkVariable tAbstractVals2 1750014 36 2 "<<" Prelude.False

aavLEQR_list =
  T.mkVariable tAbstractVals2 1980001 3 2 "avLEQR_list" Prelude.False

aavBelowEQfrel =
  T.mkVariable tAbstractVals2 2460001 3 2 "avBelowEQfrel" Prelude.False

aavBelowEQfrontier =
  T.mkVariable tAbstractVals2 2540001 3 2 "avBelowEQfrontier" Prelude.False

aavBelowEQrep =
  T.mkVariable tAbstractVals2 2750001 3 2 "avBelowEQrep" Prelude.False

(+\/) = T.mkVariable tAbstractVals2 2980016 36 2 "\\/" Prelude.False

aavLUBfrel = T.mkVariable tAbstractVals2 3180001 3 2 "avLUBfrel" Prelude.False

aavLUBfrontier =
  T.mkVariable tAbstractVals2 3260001 3 2 "avLUBfrontier" Prelude.False

aavLUBrep = T.mkVariable tAbstractVals2 3350001 3 2 "avLUBrep" Prelude.False

aavLUBmin1frontier =
  T.mkVariable tAbstractVals2 3480001 3 2 "avLUBmin1frontier" Prelude.False

aavLUBmax0frontier =
  T.mkVariable tAbstractVals2 3560001 3 2 "avLUBmax0frontier" Prelude.False

(+/\) = T.mkVariable tAbstractVals2 3640016 36 2 "/\\" Prelude.False

aavGLBfrel = T.mkVariable tAbstractVals2 3840001 3 2 "avGLBfrel" Prelude.False

aavGLBfrontier =
  T.mkVariable tAbstractVals2 3920001 3 2 "avGLBfrontier" Prelude.False

aavGLBrep = T.mkVariable tAbstractVals2 4010001 3 2 "avGLBrep" Prelude.False

aavGLBmax0frontier =
  T.mkVariable tAbstractVals2 4140001 3 2 "avGLBmax0frontier" Prelude.False

aavGLBmin1frontier =
  T.mkVariable tAbstractVals2 4220001 3 2 "avGLBmin1frontier" Prelude.False

aavBelowMax0frel =
  T.mkVariable tAbstractVals2 4340005 3 2 "avBelowMax0frel" Prelude.False

aavAboveMin1frel =
  T.mkVariable tAbstractVals2 4360005 3 2 "avAboveMin1frel" Prelude.False

aavMinAddPtfrel =
  T.mkVariable tAbstractVals2 4380001 3 2 "avMinAddPtfrel" Prelude.False

aavMaxAddPtfrel =
  T.mkVariable tAbstractVals2 4420001 3 2 "avMaxAddPtfrel" Prelude.False

aavMinfrel = T.mkVariable tAbstractVals2 4460001 3 0 "avMinfrel" Prelude.False

aavMaxfrel = T.mkVariable tAbstractVals2 4480001 3 0 "avMaxfrel" Prelude.False

aavBelowMax0R =
  T.mkVariable tAbstractVals2 4570005 3 2 "avBelowMax0R" Prelude.False

aavAboveMin1R =
  T.mkVariable tAbstractVals2 4590005 3 2 "avAboveMin1R" Prelude.False

aavMinAddPtR =
  T.mkVariable tAbstractVals2 4610001 3 2 "avMinAddPtR" Prelude.False

aavMaxAddPtR =
  T.mkVariable tAbstractVals2 4650001 3 2 "avMaxAddPtR" Prelude.False

aavMinR = T.mkVariable tAbstractVals2 4690001 3 0 "avMinR" Prelude.False

aavMaxR = T.mkVariable tAbstractVals2 4710001 3 0 "avMaxR" Prelude.False

aavBelowMax0rep =
  T.mkVariable tAbstractVals2 4800005 3 2 "avBelowMax0rep" Prelude.False

aavAboveMin1rep =
  T.mkVariable tAbstractVals2 4820005 3 2 "avAboveMin1rep" Prelude.False

aavMinAddPtrep =
  T.mkVariable tAbstractVals2 4840001 3 2 "avMinAddPtrep" Prelude.False

aavMaxAddPtrep =
  T.mkVariable tAbstractVals2 4880001 3 2 "avMaxAddPtrep" Prelude.False

aavMinrep = T.mkVariable tAbstractVals2 4920001 3 0 "avMinrep" Prelude.False

aavMaxrep = T.mkVariable tAbstractVals2 4940001 3 0 "avMaxrep" Prelude.False

a58v10lf = T.mkVariable tAbstractVals2 580010 3 0 "lf" Prelude.True

a59v10hf_domains =
  T.mkVariable tAbstractVals2 590010 3 0 "hf_domains" Prelude.True

a60v10hfs = T.mkVariable tAbstractVals2 600010 3 0 "hfs" Prelude.True

a65v10lf = T.mkVariable tAbstractVals2 650010 3 0 "lf" Prelude.True

a66v10hf_domains =
  T.mkVariable tAbstractVals2 660010 3 0 "hf_domains" Prelude.True

a67v10hfs = T.mkVariable tAbstractVals2 670010 3 0 "hfs" Prelude.True

a98v10lf = T.mkVariable tAbstractVals2 980010 3 0 "lf" Prelude.True

a99v10hf_domains =
  T.mkVariable tAbstractVals2 990010 3 0 "hf_domains" Prelude.True

a100v10hfs = T.mkVariable tAbstractVals2 1000010 3 0 "hfs" Prelude.True

a105v10lf = T.mkVariable tAbstractVals2 1050010 3 0 "lf" Prelude.True

a106v10hf_domains =
  T.mkVariable tAbstractVals2 1060010 3 0 "hf_domains" Prelude.True

a107v10hfs = T.mkVariable tAbstractVals2 1070010 3 0 "hfs" Prelude.True

a262v10outer = T.mkVariable tAbstractVals2 2620010 3 1 "outer" Prelude.True

a264v10inner = T.mkVariable tAbstractVals2 2640010 3 2 "inner" Prelude.True

p28v1 = T.mkSrcPos tAbstractVals2 280001

p28v32 = T.mkSrcPos tAbstractVals2 280032

p28v41 = T.mkSrcPos tAbstractVals2 280041

p29v32 = T.mkSrcPos tAbstractVals2 290032

p36v1 = T.mkSrcPos tAbstractVals2 360001

p36v28 = T.mkSrcPos tAbstractVals2 360028

p37v28 = T.mkSrcPos tAbstractVals2 370028

p37v33 = T.mkSrcPos tAbstractVals2 370033

p37v37 = T.mkSrcPos tAbstractVals2 370037

p38v28 = T.mkSrcPos tAbstractVals2 380028

p38v35 = T.mkSrcPos tAbstractVals2 380035

p38v39 = T.mkSrcPos tAbstractVals2 380039

p39v28 = T.mkSrcPos tAbstractVals2 390028

p39v33 = T.mkSrcPos tAbstractVals2 390033

p46v1 = T.mkSrcPos tAbstractVals2 460001

p47v6 = T.mkSrcPos tAbstractVals2 470006

p47v16 = T.mkSrcPos tAbstractVals2 470016

p47v28 = T.mkSrcPos tAbstractVals2 470028

p47v29 = T.mkSrcPos tAbstractVals2 470029

p47v37 = T.mkSrcPos tAbstractVals2 470037

p47v41 = T.mkSrcPos tAbstractVals2 470041

p47v57 = T.mkSrcPos tAbstractVals2 470057

p54v1 = T.mkSrcPos tAbstractVals2 540001

p55v6 = T.mkSrcPos tAbstractVals2 550006

p55v14 = T.mkSrcPos tAbstractVals2 550014

p58v10 = T.mkSrcPos tAbstractVals2 580010

p58v15 = T.mkSrcPos tAbstractVals2 580015

p59v10 = T.mkSrcPos tAbstractVals2 590010

p59v23 = T.mkSrcPos tAbstractVals2 590023

p59v28 = T.mkSrcPos tAbstractVals2 590028

p60v10 = T.mkSrcPos tAbstractVals2 600010

p60v16 = T.mkSrcPos tAbstractVals2 600016

p60v20 = T.mkSrcPos tAbstractVals2 600020

p60v31 = T.mkSrcPos tAbstractVals2 600031

p62v10 = T.mkSrcPos tAbstractVals2 620010

p62v15 = T.mkSrcPos tAbstractVals2 620015

p62v18 = T.mkSrcPos tAbstractVals2 620018

p65v10 = T.mkSrcPos tAbstractVals2 650010

p65v15 = T.mkSrcPos tAbstractVals2 650015

p66v10 = T.mkSrcPos tAbstractVals2 660010

p66v23 = T.mkSrcPos tAbstractVals2 660023

p66v28 = T.mkSrcPos tAbstractVals2 660028

p67v10 = T.mkSrcPos tAbstractVals2 670010

p67v16 = T.mkSrcPos tAbstractVals2 670016

p67v20 = T.mkSrcPos tAbstractVals2 670020

p67v31 = T.mkSrcPos tAbstractVals2 670031

p69v10 = T.mkSrcPos tAbstractVals2 690010

p69v15 = T.mkSrcPos tAbstractVals2 690015

p69v18 = T.mkSrcPos tAbstractVals2 690018

p69v21 = T.mkSrcPos tAbstractVals2 690021

p76v1 = T.mkSrcPos tAbstractVals2 760001

p76v31 = T.mkSrcPos tAbstractVals2 760031

p77v31 = T.mkSrcPos tAbstractVals2 770031

p78v31 = T.mkSrcPos tAbstractVals2 780031

p79v31 = T.mkSrcPos tAbstractVals2 790031

p79v36 = T.mkSrcPos tAbstractVals2 790036

p86v1 = T.mkSrcPos tAbstractVals2 860001

p87v6 = T.mkSrcPos tAbstractVals2 870006

p87v16 = T.mkSrcPos tAbstractVals2 870016

p87v28 = T.mkSrcPos tAbstractVals2 870028

p87v31 = T.mkSrcPos tAbstractVals2 870031

p87v32 = T.mkSrcPos tAbstractVals2 870032

p87v40 = T.mkSrcPos tAbstractVals2 870040

p87v44 = T.mkSrcPos tAbstractVals2 870044

p94v1 = T.mkSrcPos tAbstractVals2 940001

p95v6 = T.mkSrcPos tAbstractVals2 950006

p95v14 = T.mkSrcPos tAbstractVals2 950014

p98v10 = T.mkSrcPos tAbstractVals2 980010

p98v15 = T.mkSrcPos tAbstractVals2 980015

p99v10 = T.mkSrcPos tAbstractVals2 990010

p99v23 = T.mkSrcPos tAbstractVals2 990023

p99v28 = T.mkSrcPos tAbstractVals2 990028

p100v10 = T.mkSrcPos tAbstractVals2 1000010

p100v16 = T.mkSrcPos tAbstractVals2 1000016

p100v20 = T.mkSrcPos tAbstractVals2 1000020

p100v34 = T.mkSrcPos tAbstractVals2 1000034

p102v10 = T.mkSrcPos tAbstractVals2 1020010

p102v15 = T.mkSrcPos tAbstractVals2 1020015

p102v18 = T.mkSrcPos tAbstractVals2 1020018

p105v10 = T.mkSrcPos tAbstractVals2 1050010

p105v15 = T.mkSrcPos tAbstractVals2 1050015

p106v10 = T.mkSrcPos tAbstractVals2 1060010

p106v23 = T.mkSrcPos tAbstractVals2 1060023

p106v28 = T.mkSrcPos tAbstractVals2 1060028

p107v10 = T.mkSrcPos tAbstractVals2 1070010

p107v16 = T.mkSrcPos tAbstractVals2 1070016

p107v20 = T.mkSrcPos tAbstractVals2 1070020

p107v34 = T.mkSrcPos tAbstractVals2 1070034

p109v10 = T.mkSrcPos tAbstractVals2 1090010

p109v15 = T.mkSrcPos tAbstractVals2 1090015

p109v18 = T.mkSrcPos tAbstractVals2 1090018

p109v21 = T.mkSrcPos tAbstractVals2 1090021

p116v1 = T.mkSrcPos tAbstractVals2 1160001

p116v27 = T.mkSrcPos tAbstractVals2 1160027

p117v27 = T.mkSrcPos tAbstractVals2 1170027

p118v27 = T.mkSrcPos tAbstractVals2 1180027

p119v27 = T.mkSrcPos tAbstractVals2 1190027

p120v27 = T.mkSrcPos tAbstractVals2 1200027

p121v27 = T.mkSrcPos tAbstractVals2 1210027

p122v27 = T.mkSrcPos tAbstractVals2 1220027

p123v27 = T.mkSrcPos tAbstractVals2 1230027

p130v1 = T.mkSrcPos tAbstractVals2 1300001

p131v6 = T.mkSrcPos tAbstractVals2 1310006

p133v6 = T.mkSrcPos tAbstractVals2 1330006

p135v6 = T.mkSrcPos tAbstractVals2 1350006

p143v1 = T.mkSrcPos tAbstractVals2 1430001

p143v25 = T.mkSrcPos tAbstractVals2 1430025

p144v25 = T.mkSrcPos tAbstractVals2 1440025

p145v25 = T.mkSrcPos tAbstractVals2 1450025

p146v25 = T.mkSrcPos tAbstractVals2 1460025

p146v31 = T.mkSrcPos tAbstractVals2 1460031

p147v25 = T.mkSrcPos tAbstractVals2 1470025

p148v25 = T.mkSrcPos tAbstractVals2 1480025

p149v25 = T.mkSrcPos tAbstractVals2 1490025

p149v31 = T.mkSrcPos tAbstractVals2 1490031

p150v25 = T.mkSrcPos tAbstractVals2 1500025

p157v1 = T.mkSrcPos tAbstractVals2 1570001

p158v6 = T.mkSrcPos tAbstractVals2 1580006

p160v6 = T.mkSrcPos tAbstractVals2 1600006

p160v12 = T.mkSrcPos tAbstractVals2 1600012

p162v6 = T.mkSrcPos tAbstractVals2 1620006

p162v12 = T.mkSrcPos tAbstractVals2 1620012

p175v14 = T.mkSrcPos tAbstractVals2 1750014

p175v33 = T.mkSrcPos tAbstractVals2 1750033

p176v33 = T.mkSrcPos tAbstractVals2 1760033

p177v33 = T.mkSrcPos tAbstractVals2 1770033

p179v33 = T.mkSrcPos tAbstractVals2 1790033

p180v33 = T.mkSrcPos tAbstractVals2 1800033

p181v33 = T.mkSrcPos tAbstractVals2 1810033

p183v33 = T.mkSrcPos tAbstractVals2 1830033

p184v33 = T.mkSrcPos tAbstractVals2 1840033

p185v33 = T.mkSrcPos tAbstractVals2 1850033

p186v33 = T.mkSrcPos tAbstractVals2 1860033

p187v33 = T.mkSrcPos tAbstractVals2 1870033

p189v33 = T.mkSrcPos tAbstractVals2 1890033

p198v1 = T.mkSrcPos tAbstractVals2 1980001

p199v6 = T.mkSrcPos tAbstractVals2 1990006

p202v9 = T.mkSrcPos tAbstractVals2 2020009

p205v6 = T.mkSrcPos tAbstractVals2 2050006

p205v17 = T.mkSrcPos tAbstractVals2 2050017

p206v17 = T.mkSrcPos tAbstractVals2 2060017

p207v14 = T.mkSrcPos tAbstractVals2 2070014

p210v6 = T.mkSrcPos tAbstractVals2 2100006

p210v17 = T.mkSrcPos tAbstractVals2 2100017

p211v14 = T.mkSrcPos tAbstractVals2 2110014

p211v25 = T.mkSrcPos tAbstractVals2 2110025

p212v25 = T.mkSrcPos tAbstractVals2 2120025

p213v22 = T.mkSrcPos tAbstractVals2 2130022

p214v14 = T.mkSrcPos tAbstractVals2 2140014

p217v6 = T.mkSrcPos tAbstractVals2 2170006

p217v17 = T.mkSrcPos tAbstractVals2 2170017

p218v14 = T.mkSrcPos tAbstractVals2 2180014

p218v25 = T.mkSrcPos tAbstractVals2 2180025

p219v22 = T.mkSrcPos tAbstractVals2 2190022

p219v33 = T.mkSrcPos tAbstractVals2 2190033

p220v33 = T.mkSrcPos tAbstractVals2 2200033

p221v30 = T.mkSrcPos tAbstractVals2 2210030

p222v22 = T.mkSrcPos tAbstractVals2 2220022

p223v14 = T.mkSrcPos tAbstractVals2 2230014

p227v6 = T.mkSrcPos tAbstractVals2 2270006

p227v17 = T.mkSrcPos tAbstractVals2 2270017

p228v14 = T.mkSrcPos tAbstractVals2 2280014

p228v25 = T.mkSrcPos tAbstractVals2 2280025

p229v22 = T.mkSrcPos tAbstractVals2 2290022

p229v33 = T.mkSrcPos tAbstractVals2 2290033

p230v30 = T.mkSrcPos tAbstractVals2 2300030

p230v41 = T.mkSrcPos tAbstractVals2 2300041

p231v38 = T.mkSrcPos tAbstractVals2 2310038

p232v38 = T.mkSrcPos tAbstractVals2 2320038

p233v30 = T.mkSrcPos tAbstractVals2 2330030

p234v22 = T.mkSrcPos tAbstractVals2 2340022

p235v14 = T.mkSrcPos tAbstractVals2 2350014

p239v30 = T.mkSrcPos tAbstractVals2 2390030

p239v36 = T.mkSrcPos tAbstractVals2 2390036

p246v1 = T.mkSrcPos tAbstractVals2 2460001

p247v6 = T.mkSrcPos tAbstractVals2 2470006

p254v1 = T.mkSrcPos tAbstractVals2 2540001

p262v10 = T.mkSrcPos tAbstractVals2 2620010

p262v28 = T.mkSrcPos tAbstractVals2 2620028

p263v28 = T.mkSrcPos tAbstractVals2 2630028

p263v31 = T.mkSrcPos tAbstractVals2 2630031

p263v48 = T.mkSrcPos tAbstractVals2 2630048

p263v62 = T.mkSrcPos tAbstractVals2 2630062

p264v10 = T.mkSrcPos tAbstractVals2 2640010

p264v28 = T.mkSrcPos tAbstractVals2 2640028

p265v28 = T.mkSrcPos tAbstractVals2 2650028

p265v34 = T.mkSrcPos tAbstractVals2 2650034

p265v56 = T.mkSrcPos tAbstractVals2 2650056

p265v66 = T.mkSrcPos tAbstractVals2 2650066

p267v10 = T.mkSrcPos tAbstractVals2 2670010

p275v1 = T.mkSrcPos tAbstractVals2 2750001

p276v6 = T.mkSrcPos tAbstractVals2 2760006

p279v32 = T.mkSrcPos tAbstractVals2 2790032

p279v6 = T.mkSrcPos tAbstractVals2 2790006

p280v6 = T.mkSrcPos tAbstractVals2 2800006

p280v17 = T.mkSrcPos tAbstractVals2 2800017

p283v32 = T.mkSrcPos tAbstractVals2 2830032

p283v6 = T.mkSrcPos tAbstractVals2 2830006

p284v32 = T.mkSrcPos tAbstractVals2 2840032

p284v6 = T.mkSrcPos tAbstractVals2 2840006

p285v6 = T.mkSrcPos tAbstractVals2 2850006

p285v17 = T.mkSrcPos tAbstractVals2 2850017

p298v16 = T.mkSrcPos tAbstractVals2 2980016

p298v37 = T.mkSrcPos tAbstractVals2 2980037

p299v37 = T.mkSrcPos tAbstractVals2 2990037

p301v37 = T.mkSrcPos tAbstractVals2 3010037

p302v37 = T.mkSrcPos tAbstractVals2 3020037

p303v37 = T.mkSrcPos tAbstractVals2 3030037

p303v42 = T.mkSrcPos tAbstractVals2 3030042

p303v54 = T.mkSrcPos tAbstractVals2 3030054

p305v37 = T.mkSrcPos tAbstractVals2 3050037

p306v37 = T.mkSrcPos tAbstractVals2 3060037

p307v37 = T.mkSrcPos tAbstractVals2 3070037

p308v37 = T.mkSrcPos tAbstractVals2 3080037

p308v44 = T.mkSrcPos tAbstractVals2 3080044

p308v56 = T.mkSrcPos tAbstractVals2 3080056

p309v37 = T.mkSrcPos tAbstractVals2 3090037

p311v37 = T.mkSrcPos tAbstractVals2 3110037

p311v42 = T.mkSrcPos tAbstractVals2 3110042

p318v1 = T.mkSrcPos tAbstractVals2 3180001

p319v6 = T.mkSrcPos tAbstractVals2 3190006

p319v14 = T.mkSrcPos tAbstractVals2 3190014

p319v26 = T.mkSrcPos tAbstractVals2 3190026

p326v1 = T.mkSrcPos tAbstractVals2 3260001

p328v6 = T.mkSrcPos tAbstractVals2 3280006

p328v20 = T.mkSrcPos tAbstractVals2 3280020

p328v48 = T.mkSrcPos tAbstractVals2 3280048

p335v1 = T.mkSrcPos tAbstractVals2 3350001

p336v6 = T.mkSrcPos tAbstractVals2 3360006

p336v14 = T.mkSrcPos tAbstractVals2 3360014

p338v6 = T.mkSrcPos tAbstractVals2 3380006

p338v12 = T.mkSrcPos tAbstractVals2 3380012

p338v36 = T.mkSrcPos tAbstractVals2 3380036

p338v47 = T.mkSrcPos tAbstractVals2 3380047

p340v6 = T.mkSrcPos tAbstractVals2 3400006

p340v12 = T.mkSrcPos tAbstractVals2 3400012

p340v36 = T.mkSrcPos tAbstractVals2 3400036

p341v12 = T.mkSrcPos tAbstractVals2 3410012

p341v23 = T.mkSrcPos tAbstractVals2 3410023

p348v1 = T.mkSrcPos tAbstractVals2 3480001

p349v6 = T.mkSrcPos tAbstractVals2 3490006

p349v12 = T.mkSrcPos tAbstractVals2 3490012

p349v18 = T.mkSrcPos tAbstractVals2 3490018

p356v1 = T.mkSrcPos tAbstractVals2 3560001

p357v6 = T.mkSrcPos tAbstractVals2 3570006

p357v12 = T.mkSrcPos tAbstractVals2 3570012

p0v0 = T.mkSrcPos tAbstractVals2 0

p357v22 = T.mkSrcPos tAbstractVals2 3570022

p357v27 = T.mkSrcPos tAbstractVals2 3570027

p364v16 = T.mkSrcPos tAbstractVals2 3640016

p364v39 = T.mkSrcPos tAbstractVals2 3640039

p365v39 = T.mkSrcPos tAbstractVals2 3650039

p367v39 = T.mkSrcPos tAbstractVals2 3670039

p368v39 = T.mkSrcPos tAbstractVals2 3680039

p368v44 = T.mkSrcPos tAbstractVals2 3680044

p368v56 = T.mkSrcPos tAbstractVals2 3680056

p369v39 = T.mkSrcPos tAbstractVals2 3690039

p371v39 = T.mkSrcPos tAbstractVals2 3710039

p372v39 = T.mkSrcPos tAbstractVals2 3720039

p373v39 = T.mkSrcPos tAbstractVals2 3730039

p374v39 = T.mkSrcPos tAbstractVals2 3740039

p374v46 = T.mkSrcPos tAbstractVals2 3740046

p374v58 = T.mkSrcPos tAbstractVals2 3740058

p375v39 = T.mkSrcPos tAbstractVals2 3750039

p377v39 = T.mkSrcPos tAbstractVals2 3770039

p377v44 = T.mkSrcPos tAbstractVals2 3770044

p384v1 = T.mkSrcPos tAbstractVals2 3840001

p385v6 = T.mkSrcPos tAbstractVals2 3850006

p385v14 = T.mkSrcPos tAbstractVals2 3850014

p385v26 = T.mkSrcPos tAbstractVals2 3850026

p392v1 = T.mkSrcPos tAbstractVals2 3920001

p394v6 = T.mkSrcPos tAbstractVals2 3940006

p394v20 = T.mkSrcPos tAbstractVals2 3940020

p394v48 = T.mkSrcPos tAbstractVals2 3940048

p401v1 = T.mkSrcPos tAbstractVals2 4010001

p402v6 = T.mkSrcPos tAbstractVals2 4020006

p402v14 = T.mkSrcPos tAbstractVals2 4020014

p404v6 = T.mkSrcPos tAbstractVals2 4040006

p404v12 = T.mkSrcPos tAbstractVals2 4040012

p404v36 = T.mkSrcPos tAbstractVals2 4040036

p404v47 = T.mkSrcPos tAbstractVals2 4040047

p406v6 = T.mkSrcPos tAbstractVals2 4060006

p406v12 = T.mkSrcPos tAbstractVals2 4060012

p406v36 = T.mkSrcPos tAbstractVals2 4060036

p407v12 = T.mkSrcPos tAbstractVals2 4070012

p407v23 = T.mkSrcPos tAbstractVals2 4070023

p414v1 = T.mkSrcPos tAbstractVals2 4140001

p415v6 = T.mkSrcPos tAbstractVals2 4150006

p415v12 = T.mkSrcPos tAbstractVals2 4150012

p415v18 = T.mkSrcPos tAbstractVals2 4150018

p422v1 = T.mkSrcPos tAbstractVals2 4220001

p423v6 = T.mkSrcPos tAbstractVals2 4230006

p423v12 = T.mkSrcPos tAbstractVals2 4230012

p423v22 = T.mkSrcPos tAbstractVals2 4230022

p423v27 = T.mkSrcPos tAbstractVals2 4230027

p434v5 = T.mkSrcPos tAbstractVals2 4340005

p434v26 = T.mkSrcPos tAbstractVals2 4340026

p434v37 = T.mkSrcPos tAbstractVals2 4340037

p436v5 = T.mkSrcPos tAbstractVals2 4360005

p436v26 = T.mkSrcPos tAbstractVals2 4360026

p436v34 = T.mkSrcPos tAbstractVals2 4360034

p438v1 = T.mkSrcPos tAbstractVals2 4380001

p439v9 = T.mkSrcPos tAbstractVals2 4390009

p439v31 = T.mkSrcPos tAbstractVals2 4390031

p440v6 = T.mkSrcPos tAbstractVals2 4400006

p440v19 = T.mkSrcPos tAbstractVals2 4400019

p440v20 = T.mkSrcPos tAbstractVals2 4400020

p440v34 = T.mkSrcPos tAbstractVals2 4400034

p440v42 = T.mkSrcPos tAbstractVals2 4400042

p442v1 = T.mkSrcPos tAbstractVals2 4420001

p443v9 = T.mkSrcPos tAbstractVals2 4430009

p443v31 = T.mkSrcPos tAbstractVals2 4430031

p444v6 = T.mkSrcPos tAbstractVals2 4440006

p444v19 = T.mkSrcPos tAbstractVals2 4440019

p444v20 = T.mkSrcPos tAbstractVals2 4440020

p444v34 = T.mkSrcPos tAbstractVals2 4440034

p444v42 = T.mkSrcPos tAbstractVals2 4440042

p446v1 = T.mkSrcPos tAbstractVals2 4460001

p446v13 = T.mkSrcPos tAbstractVals2 4460013

p446v19 = T.mkSrcPos tAbstractVals2 4460019

p446v34 = T.mkSrcPos tAbstractVals2 4460034

p448v1 = T.mkSrcPos tAbstractVals2 4480001

p448v13 = T.mkSrcPos tAbstractVals2 4480013

p448v19 = T.mkSrcPos tAbstractVals2 4480019

p448v34 = T.mkSrcPos tAbstractVals2 4480034

p457v5 = T.mkSrcPos tAbstractVals2 4570005

p457v23 = T.mkSrcPos tAbstractVals2 4570023

p457v33 = T.mkSrcPos tAbstractVals2 4570033

p459v5 = T.mkSrcPos tAbstractVals2 4590005

p459v23 = T.mkSrcPos tAbstractVals2 4590023

p459v30 = T.mkSrcPos tAbstractVals2 4590030

p461v1 = T.mkSrcPos tAbstractVals2 4610001

p462v9 = T.mkSrcPos tAbstractVals2 4620009

p462v28 = T.mkSrcPos tAbstractVals2 4620028

p463v6 = T.mkSrcPos tAbstractVals2 4630006

p463v19 = T.mkSrcPos tAbstractVals2 4630019

p463v20 = T.mkSrcPos tAbstractVals2 4630020

p463v34 = T.mkSrcPos tAbstractVals2 4630034

p463v41 = T.mkSrcPos tAbstractVals2 4630041

p465v1 = T.mkSrcPos tAbstractVals2 4650001

p466v9 = T.mkSrcPos tAbstractVals2 4660009

p466v28 = T.mkSrcPos tAbstractVals2 4660028

p467v6 = T.mkSrcPos tAbstractVals2 4670006

p467v19 = T.mkSrcPos tAbstractVals2 4670019

p467v20 = T.mkSrcPos tAbstractVals2 4670020

p467v34 = T.mkSrcPos tAbstractVals2 4670034

p467v41 = T.mkSrcPos tAbstractVals2 4670041

p469v1 = T.mkSrcPos tAbstractVals2 4690001

p469v10 = T.mkSrcPos tAbstractVals2 4690010

p469v16 = T.mkSrcPos tAbstractVals2 4690016

p469v28 = T.mkSrcPos tAbstractVals2 4690028

p471v1 = T.mkSrcPos tAbstractVals2 4710001

p471v10 = T.mkSrcPos tAbstractVals2 4710010

p471v16 = T.mkSrcPos tAbstractVals2 4710016

p471v28 = T.mkSrcPos tAbstractVals2 4710028

p480v5 = T.mkSrcPos tAbstractVals2 4800005

p480v25 = T.mkSrcPos tAbstractVals2 4800025

p480v36 = T.mkSrcPos tAbstractVals2 4800036

p482v5 = T.mkSrcPos tAbstractVals2 4820005

p482v25 = T.mkSrcPos tAbstractVals2 4820025

p482v33 = T.mkSrcPos tAbstractVals2 4820033

p484v1 = T.mkSrcPos tAbstractVals2 4840001

p485v9 = T.mkSrcPos tAbstractVals2 4850009

p485v30 = T.mkSrcPos tAbstractVals2 4850030

p486v6 = T.mkSrcPos tAbstractVals2 4860006

p486v19 = T.mkSrcPos tAbstractVals2 4860019

p486v20 = T.mkSrcPos tAbstractVals2 4860020

p486v34 = T.mkSrcPos tAbstractVals2 4860034

p486v42 = T.mkSrcPos tAbstractVals2 4860042

p488v1 = T.mkSrcPos tAbstractVals2 4880001

p489v9 = T.mkSrcPos tAbstractVals2 4890009

p489v30 = T.mkSrcPos tAbstractVals2 4890030

p490v6 = T.mkSrcPos tAbstractVals2 4900006

p490v19 = T.mkSrcPos tAbstractVals2 4900019

p490v20 = T.mkSrcPos tAbstractVals2 4900020

p490v34 = T.mkSrcPos tAbstractVals2 4900034

p490v42 = T.mkSrcPos tAbstractVals2 4900042

p492v1 = T.mkSrcPos tAbstractVals2 4920001

p492v12 = T.mkSrcPos tAbstractVals2 4920012

p492v18 = T.mkSrcPos tAbstractVals2 4920018

p492v32 = T.mkSrcPos tAbstractVals2 4920032

p494v1 = T.mkSrcPos tAbstractVals2 4940001

p494v12 = T.mkSrcPos tAbstractVals2 4940012

p494v18 = T.mkSrcPos tAbstractVals2 4940018

p494v32 = T.mkSrcPos tAbstractVals2 4940032
