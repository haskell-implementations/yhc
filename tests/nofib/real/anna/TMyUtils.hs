module TMyUtils
  (gmyFail,gpanic,gmySubtract,gmyZipWith2,gmyZip2,gmyZipWith3,gmyZip3
    ,gmyZipWith4,gmyZip4,gmyZipWith5,gmyZip5,gmyAndWith2,gmyAny,gmyAll,gmyAnd
    ,gmyOr,gmyListVariants,gmyCartesianProduct,gmySeq,gmyIntsFromTo,gmyIntsFrom
    ,(!##)) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 

gmyFail pmyFail p =
  T.fun1 amyFail pmyFail p hmyFail
  where
  
  hmyFail fmsg p =
    T.ap1 p15v6 p (gerror p15v6 p)
      (T.ap2 p15v18 p (p15v18 !++ p) (T.fromLitString p15v13 p "\n")
        (T.ap2 p15v25 p (p15v25 !++ p) fmsg (T.fromLitString p15v28 p "\n")))
  

gpanic ppanic p =
  T.fun1 apanic ppanic p hpanic
  where
  
  hpanic fmsg p =
    T.ap1 p18v6 p (gerror p18v6 p)
      (T.ap2 p18v55 p (p18v55 !++ p)
        (T.fromLitString p18v13 p "\nPanic! (the `impossible' happened):\n")
        (T.ap2 p18v62 p (p18v62 !++ p) fmsg (T.fromLitString p18v65 p "\n")))
  

gmySubtract :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int (T.Fun Int Int))

gmySubtract pmySubtract p =
  T.fun2 amySubtract pmySubtract p hmySubtract
  where
  
  hmySubtract fx fy p = T.ap2 p25v20 p (p25v20 !- p) fy fx
  

gmyZipWith2 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.Fun b c))
          (T.Fun (T.List a) (T.Fun (T.List b) (T.List c))))

gmyZipWith2 pmyZipWith2 p =
  T.fun3 amyZipWith2 pmyZipWith2 p hmyZipWith2
  where
  
  hmyZipWith2 ff (T.R T.List _) (T.R T.List _) p =
    T.con0 p32v30 p T.List T.aList
  hmyZipWith2 ff (T.R (T.Cons fa fas) _) (T.R (T.Cons fb fbs) _) p =
    T.con2 p33v36 p T.Cons T.aCons (T.ap2 p33v30 p ff fa fb)
      (T.ap3 p33v38 p (gmyZipWith2 p33v38 p) ff fas fbs)
  hmyZipWith2 _ _ _ p =
    T.ap1 p34v30 p (gpanic p34v30 p)
      (T.fromLitString p34v36 p "myZipWith2: unequal lists")
  

gmyZip2 pmyZip2 p = T.constUse pmyZip2 p smyZip2

smyZip2 =
  T.constDef T.mkRoot amyZip2
    (\ p ->
      T.ap1 p36v10 p (gmyZipWith2 p36v10 p)
        (T.fun2 T.mkLambda p36v22 p
          (\ fa fb p -> T.con2 p36v30 p T.Tuple2 T.aTuple2 fa fb)))

gmyZipWith3 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.Fun b (T.Fun c d)))
          (T.Fun (T.List a) (T.Fun (T.List b) (T.Fun (T.List c) (T.List d)))))

gmyZipWith3 pmyZipWith3 p =
  T.fun4 amyZipWith3 pmyZipWith3 p hmyZipWith3
  where
  
  hmyZipWith3 ff (T.R T.List _) (T.R T.List _) (T.R T.List _) p =
    T.con0 p43v25 p T.List T.aList
  hmyZipWith3 ff (T.R (T.Cons fa fas) _) (T.R (T.Cons fb fbs) _)
    (T.R (T.Cons fc fcs) _) p =
    T.con2 p44v45 p T.Cons T.aCons (T.ap3 p44v37 p ff fa fb fc)
      (T.ap4 p44v47 p (gmyZipWith3 p44v47 p) ff fas fbs fcs)
  hmyZipWith3 _ _ _ _ p =
    T.ap1 p45v37 p (gpanic p45v37 p)
      (T.fromLitString p45v43 p "myZipWith3: unequal lists")
  

gmyZip3 pmyZip3 p = T.constUse pmyZip3 p smyZip3

smyZip3 =
  T.constDef T.mkRoot amyZip3
    (\ p ->
      T.ap1 p47v10 p (gmyZipWith3 p47v10 p)
        (T.fun3 T.mkLambda p47v22 p
          (\ fa fb fc p -> T.con3 p47v32 p T.Tuple3 T.aTuple3 fa fb fc)))

gmyZipWith4 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.Fun b (T.Fun c (T.Fun d e))))
          (T.Fun (T.List a)
            (T.Fun (T.List b)
              (T.Fun (T.List c) (T.Fun (T.List d) (T.List e))))))

gmyZipWith4 pmyZipWith4 p =
  T.fun5 amyZipWith4 pmyZipWith4 p hmyZipWith4
  where
  
  hmyZipWith4 ff (T.R T.List _) (T.R T.List _) (T.R T.List _) (T.R T.List _) p =
    T.con0 p54v28 p T.List T.aList
  hmyZipWith4 ff (T.R (T.Cons fa fas) _) (T.R (T.Cons fb fbs) _)
    (T.R (T.Cons fc fcs) _) (T.R (T.Cons fd fds) _) p =
    T.con2 p55v54 p T.Cons T.aCons (T.ap4 p55v44 p ff fa fb fc fd)
      (T.ap5 p55v56 p (gmyZipWith4 p55v56 p) ff fas fbs fcs fds)
  hmyZipWith4 _ _ _ _ _ p =
    T.ap1 p56v44 p (gpanic p56v44 p)
      (T.fromLitString p56v50 p "myZipWith4: unequal lists")
  

gmyZip4 pmyZip4 p = T.constUse pmyZip4 p smyZip4

smyZip4 =
  T.constDef T.mkRoot amyZip4
    (\ p ->
      T.ap1 p58v10 p (gmyZipWith4 p58v10 p)
        (T.fun4 T.mkLambda p58v22 p
          (\ fa fb fc fd p -> T.con4 p58v34 p T.Tuple4 T.aTuple4 fa fb fc fd)))

gmyZipWith5 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.Fun b (T.Fun c (T.Fun d (T.Fun e f)))))
          (T.Fun (T.List a)
            (T.Fun (T.List b)
              (T.Fun (T.List c)
                (T.Fun (T.List d) (T.Fun (T.List e) (T.List f)))))))

gmyZipWith5 pmyZipWith5 p =
  T.fun6 amyZipWith5 pmyZipWith5 p hmyZipWith5
  where
  
  hmyZipWith5 ff (T.R T.List _) (T.R T.List _) (T.R T.List _) (T.R T.List _)
    (T.R T.List _) p =
    T.con0 p66v31 p T.List T.aList
  hmyZipWith5 ff (T.R (T.Cons fa fas) _) (T.R (T.Cons fb fbs) _)
    (T.R (T.Cons fc fcs) _) (T.R (T.Cons fd fds) _) (T.R (T.Cons fe fes) _) p =
    T.con2 p68v18 p T.Cons T.aCons (T.ap5 p68v6 p ff fa fb fc fd fe)
      (T.ap6 p68v20 p (gmyZipWith5 p68v20 p) ff fas fbs fcs fds fes)
  hmyZipWith5 _ _ _ _ _ _ p =
    T.ap1 p70v6 p (gpanic p70v6 p)
      (T.fromLitString p70v12 p "myZipWith5: unequal lists")
  

gmyZip5 pmyZip5 p = T.constUse pmyZip5 p smyZip5

smyZip5 =
  T.constDef T.mkRoot amyZip5
    (\ p ->
      T.ap1 p72v10 p (gmyZipWith5 p72v10 p)
        (T.fun5 T.mkLambda p72v22 p
          (\ fa fb fc fd fe p ->
            T.con5 p72v36 p T.Tuple5 T.aTuple5 fa fb fc fd fe)))

gmyAndWith2 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun a (T.Fun b Bool))
          (T.Fun (T.List a) (T.Fun (T.List b) Bool)))

gmyAndWith2 pmyAndWith2 p =
  T.fun3 amyAndWith2 pmyAndWith2 p hmyAndWith2
  where
  
  hmyAndWith2 ff (T.R T.List _) (T.R T.List _) p = T.con0 p80v6 p True aTrue
  hmyAndWith2 ff (T.R (T.Cons fa fas) _) (T.R (T.Cons fb fbs) _) p =
    T.cif p83v6 p (T.ap2 p83v13 p ff fa fb)
      (\ p -> T.ap3 p84v13 p (gmyAndWith2 p84v13 p) ff fas fbs)
      (\ p -> T.con0 p85v13 p False aFalse)
  hmyAndWith2 _ _ _ p =
    T.ap1 p88v6 p (gpanic p88v6 p)
      (T.fromLitString p88v12 p "myAndWith2: unequal lists")
  

gmyAny,gmyAll ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.Fun a Bool) (T.Fun (T.List a) Bool))

gmyAny pmyAny p =
  T.fun2 amyAny pmyAny p hmyAny
  where
  
  hmyAny fp (T.R T.List _) p = T.con0 p95v20 p False aFalse
  hmyAny fp (T.R (T.Cons fx fxs) _) p =
    T.cif p96v20 p (T.ap1 p96v23 p fp fx) (\ p -> T.con0 p96v32 p True aTrue)
      (\ p -> T.ap2 p96v42 p (gmyAny p96v42 p) fp fxs)
  hmyAny _ _ p = T.fatal p
  

gmyAll pmyAll p =
  T.fun2 amyAll pmyAll p hmyAll
  where
  
  hmyAll fp (T.R T.List _) p = T.con0 p98v20 p True aTrue
  hmyAll fp (T.R (T.Cons fx fxs) _) p =
    T.cif p99v20 p (T.ap1 p99v23 p fp fx)
      (\ p -> T.ap2 p99v32 p (gmyAll p99v32 p) fp fxs)
      (\ p -> T.con0 p99v48 p False aFalse)
  hmyAll _ _ p = T.fatal p
  

gmyAnd,gmyOr :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List Bool) Bool)

gmyAnd pmyAnd p =
  T.fun1 amyAnd pmyAnd p hmyAnd
  where
  
  hmyAnd (T.R T.List _) p = T.con0 p106v19 p True aTrue
  hmyAnd (T.R (T.Cons fx fxs) _) p =
    T.cif p107v19 p fx (\ p -> T.ap1 p107v29 p (gmyAnd p107v29 p) fxs)
      (\ p -> T.con0 p107v43 p False aFalse)
  hmyAnd _ p = T.fatal p
  

gmyOr pmyOr p =
  T.fun1 amyOr pmyOr p hmyOr
  where
  
  hmyOr (T.R T.List _) p = T.con0 p109v19 p False aFalse
  hmyOr (T.R (T.Cons fx fxs) _) p =
    T.cif p110v19 p fx (\ p -> T.con0 p110v29 p True aTrue)
      (\ p -> T.ap1 p110v39 p (gmyOr p110v39 p) fxs)
  hmyOr _ p = T.fatal p
  

gmyListVariants ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R (T.Fun (T.List a) (T.Fun (T.List (T.List a)) (T.List (T.List a))))

gmyListVariants pmyListVariants p =
  T.fun2 amyListVariants pmyListVariants p hmyListVariants
  where
  
  hmyListVariants (T.R T.List _) (T.R T.List _) p =
    T.con0 p117v24 p T.List T.aList
  hmyListVariants (T.R (T.Cons fx fxs) _) (T.R (T.Cons frs frss) _) p =
    T.ap2 p120v29 p (p120v29 !++ p)
      (T.ap2 p120v6 p (gmap p120v6 p)
        (T.ap1 p120v12 p
          (T.ap1 p120v12 p (gflip p120v12 p)
            (T.pa0 T.Cons T.cn2 p120v18 p T.aCons)) fxs) frs)
      (T.ap2 p120v32 p (gmap p120v32 p)
        (T.pa1 T.Cons T.cn1 p120v38 p T.aCons fx)
        (T.ap2 p120v42 p (gmyListVariants p120v42 p) fxs frss))
  hmyListVariants _ _ p =
    T.ap1 p122v22 p (gpanic p122v22 p)
      (T.fromLitString p122v28 p "myListVariants: unequal lists")
  

gmyCartesianProduct ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List (T.List a)) (T.List (T.List a)))

gmyCartesianProduct pmyCartesianProduct p =
  T.fun1 amyCartesianProduct pmyCartesianProduct p hmyCartesianProduct
  where
  
  hmyCartesianProduct (T.R T.List _) p =
    T.fromExpList p130v6 p [T.con0 p130v7 p T.List T.aList]
  hmyCartesianProduct (T.R (T.Cons fxs fxss) _) p =
    let
      gg pg p =
        T.fun2 a133v10g pg p hg
        where
        
        hg fas fbs p =
          T.ap2 p133v20 p (gmap p133v20 p)
            (T.ap2 p133v25 p (TPrelude.gflip p133v25 p)
              (T.pa0 T.Cons T.cn2 p133v25 p T.aCons) fbs) fas
         in
      (T.ap1 p135v10 p (gconcat p135v10 p)
        (T.ap2 p135v18 p (gmap p135v18 p) (T.ap1 p135v23 p (gg p135v23 p) fxs)
          (T.ap1 p135v30 p (gmyCartesianProduct p135v30 p) fxss)))
  hmyCartesianProduct _ p = T.fatal p
  

gmySeq :: Eq a => T.RefSrcPos -> T.RefExp -> T.R (T.Fun a (T.Fun b b))

gmySeq pmySeq p =
  T.fun2 amySeq pmySeq p hmySeq
  where
  
  hmySeq fx fy p =
    T.cguard p142v15 p (T.ap2 p142v15 p (p142v15 !== p) fx fx)
      (\ p -> T.projection p142v22 p fy) (\ p -> T.fatal p)
  

gmyIntsFromTo ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int (T.Fun Int (T.List Int)))

gmyIntsFromTo pmyIntsFromTo p =
  T.fun2 amyIntsFromTo pmyIntsFromTo p hmyIntsFromTo
  where
  
  hmyIntsFromTo fn fm p =
    T.cif p150v6 p (T.ap2 p150v15 p (p150v15 !> p) fn fm)
      (\ p -> T.con0 p151v13 p T.List T.aList)
      (\ p ->
        T.con2 p152v15 p T.Cons T.aCons fn
          (T.ap2 p152v17 p (gmyIntsFromTo p152v17 p)
            (T.ap2 p152v33 p (p152v33 !+ p) fn
              (T.ap1 p152v36 p (TPreludeBasic.gfromInteger p152v36 p)
                  (T.conInteger p152v36 p 1)
                :: T.R Int)) fm))
  

gmyIntsFrom :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int (T.List Int))

gmyIntsFrom pmyIntsFrom p =
  T.fun1 amyIntsFrom pmyIntsFrom p hmyIntsFrom
  where
  
  hmyIntsFrom fn p =
    T.con2 p159v18 p T.Cons T.aCons fn
      (T.ap1 p159v20 p (gmyIntsFrom p159v20 p)
        (T.ap2 p159v34 p (p159v34 !+ p) fn
          (T.ap1 p159v37 p (TPreludeBasic.gfromInteger p159v37 p)
              (T.conInteger p159v37 p 1)
            :: T.R Int)))
  

(!##) :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List b) (T.Fun Int b))

(!##) (%##) p =
  T.fun2 (+##) (%##) p (*##)
  where
  
  (*##) (T.R T.List _) fn p =
    T.ap1 p167v6 p (gpanic p167v6 p) (T.fromLitString p167v12 p "(##) (1)")
  (*##) (T.R (T.Cons fx fxs) _) fn p =
    T.cif p169v6 p
      (T.ap2 p169v11 p (p169v11 !== p) fn
        (T.ap1 p169v15 p (TPreludeBasic.gfromInteger p169v15 p)
            (T.conInteger p169v15 p 0)
          :: T.R Int)) (\ p -> T.projection p169v30 p fx)
      (\ p ->
        T.ap2 p169v40 p (p169v40 !## p) fxs
          (T.ap2 p169v46 p (p169v46 !- p) fn
            (T.ap1 p169v49 p (TPreludeBasic.gfromInteger p169v49 p)
                (T.conInteger p169v49 p 1)
              :: T.R Int)))
  (*##) _ _ p = T.fatal p
  

tMyUtils = T.mkModule "MyUtils" "MyUtils.hs" Prelude.True

amyFail = T.mkVariable tMyUtils 140001 3 1 "myFail" Prelude.False

apanic = T.mkVariable tMyUtils 170001 3 1 "panic" Prelude.False

amySubtract = T.mkVariable tMyUtils 250001 3 2 "mySubtract" Prelude.False

amyZipWith2 = T.mkVariable tMyUtils 320001 3 3 "myZipWith2" Prelude.False

amyZip2 = T.mkVariable tMyUtils 360001 3 0 "myZip2" Prelude.False

amyZipWith3 = T.mkVariable tMyUtils 430001 3 4 "myZipWith3" Prelude.False

amyZip3 = T.mkVariable tMyUtils 470001 3 0 "myZip3" Prelude.False

amyZipWith4 = T.mkVariable tMyUtils 540001 3 5 "myZipWith4" Prelude.False

amyZip4 = T.mkVariable tMyUtils 580001 3 0 "myZip4" Prelude.False

amyZipWith5 = T.mkVariable tMyUtils 660001 3 6 "myZipWith5" Prelude.False

amyZip5 = T.mkVariable tMyUtils 720001 3 0 "myZip5" Prelude.False

amyAndWith2 = T.mkVariable tMyUtils 790001 3 3 "myAndWith2" Prelude.False

amyAny = T.mkVariable tMyUtils 950001 3 2 "myAny" Prelude.False

amyAll = T.mkVariable tMyUtils 980001 3 2 "myAll" Prelude.False

amyAnd = T.mkVariable tMyUtils 1060001 3 1 "myAnd" Prelude.False

amyOr = T.mkVariable tMyUtils 1090001 3 1 "myOr" Prelude.False

amyListVariants =
  T.mkVariable tMyUtils 1170001 3 2 "myListVariants" Prelude.False

amyCartesianProduct =
  T.mkVariable tMyUtils 1290001 3 1 "myCartesianProduct" Prelude.False

amySeq = T.mkVariable tMyUtils 1420001 3 2 "mySeq" Prelude.False

amyIntsFromTo = T.mkVariable tMyUtils 1490001 3 2 "myIntsFromTo" Prelude.False

amyIntsFrom = T.mkVariable tMyUtils 1590001 3 1 "myIntsFrom" Prelude.False

(+##) = T.mkVariable tMyUtils 1660004 38 2 "##" Prelude.False

a133v10g = T.mkVariable tMyUtils 1330010 3 2 "g" Prelude.True

p14v1 = T.mkSrcPos tMyUtils 140001

p15v6 = T.mkSrcPos tMyUtils 150006

p15v18 = T.mkSrcPos tMyUtils 150018

p15v13 = T.mkSrcPos tMyUtils 150013

p15v25 = T.mkSrcPos tMyUtils 150025

p15v28 = T.mkSrcPos tMyUtils 150028

p17v1 = T.mkSrcPos tMyUtils 170001

p18v6 = T.mkSrcPos tMyUtils 180006

p18v55 = T.mkSrcPos tMyUtils 180055

p18v13 = T.mkSrcPos tMyUtils 180013

p18v62 = T.mkSrcPos tMyUtils 180062

p18v65 = T.mkSrcPos tMyUtils 180065

p25v1 = T.mkSrcPos tMyUtils 250001

p25v20 = T.mkSrcPos tMyUtils 250020

p32v1 = T.mkSrcPos tMyUtils 320001

p32v30 = T.mkSrcPos tMyUtils 320030

p33v36 = T.mkSrcPos tMyUtils 330036

p33v30 = T.mkSrcPos tMyUtils 330030

p33v38 = T.mkSrcPos tMyUtils 330038

p34v30 = T.mkSrcPos tMyUtils 340030

p34v36 = T.mkSrcPos tMyUtils 340036

p36v1 = T.mkSrcPos tMyUtils 360001

p36v10 = T.mkSrcPos tMyUtils 360010

p36v22 = T.mkSrcPos tMyUtils 360022

p36v30 = T.mkSrcPos tMyUtils 360030

p43v1 = T.mkSrcPos tMyUtils 430001

p43v25 = T.mkSrcPos tMyUtils 430025

p44v45 = T.mkSrcPos tMyUtils 440045

p44v37 = T.mkSrcPos tMyUtils 440037

p44v47 = T.mkSrcPos tMyUtils 440047

p45v37 = T.mkSrcPos tMyUtils 450037

p45v43 = T.mkSrcPos tMyUtils 450043

p47v1 = T.mkSrcPos tMyUtils 470001

p47v10 = T.mkSrcPos tMyUtils 470010

p47v22 = T.mkSrcPos tMyUtils 470022

p47v32 = T.mkSrcPos tMyUtils 470032

p54v1 = T.mkSrcPos tMyUtils 540001

p54v28 = T.mkSrcPos tMyUtils 540028

p55v54 = T.mkSrcPos tMyUtils 550054

p55v44 = T.mkSrcPos tMyUtils 550044

p55v56 = T.mkSrcPos tMyUtils 550056

p56v44 = T.mkSrcPos tMyUtils 560044

p56v50 = T.mkSrcPos tMyUtils 560050

p58v1 = T.mkSrcPos tMyUtils 580001

p58v10 = T.mkSrcPos tMyUtils 580010

p58v22 = T.mkSrcPos tMyUtils 580022

p58v34 = T.mkSrcPos tMyUtils 580034

p66v1 = T.mkSrcPos tMyUtils 660001

p66v31 = T.mkSrcPos tMyUtils 660031

p68v18 = T.mkSrcPos tMyUtils 680018

p68v6 = T.mkSrcPos tMyUtils 680006

p68v20 = T.mkSrcPos tMyUtils 680020

p70v6 = T.mkSrcPos tMyUtils 700006

p70v12 = T.mkSrcPos tMyUtils 700012

p72v1 = T.mkSrcPos tMyUtils 720001

p72v10 = T.mkSrcPos tMyUtils 720010

p72v22 = T.mkSrcPos tMyUtils 720022

p72v36 = T.mkSrcPos tMyUtils 720036

p79v1 = T.mkSrcPos tMyUtils 790001

p80v6 = T.mkSrcPos tMyUtils 800006

p83v6 = T.mkSrcPos tMyUtils 830006

p83v13 = T.mkSrcPos tMyUtils 830013

p84v13 = T.mkSrcPos tMyUtils 840013

p85v13 = T.mkSrcPos tMyUtils 850013

p88v6 = T.mkSrcPos tMyUtils 880006

p88v12 = T.mkSrcPos tMyUtils 880012

p95v1 = T.mkSrcPos tMyUtils 950001

p95v20 = T.mkSrcPos tMyUtils 950020

p96v20 = T.mkSrcPos tMyUtils 960020

p96v23 = T.mkSrcPos tMyUtils 960023

p96v32 = T.mkSrcPos tMyUtils 960032

p96v42 = T.mkSrcPos tMyUtils 960042

p98v1 = T.mkSrcPos tMyUtils 980001

p98v20 = T.mkSrcPos tMyUtils 980020

p99v20 = T.mkSrcPos tMyUtils 990020

p99v23 = T.mkSrcPos tMyUtils 990023

p99v32 = T.mkSrcPos tMyUtils 990032

p99v48 = T.mkSrcPos tMyUtils 990048

p106v1 = T.mkSrcPos tMyUtils 1060001

p106v19 = T.mkSrcPos tMyUtils 1060019

p107v19 = T.mkSrcPos tMyUtils 1070019

p107v29 = T.mkSrcPos tMyUtils 1070029

p107v43 = T.mkSrcPos tMyUtils 1070043

p109v1 = T.mkSrcPos tMyUtils 1090001

p109v19 = T.mkSrcPos tMyUtils 1090019

p110v19 = T.mkSrcPos tMyUtils 1100019

p110v29 = T.mkSrcPos tMyUtils 1100029

p110v39 = T.mkSrcPos tMyUtils 1100039

p117v1 = T.mkSrcPos tMyUtils 1170001

p117v24 = T.mkSrcPos tMyUtils 1170024

p120v29 = T.mkSrcPos tMyUtils 1200029

p120v6 = T.mkSrcPos tMyUtils 1200006

p120v12 = T.mkSrcPos tMyUtils 1200012

p120v18 = T.mkSrcPos tMyUtils 1200018

p120v32 = T.mkSrcPos tMyUtils 1200032

p120v38 = T.mkSrcPos tMyUtils 1200038

p120v42 = T.mkSrcPos tMyUtils 1200042

p122v22 = T.mkSrcPos tMyUtils 1220022

p122v28 = T.mkSrcPos tMyUtils 1220028

p129v1 = T.mkSrcPos tMyUtils 1290001

p130v6 = T.mkSrcPos tMyUtils 1300006

p130v7 = T.mkSrcPos tMyUtils 1300007

p133v10 = T.mkSrcPos tMyUtils 1330010

p133v20 = T.mkSrcPos tMyUtils 1330020

p133v25 = T.mkSrcPos tMyUtils 1330025

p135v10 = T.mkSrcPos tMyUtils 1350010

p135v18 = T.mkSrcPos tMyUtils 1350018

p135v23 = T.mkSrcPos tMyUtils 1350023

p135v30 = T.mkSrcPos tMyUtils 1350030

p142v1 = T.mkSrcPos tMyUtils 1420001

p142v15 = T.mkSrcPos tMyUtils 1420015

p142v22 = T.mkSrcPos tMyUtils 1420022

p149v1 = T.mkSrcPos tMyUtils 1490001

p150v6 = T.mkSrcPos tMyUtils 1500006

p150v15 = T.mkSrcPos tMyUtils 1500015

p151v13 = T.mkSrcPos tMyUtils 1510013

p152v15 = T.mkSrcPos tMyUtils 1520015

p152v17 = T.mkSrcPos tMyUtils 1520017

p152v33 = T.mkSrcPos tMyUtils 1520033

p152v36 = T.mkSrcPos tMyUtils 1520036

p159v1 = T.mkSrcPos tMyUtils 1590001

p159v18 = T.mkSrcPos tMyUtils 1590018

p159v20 = T.mkSrcPos tMyUtils 1590020

p159v34 = T.mkSrcPos tMyUtils 1590034

p159v37 = T.mkSrcPos tMyUtils 1590037

p166v4 = T.mkSrcPos tMyUtils 1660004

p167v6 = T.mkSrcPos tMyUtils 1670006

p167v12 = T.mkSrcPos tMyUtils 1670012

p169v6 = T.mkSrcPos tMyUtils 1690006

p169v11 = T.mkSrcPos tMyUtils 1690011

p169v15 = T.mkSrcPos tMyUtils 1690015

p169v30 = T.mkSrcPos tMyUtils 1690030

p169v40 = T.mkSrcPos tMyUtils 1690040

p169v46 = T.mkSrcPos tMyUtils 1690046

p169v49 = T.mkSrcPos tMyUtils 1690049
