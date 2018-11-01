module TAbstractMisc
  (gamIAboves,gamIBelows,gamPushUpFF,gamPushDownFF,gamAllUpSlices
    ,gamAllDownSlices,gamAllRoutes,gamUpCloseOfMinf,gamDownCloseOfMaxf
    ,gamAllRoutesMinusTopJONES,gamEqualPoints,gamIsaHOF,gamContainsFunctionSpace
    ,gamIsDataFn,gamRepArity,gamStrongNormalise,gamMeetIRoutes) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 
import TAbstractVals2 
import TSuccsAndPreds2 
import TList  (gnub)

gamIAboves ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain (T.Fun Route (T.List Route)))

gamIAboves pamIAboves p =
  T.fun2 aamIAboves pamIAboves p hamIAboves
  where
  
  hamIAboves fd fr p =
    T.ap2 p20v17 p (gmap p20v17 p) (T.ap1 p20v24 p (p20v24 !\/ p) fr)
      (T.ap2 p20v29 p (gspSuccsR p20v29 p) fd fr)
  

gamIBelows ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain (T.Fun Route (T.List Route)))

gamIBelows pamIBelows p =
  T.fun2 aamIBelows pamIBelows p hamIBelows
  where
  
  hamIBelows fd fr p =
    T.ap2 p27v17 p (gmap p27v17 p) (T.ap1 p27v24 p (p27v24 !/\ p) fr)
      (T.ap2 p27v29 p (gspPredsR p27v29 p) fd fr)
  

gamPushUpFF ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun Domain (T.Fun (T.List Route) (T.List Route)))

gamPushUpFF pamPushUpFF p =
  T.fun2 aamPushUpFF pamPushUpFF p hamPushUpFF
  where
  
  hamPushUpFF fd (T.R T.List _) p = T.con0 p34v19 p T.List T.aList
  hamPushUpFF fd fxs p =
    T.ap1 p35v19 p (gnub p35v19 p)
      (T.ap1 p35v24 p (gconcat p35v24 p)
        (T.ap2 p35v32 p (gmap p35v32 p)
          (T.ap1 p35v37 p (gamIAboves p35v37 p) fd) fxs))
  

gamPushDownFF ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun Domain (T.Fun (T.List Route) (T.List Route)))

gamPushDownFF pamPushDownFF p =
  T.fun2 aamPushDownFF pamPushDownFF p hamPushDownFF
  where
  
  hamPushDownFF fd (T.R T.List _) p = T.con0 p42v21 p T.List T.aList
  hamPushDownFF fd fxs p =
    T.ap1 p43v21 p (gnub p43v21 p)
      (T.ap1 p43v26 p (gconcat p43v26 p)
        (T.ap2 p43v34 p (gmap p43v34 p)
          (T.ap1 p43v39 p (gamIBelows p43v39 p) fd) fxs))
  

gamAllUpSlices ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain (T.List (T.List Route)))

gamAllUpSlices pamAllUpSlices p =
  T.fun1 aamAllUpSlices pamAllUpSlices p hamAllUpSlices
  where
  
  hamAllUpSlices fd p =
    T.ap2 p51v6 p (gtakeWhile p51v6 p)
      (T.ap2 p51v20 p (p51v20 !. p) (gnot p51v17 p) (gnull p51v21 p))
      (T.ap2 p51v28 p (giterate p51v28 p)
        (T.ap1 p51v37 p (gamPushUpFF p51v37 p) fd)
        (T.fromExpList p51v51 p [T.ap1 p51v52 p (gavBottomR p51v52 p) fd]))
  

gamAllDownSlices ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain (T.List (T.List Route)))

gamAllDownSlices pamAllDownSlices p =
  T.fun1 aamAllDownSlices pamAllDownSlices p hamAllDownSlices
  where
  
  hamAllDownSlices fd p =
    T.ap2 p59v6 p (gtakeWhile p59v6 p)
      (T.ap2 p59v20 p (p59v20 !. p) (gnot p59v17 p) (gnull p59v21 p))
      (T.ap2 p59v28 p (giterate p59v28 p)
        (T.ap1 p59v37 p (gamPushDownFF p59v37 p) fd)
        (T.fromExpList p59v53 p [T.ap1 p59v54 p (gavTopR p59v54 p) fd]))
  

gamAllRoutes :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain (T.List Route))

gamAllRoutes pamAllRoutes p =
  T.fun1 aamAllRoutes pamAllRoutes p hamAllRoutes
  where
  
  hamAllRoutes (T.R Two _) p =
    T.fromExpList p67v6 p [T.con0 p67v7 p Zero aZero,T.con0 p67v13 p One aOne]
  hamAllRoutes (T.R (Lift1 fdss) _) p =
    T.con2 p70v12 p T.Cons T.aCons (T.con0 p70v6 p Stop1 aStop1)
      (T.ap2 p70v14 p (gmap p70v14 p) (T.pa0 Up1 T.cn1 p70v18 p aUp1)
        (T.ap1 p70v23 p (gmyCartesianProduct p70v23 p)
          (T.ap2 p70v43 p (gmap p70v43 p) (gamAllRoutes p70v47 p) fdss)))
  hamAllRoutes (T.R (Lift2 fdss) _) p =
    T.con2 p73v12 p T.Cons T.aCons (T.con0 p73v6 p Stop2 aStop2)
      (T.con2 p73v18 p T.Cons T.aCons (T.con0 p73v14 p Up2 aUp2)
        (T.ap2 p73v20 p (gmap p73v20 p) (T.pa0 UpUp2 T.cn1 p73v24 p aUpUp2)
          (T.ap1 p73v31 p (gmyCartesianProduct p73v31 p)
            (T.ap2 p73v51 p (gmap p73v51 p) (gamAllRoutes p73v55 p) fdss))))
  hamAllRoutes (T.R (Func fdss fdt) _) p =
    T.ap1 p76v6 p (gconcat p76v6 p)
      (T.ap1 p76v14 p (gamAllUpSlices p76v14 p)
        (T.con2 p76v29 p Func aFunc fdss fdt))
  hamAllRoutes _ p = T.fatal p
  

gamUpCloseOfMinf ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun Domain (T.Fun (T.List Route) (T.List Route)))

gamUpCloseOfMinf pamUpCloseOfMinf p =
  T.fun2 aamUpCloseOfMinf pamUpCloseOfMinf p hamUpCloseOfMinf
  where
  
  hamUpCloseOfMinf fd (T.R T.List _) p = T.con0 p84v6 p T.List T.aList
  hamUpCloseOfMinf fd (fq@(T.R (T.Cons fx _) _)) p =
    T.con2 p86v8 p T.Cons T.aCons fx
      (T.ap2 p86v11 p (gamUpCloseOfMinf p86v11 p) fd
        (T.ap1 p87v14 p (gavMinR p87v14 p)
          (T.ap1 p0v0 p
            (T.ap2 p87v21 p (TPrelude.g_foldr p87v21 p)
              (T.fun2 T.mkLambda p87v21 p
                (\ f_x f_y p ->
                  T.ccase p0v0 p
                    (let
                      v0v0v1 fy p =
                        T.ap1 p87v21 p
                          (T.ap2 p87v21 p (TPrelude.g_foldr p87v21 p)
                            (T.fun2 T.mkLambda p87v21 p
                              (\ f_x f_y p ->
                                T.ccase p0v0 p
                                  (let
                                    v0v0v1 fz p =
                                      T.ap1 p87v21 p
                                        (T.pa1 T.Cons T.cn1 p87v21 p T.aCons
                                          (T.ap2 p87v25 p (p87v25 !\/ p) fy fz))
                                        f_y
                                    v0v0v1 _ p = T.projection p87v21 p f_y in
                                    (v0v0v1)) f_x))
                            (T.ap2 p87v45 p (gspSuccsR p87v45 p) fd fx)) f_y
                      v0v0v1 _ p = T.projection p87v21 p f_y in (v0v0v1)) f_x))
              fq) (T.fromExpList p0v0 p []))))
  hamUpCloseOfMinf _ _ p = T.fatal p
  

gamDownCloseOfMaxf ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun Domain (T.Fun (T.List Route) (T.List Route)))

gamDownCloseOfMaxf pamDownCloseOfMaxf p =
  T.fun2 aamDownCloseOfMaxf pamDownCloseOfMaxf p hamDownCloseOfMaxf
  where
  
  hamDownCloseOfMaxf fd (T.R T.List _) p = T.con0 p95v6 p T.List T.aList
  hamDownCloseOfMaxf fd (fq@(T.R (T.Cons fx _) _)) p =
    T.con2 p97v8 p T.Cons T.aCons fx
      (T.ap2 p97v11 p (gamDownCloseOfMaxf p97v11 p) fd
        (T.ap1 p98v14 p (gavMaxR p98v14 p)
          (T.ap1 p0v0 p
            (T.ap2 p98v21 p (TPrelude.g_foldr p98v21 p)
              (T.fun2 T.mkLambda p98v21 p
                (\ f_x f_y p ->
                  T.ccase p0v0 p
                    (let
                      v0v0v1 fy p =
                        T.ap1 p98v21 p
                          (T.ap2 p98v21 p (TPrelude.g_foldr p98v21 p)
                            (T.fun2 T.mkLambda p98v21 p
                              (\ f_x f_y p ->
                                T.ccase p0v0 p
                                  (let
                                    v0v0v1 fz p =
                                      T.ap1 p98v21 p
                                        (T.pa1 T.Cons T.cn1 p98v21 p T.aCons
                                          (T.ap2 p98v25 p (p98v25 !/\ p) fy fz))
                                        f_y
                                    v0v0v1 _ p = T.projection p98v21 p f_y in
                                    (v0v0v1)) f_x))
                            (T.ap2 p98v45 p (gspPredsR p98v45 p) fd fx)) f_y
                      v0v0v1 _ p = T.projection p98v21 p f_y in (v0v0v1)) f_x))
              fq) (T.fromExpList p0v0 p []))))
  hamDownCloseOfMaxf _ _ p = T.fatal p
  

gamAllRoutesMinusTopJONES ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain (T.List Route))

gamAllRoutesMinusTopJONES pamAllRoutesMinusTopJONES p =
  T.fun1 aamAllRoutesMinusTopJONES pamAllRoutesMinusTopJONES p
    hamAllRoutesMinusTopJONES
  where
  
  hamAllRoutesMinusTopJONES fd p =
    T.ap2 p106v6 p (gamDownCloseOfMaxf p106v6 p) fd
      (T.ap2 p106v27 p (gspPredsR p106v27 p) fd
        (T.ap1 p106v39 p (gavTopR p106v39 p) fd))
  

gamEqualPoints ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Point (T.Fun Point Bool))

gamEqualPoints pamEqualPoints p =
  T.fun2 aamEqualPoints pamEqualPoints p hamEqualPoints
  where
  
  hamEqualPoints (T.R (T.Tuple2 fd1 fr1) _) (T.R (T.Tuple2 fd2 fr2) _) p =
    T.cif p128v6 p (T.ap2 p128v16 p (p128v16 !== p) fd1 fd2)
      (\ p -> T.ap2 p129v16 p (p129v16 !== p) fr1 fr2)
      (\ p ->
        T.ap1 p130v13 p (gpanic p130v13 p)
          (T.fromLitString p130v19 p "Comparing points in different domains."))
  hamEqualPoints _ _ p = T.fatal p
  

gamIsaHOF :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain Bool)

gamIsaHOF pamIsaHOF p =
  T.fun1 aamIsaHOF pamIsaHOF p hamIsaHOF
  where
  
  hamIsaHOF (T.R (Func fdss fdt) _) p =
    T.ap2 p138v33 p (p138v33 !|| p)
      (T.ap1 p138v6 p (gamContainsFunctionSpace p138v6 p) fdt)
      (T.ap2 p139v6 p (gmyAny p139v6 p) (gamContainsFunctionSpace p139v12 p)
        fdss)
  hamIsaHOF _ p = T.fatal p
  

gamContainsFunctionSpace :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain Bool)

gamContainsFunctionSpace pamContainsFunctionSpace p =
  T.fun1 aamContainsFunctionSpace pamContainsFunctionSpace p
    hamContainsFunctionSpace
  where
  
  hamContainsFunctionSpace (T.R Two _) p = T.con0 p146v41 p False aFalse
  hamContainsFunctionSpace (T.R (Lift1 fdss) _) p =
    T.ap2 p147v41 p (gmyAny p147v41 p) (gamContainsFunctionSpace p147v47 p) fdss
  hamContainsFunctionSpace (T.R (Lift2 fdss) _) p =
    T.ap2 p148v41 p (gmyAny p148v41 p) (gamContainsFunctionSpace p148v47 p) fdss
  hamContainsFunctionSpace (T.R (Func _ _) _) p = T.con0 p149v41 p True aTrue
  hamContainsFunctionSpace _ p = T.fatal p
  

gamIsDataFn :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain Bool)

gamIsDataFn pamIsDataFn p =
  T.fun1 aamIsDataFn pamIsDataFn p hamIsDataFn
  where
  
  hamIsDataFn (T.R (Func _ fdt) _) p =
    T.ap1 p156v26 p (gnot p156v26 p)
      (T.ap1 p156v31 p (gamContainsFunctionSpace p156v31 p) fdt)
  hamIsDataFn _ p = T.fatal p
  

gamRepArity :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Rep Int)

gamRepArity pamRepArity p =
  T.fun1 aamRepArity pamRepArity p hamRepArity
  where
  
  hamRepArity (T.R (RepTwo (T.R (Min1Max0 far ff1 ff0) _)) _) p =
    T.projection p163v59 p far
  hamRepArity (T.R (Rep1 (T.R (Min1Max0 flf_ar flf_f1 flf_f0) _) fhfs) _) p =
    T.projection p164v59 p flf_ar
  hamRepArity (T.R (Rep2 (T.R (Min1Max0 flf_ar flf_f1 flf_f0) _) fmf fhfs) _)
    p =
    T.projection p165v59 p flf_ar
  hamRepArity _ p = T.fatal p
  

gamStrongNormalise :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain Domain)

gamStrongNormalise pamStrongNormalise p =
  T.fun1 aamStrongNormalise pamStrongNormalise p hamStrongNormalise
  where
  
  hamStrongNormalise (T.R Two _) p = T.con0 p173v6 p Two aTwo
  hamStrongNormalise (T.R (Lift1 fds) _) p =
    T.con1 p176v6 p Lift1 aLift1
      (T.ap2 p176v13 p (gmap p176v13 p) (gamStrongNormalise p176v17 p) fds)
  hamStrongNormalise (T.R (Lift2 fds) _) p =
    T.con1 p179v6 p Lift2 aLift2
      (T.ap2 p179v13 p (gmap p179v13 p) (gamStrongNormalise p179v17 p) fds)
  hamStrongNormalise (T.R (Func fdss (T.R (Func fdss2 fdt) _)) _) p =
    T.ap1 p182v6 p (gamStrongNormalise p182v6 p)
      (T.con2 p182v25 p Func aFunc (T.ap2 p182v34 p (p182v34 !++ p) fdss fdss2)
        fdt)
  hamStrongNormalise (T.R (Func fdss fnon_func_res) _) p =
    T.con2 p185v6 p Func aFunc
      (T.ap2 p185v12 p (gmap p185v12 p) (gamStrongNormalise p185v16 p) fdss)
      (T.ap1 p185v40 p (gamStrongNormalise p185v40 p) fnon_func_res)
  hamStrongNormalise _ p = T.fatal p
  

gamMeetIRoutes :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain (T.List Route))

gamMeetIRoutes pamMeetIRoutes p =
  T.fun1 aamMeetIRoutes pamMeetIRoutes p hamMeetIRoutes
  where
  
  hamMeetIRoutes (T.R Two _) p =
    T.fromExpList p193v6 p [T.con0 p193v7 p Zero aZero]
  hamMeetIRoutes (T.R (Lift1 fds) _) p =
    T.con2 p195v12 p T.Cons T.aCons (T.con0 p195v6 p Stop1 aStop1)
      (T.ap2 p196v6 p (gmap p196v6 p) (T.pa0 Up1 T.cn1 p196v10 p aUp1)
        (T.ap2 p196v15 p (gmyListVariants p196v15 p)
          (T.ap2 p196v31 p (gmap p196v31 p) (gavTopR p196v35 p) fds)
          (T.ap2 p196v47 p (gmap p196v47 p) (gamMeetIRoutes p196v51 p) fds)))
  hamMeetIRoutes (T.R (Lift2 fds) _) p =
    T.con2 p198v12 p T.Cons T.aCons (T.con0 p198v6 p Stop2 aStop2)
      (T.con2 p199v12 p T.Cons T.aCons (T.con0 p199v6 p Up2 aUp2)
        (T.ap2 p200v6 p (gmap p200v6 p) (T.pa0 UpUp2 T.cn1 p200v10 p aUpUp2)
          (T.ap2 p200v17 p (gmyListVariants p200v17 p)
            (T.ap2 p200v33 p (gmap p200v33 p) (gavTopR p200v37 p) fds)
            (T.ap2 p200v49 p (gmap p200v49 p) (gamMeetIRoutes p200v53 p) fds))))
  hamMeetIRoutes _ p = T.fatal p
  

tAbstractMisc = T.mkModule "AbstractMisc" "AbstractMisc.hs" Prelude.True

aamIAboves = T.mkVariable tAbstractMisc 200001 3 2 "amIAboves" Prelude.False

aamIBelows = T.mkVariable tAbstractMisc 270001 3 2 "amIBelows" Prelude.False

aamPushUpFF = T.mkVariable tAbstractMisc 340001 3 2 "amPushUpFF" Prelude.False

aamPushDownFF =
  T.mkVariable tAbstractMisc 420001 3 2 "amPushDownFF" Prelude.False

aamAllUpSlices =
  T.mkVariable tAbstractMisc 500001 3 1 "amAllUpSlices" Prelude.False

aamAllDownSlices =
  T.mkVariable tAbstractMisc 580001 3 1 "amAllDownSlices" Prelude.False

aamAllRoutes = T.mkVariable tAbstractMisc 660001 3 1 "amAllRoutes" Prelude.False

aamUpCloseOfMinf =
  T.mkVariable tAbstractMisc 830001 3 2 "amUpCloseOfMinf" Prelude.False

aamDownCloseOfMaxf =
  T.mkVariable tAbstractMisc 940001 3 2 "amDownCloseOfMaxf" Prelude.False

aamAllRoutesMinusTopJONES =
  T.mkVariable tAbstractMisc 1050001 3 1 "amAllRoutesMinusTopJONES"
    Prelude.False

aamEqualPoints =
  T.mkVariable tAbstractMisc 1270001 3 2 "amEqualPoints" Prelude.False

aamIsaHOF = T.mkVariable tAbstractMisc 1370001 3 1 "amIsaHOF" Prelude.False

aamContainsFunctionSpace =
  T.mkVariable tAbstractMisc 1460001 3 1 "amContainsFunctionSpace" Prelude.False

aamIsDataFn = T.mkVariable tAbstractMisc 1560001 3 1 "amIsDataFn" Prelude.False

aamRepArity = T.mkVariable tAbstractMisc 1630001 3 1 "amRepArity" Prelude.False

aamStrongNormalise =
  T.mkVariable tAbstractMisc 1720001 3 1 "amStrongNormalise" Prelude.False

aamMeetIRoutes =
  T.mkVariable tAbstractMisc 1920001 3 1 "amMeetIRoutes" Prelude.False

p20v1 = T.mkSrcPos tAbstractMisc 200001

p20v17 = T.mkSrcPos tAbstractMisc 200017

p20v24 = T.mkSrcPos tAbstractMisc 200024

p20v29 = T.mkSrcPos tAbstractMisc 200029

p27v1 = T.mkSrcPos tAbstractMisc 270001

p27v17 = T.mkSrcPos tAbstractMisc 270017

p27v24 = T.mkSrcPos tAbstractMisc 270024

p27v29 = T.mkSrcPos tAbstractMisc 270029

p34v1 = T.mkSrcPos tAbstractMisc 340001

p34v19 = T.mkSrcPos tAbstractMisc 340019

p35v19 = T.mkSrcPos tAbstractMisc 350019

p35v24 = T.mkSrcPos tAbstractMisc 350024

p35v32 = T.mkSrcPos tAbstractMisc 350032

p35v37 = T.mkSrcPos tAbstractMisc 350037

p42v1 = T.mkSrcPos tAbstractMisc 420001

p42v21 = T.mkSrcPos tAbstractMisc 420021

p43v21 = T.mkSrcPos tAbstractMisc 430021

p43v26 = T.mkSrcPos tAbstractMisc 430026

p43v34 = T.mkSrcPos tAbstractMisc 430034

p43v39 = T.mkSrcPos tAbstractMisc 430039

p50v1 = T.mkSrcPos tAbstractMisc 500001

p51v6 = T.mkSrcPos tAbstractMisc 510006

p51v20 = T.mkSrcPos tAbstractMisc 510020

p51v17 = T.mkSrcPos tAbstractMisc 510017

p51v21 = T.mkSrcPos tAbstractMisc 510021

p51v28 = T.mkSrcPos tAbstractMisc 510028

p51v37 = T.mkSrcPos tAbstractMisc 510037

p51v51 = T.mkSrcPos tAbstractMisc 510051

p51v52 = T.mkSrcPos tAbstractMisc 510052

p58v1 = T.mkSrcPos tAbstractMisc 580001

p59v6 = T.mkSrcPos tAbstractMisc 590006

p59v20 = T.mkSrcPos tAbstractMisc 590020

p59v17 = T.mkSrcPos tAbstractMisc 590017

p59v21 = T.mkSrcPos tAbstractMisc 590021

p59v28 = T.mkSrcPos tAbstractMisc 590028

p59v37 = T.mkSrcPos tAbstractMisc 590037

p59v53 = T.mkSrcPos tAbstractMisc 590053

p59v54 = T.mkSrcPos tAbstractMisc 590054

p66v1 = T.mkSrcPos tAbstractMisc 660001

p67v6 = T.mkSrcPos tAbstractMisc 670006

p67v7 = T.mkSrcPos tAbstractMisc 670007

p67v13 = T.mkSrcPos tAbstractMisc 670013

p70v12 = T.mkSrcPos tAbstractMisc 700012

p70v6 = T.mkSrcPos tAbstractMisc 700006

p70v14 = T.mkSrcPos tAbstractMisc 700014

p70v18 = T.mkSrcPos tAbstractMisc 700018

p70v23 = T.mkSrcPos tAbstractMisc 700023

p70v43 = T.mkSrcPos tAbstractMisc 700043

p70v47 = T.mkSrcPos tAbstractMisc 700047

p73v12 = T.mkSrcPos tAbstractMisc 730012

p73v6 = T.mkSrcPos tAbstractMisc 730006

p73v18 = T.mkSrcPos tAbstractMisc 730018

p73v14 = T.mkSrcPos tAbstractMisc 730014

p73v20 = T.mkSrcPos tAbstractMisc 730020

p73v24 = T.mkSrcPos tAbstractMisc 730024

p73v31 = T.mkSrcPos tAbstractMisc 730031

p73v51 = T.mkSrcPos tAbstractMisc 730051

p73v55 = T.mkSrcPos tAbstractMisc 730055

p76v6 = T.mkSrcPos tAbstractMisc 760006

p76v14 = T.mkSrcPos tAbstractMisc 760014

p76v29 = T.mkSrcPos tAbstractMisc 760029

p83v1 = T.mkSrcPos tAbstractMisc 830001

p84v6 = T.mkSrcPos tAbstractMisc 840006

p86v8 = T.mkSrcPos tAbstractMisc 860008

p86v11 = T.mkSrcPos tAbstractMisc 860011

p87v14 = T.mkSrcPos tAbstractMisc 870014

p0v0 = T.mkSrcPos tAbstractMisc 0

p87v21 = T.mkSrcPos tAbstractMisc 870021

p87v25 = T.mkSrcPos tAbstractMisc 870025

p87v45 = T.mkSrcPos tAbstractMisc 870045

p94v1 = T.mkSrcPos tAbstractMisc 940001

p95v6 = T.mkSrcPos tAbstractMisc 950006

p97v8 = T.mkSrcPos tAbstractMisc 970008

p97v11 = T.mkSrcPos tAbstractMisc 970011

p98v14 = T.mkSrcPos tAbstractMisc 980014

p98v21 = T.mkSrcPos tAbstractMisc 980021

p98v25 = T.mkSrcPos tAbstractMisc 980025

p98v45 = T.mkSrcPos tAbstractMisc 980045

p105v1 = T.mkSrcPos tAbstractMisc 1050001

p106v6 = T.mkSrcPos tAbstractMisc 1060006

p106v27 = T.mkSrcPos tAbstractMisc 1060027

p106v39 = T.mkSrcPos tAbstractMisc 1060039

p127v1 = T.mkSrcPos tAbstractMisc 1270001

p128v6 = T.mkSrcPos tAbstractMisc 1280006

p128v16 = T.mkSrcPos tAbstractMisc 1280016

p129v16 = T.mkSrcPos tAbstractMisc 1290016

p130v13 = T.mkSrcPos tAbstractMisc 1300013

p130v19 = T.mkSrcPos tAbstractMisc 1300019

p137v1 = T.mkSrcPos tAbstractMisc 1370001

p138v33 = T.mkSrcPos tAbstractMisc 1380033

p138v6 = T.mkSrcPos tAbstractMisc 1380006

p139v6 = T.mkSrcPos tAbstractMisc 1390006

p139v12 = T.mkSrcPos tAbstractMisc 1390012

p146v1 = T.mkSrcPos tAbstractMisc 1460001

p146v41 = T.mkSrcPos tAbstractMisc 1460041

p147v41 = T.mkSrcPos tAbstractMisc 1470041

p147v47 = T.mkSrcPos tAbstractMisc 1470047

p148v41 = T.mkSrcPos tAbstractMisc 1480041

p148v47 = T.mkSrcPos tAbstractMisc 1480047

p149v41 = T.mkSrcPos tAbstractMisc 1490041

p156v1 = T.mkSrcPos tAbstractMisc 1560001

p156v26 = T.mkSrcPos tAbstractMisc 1560026

p156v31 = T.mkSrcPos tAbstractMisc 1560031

p163v1 = T.mkSrcPos tAbstractMisc 1630001

p163v59 = T.mkSrcPos tAbstractMisc 1630059

p164v59 = T.mkSrcPos tAbstractMisc 1640059

p165v59 = T.mkSrcPos tAbstractMisc 1650059

p172v1 = T.mkSrcPos tAbstractMisc 1720001

p173v6 = T.mkSrcPos tAbstractMisc 1730006

p176v6 = T.mkSrcPos tAbstractMisc 1760006

p176v13 = T.mkSrcPos tAbstractMisc 1760013

p176v17 = T.mkSrcPos tAbstractMisc 1760017

p179v6 = T.mkSrcPos tAbstractMisc 1790006

p179v13 = T.mkSrcPos tAbstractMisc 1790013

p179v17 = T.mkSrcPos tAbstractMisc 1790017

p182v6 = T.mkSrcPos tAbstractMisc 1820006

p182v25 = T.mkSrcPos tAbstractMisc 1820025

p182v34 = T.mkSrcPos tAbstractMisc 1820034

p185v6 = T.mkSrcPos tAbstractMisc 1850006

p185v12 = T.mkSrcPos tAbstractMisc 1850012

p185v16 = T.mkSrcPos tAbstractMisc 1850016

p185v40 = T.mkSrcPos tAbstractMisc 1850040

p192v1 = T.mkSrcPos tAbstractMisc 1920001

p193v6 = T.mkSrcPos tAbstractMisc 1930006

p193v7 = T.mkSrcPos tAbstractMisc 1930007

p195v12 = T.mkSrcPos tAbstractMisc 1950012

p195v6 = T.mkSrcPos tAbstractMisc 1950006

p196v6 = T.mkSrcPos tAbstractMisc 1960006

p196v10 = T.mkSrcPos tAbstractMisc 1960010

p196v15 = T.mkSrcPos tAbstractMisc 1960015

p196v31 = T.mkSrcPos tAbstractMisc 1960031

p196v35 = T.mkSrcPos tAbstractMisc 1960035

p196v47 = T.mkSrcPos tAbstractMisc 1960047

p196v51 = T.mkSrcPos tAbstractMisc 1960051

p198v12 = T.mkSrcPos tAbstractMisc 1980012

p198v6 = T.mkSrcPos tAbstractMisc 1980006

p199v12 = T.mkSrcPos tAbstractMisc 1990012

p199v6 = T.mkSrcPos tAbstractMisc 1990006

p200v6 = T.mkSrcPos tAbstractMisc 2000006

p200v10 = T.mkSrcPos tAbstractMisc 2000010

p200v17 = T.mkSrcPos tAbstractMisc 2000017

p200v33 = T.mkSrcPos tAbstractMisc 2000033

p200v37 = T.mkSrcPos tAbstractMisc 2000037

p200v49 = T.mkSrcPos tAbstractMisc 2000049

p200v53 = T.mkSrcPos tAbstractMisc 2000053
