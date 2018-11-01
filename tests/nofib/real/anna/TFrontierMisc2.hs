module TFrontierMisc2
  (gfsZULB,gfsZULB_2,gfmSelect,gfmIsNothing,gfmMaxIntersection
    ,gfmMinIntersection,gfmReviseMinXX,gfmReviseMaxYY) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 
import TAbstractVals2 
import TSuccsAndPreds2 
import TAbstractMisc 

gfsZULB :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Rep (T.Fun Rep Rep))

gfsZULB pfsZULB p =
  T.fun2 afsZULB pfsZULB p hfsZULB
  where
  
  hfsZULB (T.R (RepTwo ffru) _) (T.R (RepTwo ffrl) _) p =
    T.con1 p22v6 p RepTwo aRepTwo
      (T.ap2 p22v14 p (gfsZULB_2 p22v14 p) ffru ffrl)
  hfsZULB (T.R (Rep1 flfu fhfsu) _) (T.R (Rep1 flfl fhfsl) _) p =
    T.con2 p25v6 p Rep1 aRep1 (T.ap2 p25v12 p (gfsZULB_2 p25v12 p) flfu flfl)
      (T.ap3 p25v31 p (gmyZipWith2 p25v31 p) (gfsZULB p25v42 p) fhfsu fhfsl)
  hfsZULB (T.R (Rep2 flfu fmfu fhfsu) _) (T.R (Rep2 flfl fmfl fhfsl) _) p =
    T.con3 p28v6 p Rep2 aRep2 (T.ap2 p28v12 p (gfsZULB_2 p28v12 p) flfu flfl)
      (T.ap2 p28v31 p (gfsZULB_2 p28v31 p) fmfu fmfl)
      (T.ap3 p28v50 p (gmyZipWith2 p28v50 p) (gfsZULB p28v61 p) fhfsu fhfsl)
  hfsZULB _ _ p = T.fatal p
  

gfsZULB_2 pfsZULB_2 p =
  T.fun2 afsZULB_2 pfsZULB_2 p hfsZULB_2
  where
  
  hfsZULB_2 (T.R (Min1Max0 faru ff1u ff0u) _) (T.R (Min1Max0 farl ff1l ff0l) _)
    p =
    T.con3 p31v6 p Min1Max0 aMin1Max0 faru ff1l ff0u
  hfsZULB_2 _ _ p = T.fatal p
  

gfmSelect ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Int
          (T.Fun (T.List FrontierElem)
            (T.Fun (T.List FrontierElem) (T.Fun Bool (Maybe FrontierElem)))))

gfmSelect pfmSelect p =
  T.fun4 afmSelect pfmSelect p hfmSelect
  where
  
  hfmSelect fa_rand fup_space fdown_space ffromTop p =
    let
      gmin_max_pairs pmin_max_pairs p =
        T.constUse pmin_max_pairs p smin_max_pairs
      smin_max_pairs =
        T.constDef p a43v10min_max_pairs
          (\ p ->
            T.ap2 p44v15 p (gtake p44v15 p)
              (T.ap1 p44v20 p (TPreludeBasic.gfromInteger p44v20 p)
                (T.conInteger p44v20 p 30))
              (T.ap1 p0v0 p
                (T.ap2 p44v23 p (TPrelude.g_foldr p44v23 p)
                  (T.fun2 T.mkLambda p44v23 p
                    (\ f_x f_y p ->
                      T.ccase p0v0 p
                        (let
                          v0v0v1 fmi p =
                            T.ap1 p44v23 p
                              (T.ap2 p44v23 p (TPrelude.g_foldr p44v23 p)
                                (T.fun2 T.mkLambda p44v23 p
                                  (\ f_x f_y p ->
                                    T.ccase p0v0 p
                                      (let
                                        v0v0v1 fma p =
                                          T.ap1 p44v23 p
                                            (T.ap2 p44v23 p
                                              (TPrelude.g_filter p44v23 p)
                                              (T.ap2 p45v59 p
                                                (gavBelowEQfrel p45v59 p) fmi
                                                fma)
                                              (T.pa1 T.Cons T.cn1 p44v23 p
                                                T.aCons
                                                (T.con2 p44v24 p T.Tuple2
                                                  T.aTuple2 fmi fma))) f_y
                                        v0v0v1 _ p = T.projection p44v23 p f_y
                                        in (v0v0v1)) f_x)) fdown_space) f_y
                          v0v0v1 _ p = T.projection p44v23 p f_y in (v0v0v1))
                        f_x)) fup_space) (T.fromExpList p0v0 p [])))
      gmmpl pmmpl p = T.constUse pmmpl p smmpl
      smmpl =
        T.constDef p a46v10mmpl
          (\ p -> T.ap1 p46v17 p (glength p46v17 p) (gmin_max_pairs p46v24 p))
      gn pn p = T.constUse pn p sn
      sn =
        T.constDef p a47v10n
          (\ p -> T.ap2 p47v22 p (gmod p47v22 p) fa_rand (gmmpl p47v27 p))
      gselected_pair pselected_pair p =
        T.constUse pselected_pair p sselected_pair
      sselected_pair =
        T.constDef p a48v10selected_pair
          (\ p ->
            T.ap2 p48v40 p (p48v40 !## p) (gmin_max_pairs p48v26 p)
              (gn p48v43 p)) in
      (T.cif p50v10 p
        (T.ap1 p50v13 p (gnull p50v13 p) (gmin_max_pairs p50v18 p))
        (\ p -> T.con0 p51v15 p Nothing aNothing)
        (\ p ->
          T.cif p53v10 p ffromTop
            (\ p ->
              T.con1 p54v15 p Just aJust
                (T.ap1 p54v21 p (gsecond p54v21 p) (gselected_pair p54v28 p)))
            (\ p ->
              T.con1 p55v15 p Just aJust
                (T.ap1 p55v21 p (gfirst p55v21 p) (gselected_pair p55v28 p)))))
  

gfmIsNothing :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (Maybe a) Bool)

gfmIsNothing pfmIsNothing p =
  T.fun1 afmIsNothing pfmIsNothing p hfmIsNothing
  where
  
  hfmIsNothing (T.R Nothing _) p = T.con0 p62v25 p True aTrue
  hfmIsNothing (T.R (Just _) _) p = T.con0 p63v25 p False aFalse
  hfmIsNothing _ p = T.fatal p
  

gfmMaxIntersection ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.List FrontierElem)
          (T.Fun (T.List FrontierElem) (T.List FrontierElem)))

gfmMaxIntersection pfmMaxIntersection p =
  T.fun2 afmMaxIntersection pfmMaxIntersection p hfmMaxIntersection
  where
  
  hfmMaxIntersection fxx fyy p =
    T.ap1 p71v6 p (gavMaxfrel p71v6 p)
      (T.ap1 p0v0 p
        (T.ap2 p71v16 p (TPrelude.g_foldr p71v16 p)
          (T.fun2 T.mkLambda p71v16 p
            (\ f_x f_y p ->
              T.ccase p0v0 p
                (let
                  v0v0v1 fx p =
                    T.ap1 p71v16 p
                      (T.ap2 p71v16 p (TPrelude.g_foldr p71v16 p)
                        (T.fun2 T.mkLambda p71v16 p
                          (\ f_x f_y p ->
                            T.ccase p0v0 p
                              (let
                                v0v0v1 fy p =
                                  T.ap1 p71v16 p
                                    (T.pa1 T.Cons T.cn1 p71v16 p T.aCons
                                      (T.ap2 p71v21 p (gavGLBfrel p71v21 p) fx
                                        fy)) f_y
                                v0v0v1 _ p = T.projection p71v16 p f_y in
                                (v0v0v1)) f_x)) fyy) f_y
                  v0v0v1 _ p = T.projection p71v16 p f_y in (v0v0v1)) f_x)) fxx)
        (T.fromExpList p0v0 p []))
  

gfmMinIntersection ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.List FrontierElem)
          (T.Fun (T.List FrontierElem) (T.List FrontierElem)))

gfmMinIntersection pfmMinIntersection p =
  T.fun2 afmMinIntersection pfmMinIntersection p hfmMinIntersection
  where
  
  hfmMinIntersection fxx fyy p =
    T.ap1 p79v6 p (gavMinfrel p79v6 p)
      (T.ap1 p0v0 p
        (T.ap2 p79v16 p (TPrelude.g_foldr p79v16 p)
          (T.fun2 T.mkLambda p79v16 p
            (\ f_x f_y p ->
              T.ccase p0v0 p
                (let
                  v0v0v1 fx p =
                    T.ap1 p79v16 p
                      (T.ap2 p79v16 p (TPrelude.g_foldr p79v16 p)
                        (T.fun2 T.mkLambda p79v16 p
                          (\ f_x f_y p ->
                            T.ccase p0v0 p
                              (let
                                v0v0v1 fy p =
                                  T.ap1 p79v16 p
                                    (T.pa1 T.Cons T.cn1 p79v16 p T.aCons
                                      (T.ap2 p79v21 p (gavLUBfrel p79v21 p) fx
                                        fy)) f_y
                                v0v0v1 _ p = T.projection p79v16 p f_y in
                                (v0v0v1)) f_x)) fyy) f_y
                  v0v0v1 _ p = T.projection p79v16 p f_y in (v0v0v1)) f_x)) fxx)
        (T.fromExpList p0v0 p []))
  

gfmReviseMinXX ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.List Domain)
          (T.Fun (T.List FrontierElem)
            (T.Fun FrontierElem (T.List FrontierElem))))

gfmReviseMinXX pfmReviseMinXX p =
  T.fun3 afmReviseMinXX pfmReviseMinXX p hfmReviseMinXX
  where
  
  hfmReviseMinXX fds ftrial_min_xx fargs p =
    let
      gx_underneath px_underneath p = T.constUse px_underneath p sx_underneath
      gx_not_underneath px_underneath p =
        T.constUse px_underneath p sx_not_underneath
      j90v10x_underneath =
        case
          T.ap2 p91v15 p (gsplitList p91v15 p)
            (T.ap2 p91v27 p (TPrelude.gflip p91v27 p) (gavBelowEQfrel p91v27 p)
              fargs) ftrial_min_xx of
          T.R (T.Tuple2 fx_underneath fx_not_underneath) kx_underneath ->
            (kx_underneath,fx_underneath,fx_not_underneath)
          _ -> T.fatal p
      sx_underneath =
        T.constDef p a90v11x_underneath
          (\ _ ->
            case j90v10x_underneath of
              (kx_underneath,fx_underneath,fx_not_underneath) ->
                T.projection p90v11 kx_underneath fx_underneath)
      sx_not_underneath =
        T.constDef p a90v25x_not_underneath
          (\ _ ->
            case j90v10x_underneath of
              (kx_underneath,fx_underneath,fx_not_underneath) ->
                T.projection p90v25 kx_underneath fx_not_underneath)
      goptimised_result poptimised_result p =
        T.constUse poptimised_result p soptimised_result
      soptimised_result =
        T.constDef p a92v10optimised_result
          (\ p ->
            T.ap2 p93v15 p (gfmReviseMinXX_aux p93v15 p)
              (T.ap2 p94v22 p (gfmMinIntersection p94v22 p)
                (gx_underneath p94v40 p)
                (T.ap2 p94v54 p (gspSuccsFrel p94v54 p) fds fargs))
              (gx_not_underneath p95v21 p))
      gfmReviseMinXX_aux pfmReviseMinXX_aux p =
        T.fun2 a96v10fmReviseMinXX_aux pfmReviseMinXX_aux p hfmReviseMinXX_aux
        where
        
        hfmReviseMinXX_aux fxs fys p =
          T.cif p97v15 p
            (T.ap2 p97v32 p (p97v32 !< p)
              (T.ap1 p97v22 p (glength p97v22 p) fxs)
              (T.ap1 p97v34 p (glength p97v34 p) fys))
            (\ p ->
              T.ap3 p98v22 p (gfoldr p98v22 p) (gavMinAddPtfrel p98v28 p) fxs
                fys)
            (\ p ->
              T.ap3 p99v22 p (gfoldr p99v22 p) (gavMinAddPtfrel p99v28 p) fys
                fxs)
         in (goptimised_result p101v10 p)
  

gfmReviseMaxYY ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.List Domain)
          (T.Fun (T.List FrontierElem)
            (T.Fun FrontierElem (T.List FrontierElem))))

gfmReviseMaxYY pfmReviseMaxYY p =
  T.fun3 afmReviseMaxYY pfmReviseMaxYY p hfmReviseMaxYY
  where
  
  hfmReviseMaxYY fds ftrial_max_yy fargs p =
    let
      gy_above py_above p = T.constUse py_above p sy_above
      gy_not_above py_above p = T.constUse py_above p sy_not_above
      j112v10y_above =
        case
          T.ap2 p113v15 p (gsplitList p113v15 p)
            (T.ap1 p113v32 p (gavBelowEQfrel p113v32 p) fargs) ftrial_max_yy of
          T.R (T.Tuple2 fy_above fy_not_above) ky_above ->
            (ky_above,fy_above,fy_not_above)
          _ -> T.fatal p
      sy_above =
        T.constDef p a112v11y_above
          (\ _ ->
            case j112v10y_above of
              (ky_above,fy_above,fy_not_above) ->
                T.projection p112v11 ky_above fy_above)
      sy_not_above =
        T.constDef p a112v20y_not_above
          (\ _ ->
            case j112v10y_above of
              (ky_above,fy_above,fy_not_above) ->
                T.projection p112v20 ky_above fy_not_above)
      goptimised_result poptimised_result p =
        T.constUse poptimised_result p soptimised_result
      soptimised_result =
        T.constDef p a114v10optimised_result
          (\ p ->
            T.ap2 p115v15 p (gfmReviseMaxYY_aux p115v15 p)
              (gy_not_above p116v21 p)
              (T.ap2 p117v22 p (gfmMaxIntersection p117v22 p)
                (gy_above p117v40 p)
                (T.ap2 p117v49 p (gspPredsFrel p117v49 p) fds fargs)))
      gfmReviseMaxYY_aux pfmReviseMaxYY_aux p =
        T.fun2 a118v10fmReviseMaxYY_aux pfmReviseMaxYY_aux p hfmReviseMaxYY_aux
        where
        
        hfmReviseMaxYY_aux fxs fys p =
          T.cif p119v15 p
            (T.ap2 p119v31 p (p119v31 !> p)
              (T.ap1 p119v21 p (glength p119v21 p) fxs)
              (T.ap1 p119v33 p (glength p119v33 p) fys))
            (\ p ->
              T.ap3 p120v21 p (gfoldr p120v21 p) (gavMaxAddPtfrel p120v27 p) fxs
                fys)
            (\ p ->
              T.ap3 p121v21 p (gfoldr p121v21 p) (gavMaxAddPtfrel p121v27 p) fys
                fxs)
         in (goptimised_result p123v10 p)
  

tFrontierMisc2 = T.mkModule "FrontierMisc2" "FrontierMisc2.hs" Prelude.True

afsZULB = T.mkVariable tFrontierMisc2 210001 3 2 "fsZULB" Prelude.False

afsZULB_2 = T.mkVariable tFrontierMisc2 300001 3 2 "fsZULB_2" Prelude.False

afmSelect = T.mkVariable tFrontierMisc2 420001 3 4 "fmSelect" Prelude.False

afmIsNothing =
  T.mkVariable tFrontierMisc2 620001 3 1 "fmIsNothing" Prelude.False

afmMaxIntersection =
  T.mkVariable tFrontierMisc2 700001 3 2 "fmMaxIntersection" Prelude.False

afmMinIntersection =
  T.mkVariable tFrontierMisc2 780001 3 2 "fmMinIntersection" Prelude.False

afmReviseMinXX =
  T.mkVariable tFrontierMisc2 890001 3 3 "fmReviseMinXX" Prelude.False

afmReviseMaxYY =
  T.mkVariable tFrontierMisc2 1110001 3 3 "fmReviseMaxYY" Prelude.False

a43v10min_max_pairs =
  T.mkVariable tFrontierMisc2 430010 3 0 "min_max_pairs" Prelude.True

a46v10mmpl = T.mkVariable tFrontierMisc2 460010 3 0 "mmpl" Prelude.True

a47v10n = T.mkVariable tFrontierMisc2 470010 3 0 "n" Prelude.True

a48v10selected_pair =
  T.mkVariable tFrontierMisc2 480010 3 0 "selected_pair" Prelude.True

a90v11x_underneath =
  T.mkVariable tFrontierMisc2 900011 3 0 "x_underneath" Prelude.True

a90v25x_not_underneath =
  T.mkVariable tFrontierMisc2 900025 3 0 "x_not_underneath" Prelude.True

a92v10optimised_result =
  T.mkVariable tFrontierMisc2 920010 3 0 "optimised_result" Prelude.True

a96v10fmReviseMinXX_aux =
  T.mkVariable tFrontierMisc2 960010 3 2 "fmReviseMinXX_aux" Prelude.True

a112v11y_above = T.mkVariable tFrontierMisc2 1120011 3 0 "y_above" Prelude.True

a112v20y_not_above =
  T.mkVariable tFrontierMisc2 1120020 3 0 "y_not_above" Prelude.True

a114v10optimised_result =
  T.mkVariable tFrontierMisc2 1140010 3 0 "optimised_result" Prelude.True

a118v10fmReviseMaxYY_aux =
  T.mkVariable tFrontierMisc2 1180010 3 2 "fmReviseMaxYY_aux" Prelude.True

p21v1 = T.mkSrcPos tFrontierMisc2 210001

p22v6 = T.mkSrcPos tFrontierMisc2 220006

p22v14 = T.mkSrcPos tFrontierMisc2 220014

p25v6 = T.mkSrcPos tFrontierMisc2 250006

p25v12 = T.mkSrcPos tFrontierMisc2 250012

p25v31 = T.mkSrcPos tFrontierMisc2 250031

p25v42 = T.mkSrcPos tFrontierMisc2 250042

p28v6 = T.mkSrcPos tFrontierMisc2 280006

p28v12 = T.mkSrcPos tFrontierMisc2 280012

p28v31 = T.mkSrcPos tFrontierMisc2 280031

p28v50 = T.mkSrcPos tFrontierMisc2 280050

p28v61 = T.mkSrcPos tFrontierMisc2 280061

p30v1 = T.mkSrcPos tFrontierMisc2 300001

p31v6 = T.mkSrcPos tFrontierMisc2 310006

p42v1 = T.mkSrcPos tFrontierMisc2 420001

p43v10 = T.mkSrcPos tFrontierMisc2 430010

p44v15 = T.mkSrcPos tFrontierMisc2 440015

p44v20 = T.mkSrcPos tFrontierMisc2 440020

p0v0 = T.mkSrcPos tFrontierMisc2 0

p44v23 = T.mkSrcPos tFrontierMisc2 440023

p45v59 = T.mkSrcPos tFrontierMisc2 450059

p44v24 = T.mkSrcPos tFrontierMisc2 440024

p46v10 = T.mkSrcPos tFrontierMisc2 460010

p46v17 = T.mkSrcPos tFrontierMisc2 460017

p46v24 = T.mkSrcPos tFrontierMisc2 460024

p47v10 = T.mkSrcPos tFrontierMisc2 470010

p47v22 = T.mkSrcPos tFrontierMisc2 470022

p47v27 = T.mkSrcPos tFrontierMisc2 470027

p48v10 = T.mkSrcPos tFrontierMisc2 480010

p48v40 = T.mkSrcPos tFrontierMisc2 480040

p48v26 = T.mkSrcPos tFrontierMisc2 480026

p48v43 = T.mkSrcPos tFrontierMisc2 480043

p50v10 = T.mkSrcPos tFrontierMisc2 500010

p50v13 = T.mkSrcPos tFrontierMisc2 500013

p50v18 = T.mkSrcPos tFrontierMisc2 500018

p51v15 = T.mkSrcPos tFrontierMisc2 510015

p53v10 = T.mkSrcPos tFrontierMisc2 530010

p54v15 = T.mkSrcPos tFrontierMisc2 540015

p54v21 = T.mkSrcPos tFrontierMisc2 540021

p54v28 = T.mkSrcPos tFrontierMisc2 540028

p55v15 = T.mkSrcPos tFrontierMisc2 550015

p55v21 = T.mkSrcPos tFrontierMisc2 550021

p55v28 = T.mkSrcPos tFrontierMisc2 550028

p62v1 = T.mkSrcPos tFrontierMisc2 620001

p62v25 = T.mkSrcPos tFrontierMisc2 620025

p63v25 = T.mkSrcPos tFrontierMisc2 630025

p70v1 = T.mkSrcPos tFrontierMisc2 700001

p71v6 = T.mkSrcPos tFrontierMisc2 710006

p71v16 = T.mkSrcPos tFrontierMisc2 710016

p71v21 = T.mkSrcPos tFrontierMisc2 710021

p78v1 = T.mkSrcPos tFrontierMisc2 780001

p79v6 = T.mkSrcPos tFrontierMisc2 790006

p79v16 = T.mkSrcPos tFrontierMisc2 790016

p79v21 = T.mkSrcPos tFrontierMisc2 790021

p89v1 = T.mkSrcPos tFrontierMisc2 890001

p90v11 = T.mkSrcPos tFrontierMisc2 900011

p90v25 = T.mkSrcPos tFrontierMisc2 900025

p91v15 = T.mkSrcPos tFrontierMisc2 910015

p91v27 = T.mkSrcPos tFrontierMisc2 910027

p92v10 = T.mkSrcPos tFrontierMisc2 920010

p93v15 = T.mkSrcPos tFrontierMisc2 930015

p94v22 = T.mkSrcPos tFrontierMisc2 940022

p94v40 = T.mkSrcPos tFrontierMisc2 940040

p94v54 = T.mkSrcPos tFrontierMisc2 940054

p95v21 = T.mkSrcPos tFrontierMisc2 950021

p96v10 = T.mkSrcPos tFrontierMisc2 960010

p97v15 = T.mkSrcPos tFrontierMisc2 970015

p97v32 = T.mkSrcPos tFrontierMisc2 970032

p97v22 = T.mkSrcPos tFrontierMisc2 970022

p97v34 = T.mkSrcPos tFrontierMisc2 970034

p98v22 = T.mkSrcPos tFrontierMisc2 980022

p98v28 = T.mkSrcPos tFrontierMisc2 980028

p99v22 = T.mkSrcPos tFrontierMisc2 990022

p99v28 = T.mkSrcPos tFrontierMisc2 990028

p101v10 = T.mkSrcPos tFrontierMisc2 1010010

p111v1 = T.mkSrcPos tFrontierMisc2 1110001

p112v11 = T.mkSrcPos tFrontierMisc2 1120011

p112v20 = T.mkSrcPos tFrontierMisc2 1120020

p113v15 = T.mkSrcPos tFrontierMisc2 1130015

p113v32 = T.mkSrcPos tFrontierMisc2 1130032

p114v10 = T.mkSrcPos tFrontierMisc2 1140010

p115v15 = T.mkSrcPos tFrontierMisc2 1150015

p116v21 = T.mkSrcPos tFrontierMisc2 1160021

p117v22 = T.mkSrcPos tFrontierMisc2 1170022

p117v40 = T.mkSrcPos tFrontierMisc2 1170040

p117v49 = T.mkSrcPos tFrontierMisc2 1170049

p118v10 = T.mkSrcPos tFrontierMisc2 1180010

p119v15 = T.mkSrcPos tFrontierMisc2 1190015

p119v31 = T.mkSrcPos tFrontierMisc2 1190031

p119v21 = T.mkSrcPos tFrontierMisc2 1190021

p119v33 = T.mkSrcPos tFrontierMisc2 1190033

p120v21 = T.mkSrcPos tFrontierMisc2 1200021

p120v27 = T.mkSrcPos tFrontierMisc2 1200027

p121v21 = T.mkSrcPos tFrontierMisc2 1210021

p121v27 = T.mkSrcPos tFrontierMisc2 1210027

p123v10 = T.mkSrcPos tFrontierMisc2 1230010
