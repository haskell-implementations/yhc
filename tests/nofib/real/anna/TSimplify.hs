module TSimplify
  (gsiVectorise,gsiSimplify,gsiHOpt,gsiHOpt_meet,gsiHOpt_app) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 
import TAbstractVals2 
import TAbstractEval2 
import TApply 

gsiVectorise :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (HExpr Naam) (HExpr Naam))

gsiVectorise psiVectorise p =
  T.fun1 asiVectorise psiVectorise p hsiVectorise
  where
  
  hsiVectorise (T.R (HLam fvs1 (T.R (HLam fvs2 fe) _)) _) p =
    T.ap1 p20v6 p (gsiVectorise p20v6 p)
      (T.con2 p20v19 p HLam aHLam (T.ap2 p20v28 p (p20v28 !++ p) fvs1 fvs2) fe)
  hsiVectorise (T.R (HLam fvs fe) _) p =
    T.con2 p22v6 p HLam aHLam fvs (T.ap1 p22v15 p (gsiVectorise p22v15 p) fe)
  hsiVectorise (T.R (HApp (T.R (HTable ft) _) fe) _) p =
    T.con2 p24v6 p HApp aHApp
      (T.con1 p24v12 p HTable aHTable
        (T.ap2 p24v20 p (gmap2nd p24v20 p) (gsiVectorise p24v27 p) ft))
      (T.ap1 p24v44 p (gsiVectorise p24v44 p) fe)
  hsiVectorise (T.R (HApp ff fa) _) p =
    T.ccase p26v6 p
      (let
        v26v6v1 (T.R (HVAp ffn fargs) _) p =
          T.con2 p27v25 p HVAp aHVAp ffn
            (T.ap2 p27v38 p (p27v38 !++ p) fargs
              (T.fromExpList p27v40 p
                [T.ap1 p27v41 p (gsiVectorise p27v41 p) fa]))
        v26v6v1 (T.R (HPoint fp) _) p =
          T.con2 p28v25 p HVAp aHVAp (T.con1 p28v31 p HPoint aHPoint fp)
            (T.fromExpList p28v41 p [T.ap1 p28v42 p (gsiVectorise p28v42 p) fa])
        v26v6v1 (T.R (HVar fv) _) p =
          T.con2 p29v25 p HVAp aHVAp (T.con1 p29v31 p HVar aHVar fv)
            (T.fromExpList p29v39 p [T.ap1 p29v40 p (gsiVectorise p29v40 p) fa])
        v26v6v1 fnon_vap p =
          T.con2 p30v25 p HApp aHApp fnon_vap
            (T.ap1 p30v39 p (gsiVectorise p30v39 p) fa) in (v26v6v1))
      (T.ap1 p26v11 p (gsiVectorise p26v11 p) ff)
  hsiVectorise (fh@(T.R (HVar _) _)) p = T.projection p31v26 p fh
  hsiVectorise (fh@(T.R (HPoint _) _)) p = T.projection p32v28 p fh
  hsiVectorise (T.R (HMeet fes) _) p =
    T.con1 p33v26 p HMeet aHMeet
      (T.ap2 p33v33 p (gmap p33v33 p) (gsiVectorise p33v37 p) fes)
  hsiVectorise _ p = T.fatal p
  

gsiSimplify :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (HExpr Naam) (HExpr Naam))

gsiSimplify psiSimplify p =
  T.fun1 asiSimplify psiSimplify p hsiSimplify
  where
  
  hsiSimplify fhexpr p =
    let
      ghexpr_after_one_cycle phexpr_after_one_cycle p =
        T.constUse phexpr_after_one_cycle p shexpr_after_one_cycle
      shexpr_after_one_cycle =
        T.constDef p a42v9hexpr_after_one_cycle
          (\ p -> T.ap1 p42v33 p (gsiHOpt p42v33 p) fhexpr) in
      (T.cif p44v9 p
        (T.ap2 p44v21 p (p44v21 !== p) fhexpr (ghexpr_after_one_cycle p44v24 p))
        (\ p -> T.projection p45v15 p fhexpr)
        (\ p ->
          T.ap1 p46v15 p (gsiSimplify p46v15 p)
            (ghexpr_after_one_cycle p46v26 p)))
  

gsiHOpt :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun (HExpr Naam) (HExpr Naam))

gsiHOpt psiHOpt p =
  T.fun1 asiHOpt psiHOpt p hsiHOpt
  where
  
  hsiHOpt (T.R (HMeet fes) _) p = T.ap1 p53v25 p (gsiHOpt_meet p53v25 p) fes
  hsiHOpt (T.R (HApp fh1 fh2) _) p =
    T.ap2 p54v25 p (gsiHOpt_app p54v25 p)
      (T.ap1 p54v37 p (gsiHOpt p54v37 p) fh1)
      (T.ap1 p54v49 p (gsiHOpt p54v49 p) fh2)
  hsiHOpt (fp@(T.R (HPoint _) _)) p = T.projection p55v25 p fp
  hsiHOpt (fv@(T.R (HVar _) _)) p = T.projection p56v25 p fv
  hsiHOpt (T.R (HLam fvs fe) _) p =
    T.con2 p57v25 p HLam aHLam fvs (T.ap1 p57v34 p (gsiHOpt p57v34 p) fe)
  hsiHOpt (T.R (HTable ft) _) p =
    T.con1 p58v25 p HTable aHTable
      (T.ap2 p58v33 p (gmap2nd p58v33 p) (gsiHOpt p58v40 p) ft)
  hsiHOpt _ p = T.fatal p
  

gsiHOpt_meet ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List (HExpr Naam)) (HExpr Naam))

gsiHOpt_meet psiHOpt_meet p =
  T.fun1 asiHOpt_meet psiHOpt_meet p hsiHOpt_meet
  where
  
  hsiHOpt_meet fes p =
    let
      gpresimplified ppresimplified p =
        T.constUse ppresimplified p spresimplified
      spresimplified =
        T.constDef p a67v10presimplified
          (\ p -> T.ap2 p67v26 p (gmap p67v26 p) (gsiHOpt p67v30 p) fes)
      glitsplit plitsplit p =
        T.fun2 a68v10litsplit plitsplit p hlitsplit
        where
        
        hlitsplit (T.R (T.Tuple2 flits fnonlits) _) (T.R (HPoint fp) _) p =
          T.con2 p68v48 p T.Tuple2 T.aTuple2
            (T.con2 p68v50 p T.Cons T.aCons fp flits) fnonlits
        hlitsplit (T.R (T.Tuple2 flits fnonlits) _) fother p =
          T.con2 p69v48 p T.Tuple2 T.aTuple2 flits
            (T.con2 p69v60 p T.Cons T.aCons fother fnonlits)
        hlitsplit _ _ p = T.fatal p
        
      glits plits p = T.constUse plits p slits
      gnonlits plits p = T.constUse plits p snonlits
      j70v10lits =
        case
          T.ap3 p70v28 p (gfoldl p70v28 p) (glitsplit p70v34 p)
            (T.con2 p70v43 p T.Tuple2 T.aTuple2 (T.con0 p70v44 p T.List T.aList)
              (T.con0 p70v47 p T.List T.aList)) (gpresimplified p70v51 p) of
          T.R (T.Tuple2 flits fnonlits) klits -> (klits,flits,fnonlits)
          _ -> T.fatal p
      slits =
        T.constDef p a70v11lits
          (\ _ ->
            case j70v10lits of
              (klits,flits,fnonlits) -> T.projection p70v11 klits flits)
      snonlits =
        T.constDef p a70v17nonlits
          (\ _ ->
            case j70v10lits of
              (klits,flits,fnonlits) -> T.projection p70v17 klits fnonlits)
      gonelit ponelit p = T.constUse ponelit p sonelit
      sonelit =
        T.constDef p a71v10onelit
          (\ p ->
            T.ap2 p71v19 p (gfoldr1 p71v19 p) (p71v27 !\/ p) (glits p71v31 p))
      in
      (T.cif p73v6 p (T.ap1 p73v18 p (gnull p73v18 p) (glits p73v23 p))
        (\ p -> T.con1 p74v18 p HMeet aHMeet (gpresimplified p74v24 p))
        (\ p ->
          T.cif p75v11 p
            (T.ap1 p75v18 p (gavIsTopR p75v18 p) (gonelit p75v27 p))
            (\ p -> T.con1 p76v18 p HPoint aHPoint (gonelit p76v25 p))
            (\ p ->
              T.cif p77v11 p
                (T.ap1 p77v18 p (gavIsBottomR p77v18 p) (gonelit p77v30 p))
                (\ p ->
                  T.ap2 p78v18 p (gaeMkMeet p78v18 p)
                    (T.con1 p78v28 p HPoint aHPoint (gonelit p78v35 p))
                    (gnonlits p78v43 p))
                (\ p ->
                  T.ap2 p79v18 p (gaeMkMeet p79v18 p)
                    (T.con1 p79v28 p HPoint aHPoint (gonelit p79v35 p))
                    (T.con2 p79v59 p T.Cons T.aCons
                      (T.con1 p79v45 p HPoint aHPoint (gonelit p79v52 p))
                      (gnonlits p79v60 p))))))
  

gsiHOpt_app ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (HExpr Naam) (T.Fun (HExpr Naam) (HExpr Naam)))

gsiHOpt_app psiHOpt_app p =
  T.fun2 asiHOpt_app psiHOpt_app p hsiHOpt_app
  where
  
  hsiHOpt_app (T.R (HTable ft) _) (T.R (HPoint fp) _) p =
    T.ap1 p89v5 p (gsiHOpt p89v5 p)
      (T.ap3 p89v13 p (gutSureLookup p89v13 p) ft
        (T.fromLitString p89v28 p "siHOpt_app") fp)
  hsiHOpt_app (T.R (HPoint fp1) _) (T.R (HPoint fp2) _) p =
    T.con1 p92v5 p HPoint aHPoint
      (T.ap2 p92v13 p (gapApply p92v13 p) fp1 (T.fromExpList p92v24 p [fp2]))
  hsiHOpt_app fh1_other fh2_other p =
    T.con2 p94v32 p HApp aHApp fh1_other fh2_other
  

tSimplify = T.mkModule "Simplify" "Simplify.hs" Prelude.True

asiVectorise = T.mkVariable tSimplify 190001 3 1 "siVectorise" Prelude.False

asiSimplify = T.mkVariable tSimplify 400001 3 1 "siSimplify" Prelude.False

asiHOpt = T.mkVariable tSimplify 530001 3 1 "siHOpt" Prelude.False

asiHOpt_meet = T.mkVariable tSimplify 660001 3 1 "siHOpt_meet" Prelude.False

asiHOpt_app = T.mkVariable tSimplify 880001 3 2 "siHOpt_app" Prelude.False

a42v9hexpr_after_one_cycle =
  T.mkVariable tSimplify 420009 3 0 "hexpr_after_one_cycle" Prelude.True

a67v10presimplified =
  T.mkVariable tSimplify 670010 3 0 "presimplified" Prelude.True

a68v10litsplit = T.mkVariable tSimplify 680010 3 2 "litsplit" Prelude.True

a70v11lits = T.mkVariable tSimplify 700011 3 0 "lits" Prelude.True

a70v17nonlits = T.mkVariable tSimplify 700017 3 0 "nonlits" Prelude.True

a71v10onelit = T.mkVariable tSimplify 710010 3 0 "onelit" Prelude.True

p19v1 = T.mkSrcPos tSimplify 190001

p20v6 = T.mkSrcPos tSimplify 200006

p20v19 = T.mkSrcPos tSimplify 200019

p20v28 = T.mkSrcPos tSimplify 200028

p22v6 = T.mkSrcPos tSimplify 220006

p22v15 = T.mkSrcPos tSimplify 220015

p24v6 = T.mkSrcPos tSimplify 240006

p24v12 = T.mkSrcPos tSimplify 240012

p24v20 = T.mkSrcPos tSimplify 240020

p24v27 = T.mkSrcPos tSimplify 240027

p24v44 = T.mkSrcPos tSimplify 240044

p26v6 = T.mkSrcPos tSimplify 260006

p26v11 = T.mkSrcPos tSimplify 260011

p27v25 = T.mkSrcPos tSimplify 270025

p27v38 = T.mkSrcPos tSimplify 270038

p27v40 = T.mkSrcPos tSimplify 270040

p27v41 = T.mkSrcPos tSimplify 270041

p28v25 = T.mkSrcPos tSimplify 280025

p28v31 = T.mkSrcPos tSimplify 280031

p28v41 = T.mkSrcPos tSimplify 280041

p28v42 = T.mkSrcPos tSimplify 280042

p29v25 = T.mkSrcPos tSimplify 290025

p29v31 = T.mkSrcPos tSimplify 290031

p29v39 = T.mkSrcPos tSimplify 290039

p29v40 = T.mkSrcPos tSimplify 290040

p30v25 = T.mkSrcPos tSimplify 300025

p30v39 = T.mkSrcPos tSimplify 300039

p31v26 = T.mkSrcPos tSimplify 310026

p32v28 = T.mkSrcPos tSimplify 320028

p33v26 = T.mkSrcPos tSimplify 330026

p33v33 = T.mkSrcPos tSimplify 330033

p33v37 = T.mkSrcPos tSimplify 330037

p40v1 = T.mkSrcPos tSimplify 400001

p42v9 = T.mkSrcPos tSimplify 420009

p42v33 = T.mkSrcPos tSimplify 420033

p44v9 = T.mkSrcPos tSimplify 440009

p44v21 = T.mkSrcPos tSimplify 440021

p44v24 = T.mkSrcPos tSimplify 440024

p45v15 = T.mkSrcPos tSimplify 450015

p46v15 = T.mkSrcPos tSimplify 460015

p46v26 = T.mkSrcPos tSimplify 460026

p53v1 = T.mkSrcPos tSimplify 530001

p53v25 = T.mkSrcPos tSimplify 530025

p54v25 = T.mkSrcPos tSimplify 540025

p54v37 = T.mkSrcPos tSimplify 540037

p54v49 = T.mkSrcPos tSimplify 540049

p55v25 = T.mkSrcPos tSimplify 550025

p56v25 = T.mkSrcPos tSimplify 560025

p57v25 = T.mkSrcPos tSimplify 570025

p57v34 = T.mkSrcPos tSimplify 570034

p58v25 = T.mkSrcPos tSimplify 580025

p58v33 = T.mkSrcPos tSimplify 580033

p58v40 = T.mkSrcPos tSimplify 580040

p66v1 = T.mkSrcPos tSimplify 660001

p67v10 = T.mkSrcPos tSimplify 670010

p67v26 = T.mkSrcPos tSimplify 670026

p67v30 = T.mkSrcPos tSimplify 670030

p68v10 = T.mkSrcPos tSimplify 680010

p68v48 = T.mkSrcPos tSimplify 680048

p68v50 = T.mkSrcPos tSimplify 680050

p69v48 = T.mkSrcPos tSimplify 690048

p69v60 = T.mkSrcPos tSimplify 690060

p70v11 = T.mkSrcPos tSimplify 700011

p70v17 = T.mkSrcPos tSimplify 700017

p70v28 = T.mkSrcPos tSimplify 700028

p70v34 = T.mkSrcPos tSimplify 700034

p70v43 = T.mkSrcPos tSimplify 700043

p70v44 = T.mkSrcPos tSimplify 700044

p70v47 = T.mkSrcPos tSimplify 700047

p70v51 = T.mkSrcPos tSimplify 700051

p71v10 = T.mkSrcPos tSimplify 710010

p71v19 = T.mkSrcPos tSimplify 710019

p71v27 = T.mkSrcPos tSimplify 710027

p71v31 = T.mkSrcPos tSimplify 710031

p73v6 = T.mkSrcPos tSimplify 730006

p73v18 = T.mkSrcPos tSimplify 730018

p73v23 = T.mkSrcPos tSimplify 730023

p74v18 = T.mkSrcPos tSimplify 740018

p74v24 = T.mkSrcPos tSimplify 740024

p75v11 = T.mkSrcPos tSimplify 750011

p75v18 = T.mkSrcPos tSimplify 750018

p75v27 = T.mkSrcPos tSimplify 750027

p76v18 = T.mkSrcPos tSimplify 760018

p76v25 = T.mkSrcPos tSimplify 760025

p77v11 = T.mkSrcPos tSimplify 770011

p77v18 = T.mkSrcPos tSimplify 770018

p77v30 = T.mkSrcPos tSimplify 770030

p78v18 = T.mkSrcPos tSimplify 780018

p78v28 = T.mkSrcPos tSimplify 780028

p78v35 = T.mkSrcPos tSimplify 780035

p78v43 = T.mkSrcPos tSimplify 780043

p79v18 = T.mkSrcPos tSimplify 790018

p79v28 = T.mkSrcPos tSimplify 790028

p79v35 = T.mkSrcPos tSimplify 790035

p79v59 = T.mkSrcPos tSimplify 790059

p79v45 = T.mkSrcPos tSimplify 790045

p79v52 = T.mkSrcPos tSimplify 790052

p79v60 = T.mkSrcPos tSimplify 790060

p88v1 = T.mkSrcPos tSimplify 880001

p89v5 = T.mkSrcPos tSimplify 890005

p89v13 = T.mkSrcPos tSimplify 890013

p89v28 = T.mkSrcPos tSimplify 890028

p92v5 = T.mkSrcPos tSimplify 920005

p92v13 = T.mkSrcPos tSimplify 920013

p92v24 = T.mkSrcPos tSimplify 920024

p94v32 = T.mkSrcPos tSimplify 940032
