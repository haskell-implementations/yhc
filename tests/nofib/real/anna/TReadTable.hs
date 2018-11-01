module TReadTable
  (grtReadTable,grtLex,grtPWithComma,grtListMain,grtList,grtListDomain,grtDomain
    ,grtPair,grtTable) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 
import TParser2 
import TChar  (gisDigit)

grtReadTable ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun String (T.List (T.Tuple2 Domain Int)))

grtReadTable prtReadTable p =
  T.fun1 artReadTable prtReadTable p hrtReadTable
  where
  
  hrtReadTable fs p =
    T.ccase p19v6 p
      (let
        v19v6v1 (T.R (PFail (T.R T.List _)) _) p =
          T.ap1 p21v15 p (gmyFail p21v15 p)
            (T.fromLitString p21v22 p "Unexpected end of lattice table")
        v19v6v1 (T.R (PFail (T.R (T.Cons (T.R (T.Tuple2 fn ft) _) _) _)) _) p =
          T.ap1 p23v15 p (gmyFail p23v15 p)
            (T.ap2 p23v62 p (p23v62 !++ p)
              (T.fromLitString p23v23 p "Syntax error in lattice table, line ")
              (T.ap2 p23v72 p (p23v72 !++ p)
                (T.ap1 p23v65 p (gshow p23v65 p) fn)
                (T.fromLitString p23v75 p ".")))
        v19v6v1 (T.R (POk ftab (T.R T.List _)) _) p = T.projection p25v15 p ftab
        v19v6v1 (T.R (POk ftab (T.R (T.Cons (T.R (T.Tuple2 fn ft) _) _) _)) _)
          p =
          T.ap1 p27v15 p (gmyFail p27v15 p)
            (T.ap2 p27v62 p (p27v62 !++ p)
              (T.fromLitString p27v23 p "Syntax error in lattice table, line ")
              (T.ap2 p27v72 p (p27v72 !++ p)
                (T.ap1 p27v65 p (gshow p27v65 p) fn)
                (T.fromLitString p27v75 p ".")))
        v19v6v1 _ p = T.fatal p in (v19v6v1))
      (T.ap1 p19v11 p (grtTable p19v11 p)
        (T.ap2 p19v20 p (grtLex p19v20 p)
          (T.ap1 p19v26 p (TPreludeBasic.gfromInteger p19v26 p)
            (T.conInteger p19v26 p 1)) fs))
  

grtLex ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int (T.Fun String (T.List Token)))

grtLex prtLex p =
  T.fun2 artLex prtLex p hrtLex
  where
  
  hrtLex fn (T.R T.List _) p = T.con0 p34v14 p T.List T.aList
  hrtLex fn (T.R (T.Cons (T.R '\n' _) fcs) _) p =
    T.ap2 p36v21 p (grtLex p36v21 p)
      (T.ap2 p36v29 p (p36v29 !+ p) fn
        (T.ap1 p36v30 p (TPreludeBasic.gfromInteger p36v30 p)
          (T.conInteger p36v30 p 1))) fcs
  hrtLex fn (T.R (T.Cons (T.R ' ' _) fcs) _) p =
    T.ap2 p37v20 p (grtLex p37v20 p) fn fcs
  hrtLex fn (T.R (T.Cons (T.R '\t' _) fcs) _) p =
    T.ap2 p38v21 p (grtLex p38v21 p) fn fcs
  hrtLex fn (T.R (T.Cons (T.R '(' _) fcs) _) p =
    T.con2 p40v28 p T.Cons T.aCons
      (T.con2 p40v20 p T.Tuple2 T.aTuple2 fn (T.fromLitString p40v24 p "("))
      (T.ap2 p40v29 p (grtLex p40v29 p) fn fcs)
  hrtLex fn (T.R (T.Cons (T.R ')' _) fcs) _) p =
    T.con2 p41v28 p T.Cons T.aCons
      (T.con2 p41v20 p T.Tuple2 T.aTuple2 fn (T.fromLitString p41v24 p ")"))
      (T.ap2 p41v29 p (grtLex p41v29 p) fn fcs)
  hrtLex fn (T.R (T.Cons (T.R '[' _) fcs) _) p =
    T.con2 p42v28 p T.Cons T.aCons
      (T.con2 p42v20 p T.Tuple2 T.aTuple2 fn (T.fromLitString p42v24 p "["))
      (T.ap2 p42v29 p (grtLex p42v29 p) fn fcs)
  hrtLex fn (T.R (T.Cons (T.R ']' _) fcs) _) p =
    T.con2 p43v28 p T.Cons T.aCons
      (T.con2 p43v20 p T.Tuple2 T.aTuple2 fn (T.fromLitString p43v24 p "]"))
      (T.ap2 p43v29 p (grtLex p43v29 p) fn fcs)
  hrtLex fn (T.R (T.Cons (T.R ',' _) fcs) _) p =
    T.con2 p44v28 p T.Cons T.aCons
      (T.con2 p44v20 p T.Tuple2 T.aTuple2 fn (T.fromLitString p44v24 p ","))
      (T.ap2 p44v29 p (grtLex p44v29 p) fn fcs)
  hrtLex fn
    (T.R
      (T.Cons (T.R 'T' _)
        (T.R (T.Cons (T.R 'w' _) (T.R (T.Cons (T.R 'o' _) fcs) _)) _)) _) p =
    T.con2 p46v45 p T.Cons T.aCons
      (T.con2 p46v37 p T.Tuple2 T.aTuple2 fn (T.fromLitString p46v41 p "T"))
      (T.ap2 p46v46 p (grtLex p46v46 p) fn fcs)
  hrtLex fn
    (T.R
      (T.Cons (T.R 'F' _)
        (T.R
          (T.Cons (T.R 'u' _)
            (T.R (T.Cons (T.R 'n' _) (T.R (T.Cons (T.R 'c' _) fcs) _)) _)) _))
      _) p =
    T.con2 p47v45 p T.Cons T.aCons
      (T.con2 p47v37 p T.Tuple2 T.aTuple2 fn (T.fromLitString p47v41 p "F"))
      (T.ap2 p47v46 p (grtLex p47v46 p) fn fcs)
  hrtLex fn
    (T.R
      (T.Cons (T.R 'L' _)
        (T.R
          (T.Cons (T.R 'i' _)
            (T.R
              (T.Cons (T.R 'f' _)
                (T.R (T.Cons (T.R 't' _) (T.R (T.Cons (T.R '1' _) fcs) _)) _))
              _)) _)) _) p =
    T.con2 p48v45 p T.Cons T.aCons
      (T.con2 p48v37 p T.Tuple2 T.aTuple2 fn (T.fromLitString p48v41 p "L"))
      (T.ap2 p48v46 p (grtLex p48v46 p) fn fcs)
  hrtLex fn
    (T.R
      (T.Cons (T.R 'L' _)
        (T.R
          (T.Cons (T.R 'i' _)
            (T.R
              (T.Cons (T.R 'f' _)
                (T.R (T.Cons (T.R 't' _) (T.R (T.Cons (T.R '2' _) fcs) _)) _))
              _)) _)) _) p =
    T.con2 p49v45 p T.Cons T.aCons
      (T.con2 p49v37 p T.Tuple2 T.aTuple2 fn (T.fromLitString p49v41 p "M"))
      (T.ap2 p49v46 p (grtLex p49v46 p) fn fcs)
  hrtLex fn (T.R (T.Cons fc fcs) _) p =
    T.cguard p52v6 p (T.ap1 p52v6 p (gisDigit p52v6 p) fc)
      (\ p ->
        T.con2 p53v33 p T.Cons T.aCons
          (T.con2 p53v6 p T.Tuple2 T.aTuple2 fn
            (T.con2 p53v11 p T.Cons T.aCons fc
              (T.ap2 p53v12 p (gtakeWhile p53v12 p) (gisDigit p53v22 p) fcs)))
          (T.ap2 p53v34 p (grtLex p53v34 p) fn
            (T.ap2 p53v43 p (gdropWhile p53v43 p) (gisDigit p53v53 p) fcs)))
      (\ p ->
        T.cguard p54v6 p (gotherwise p54v6 p)
          (\ p ->
            T.ap1 p55v6 p (gmyFail p55v6 p)
              (T.ap2 p55v35 p (p55v35 !++ p)
                (T.fromLitString p55v14 p "Illegal character ")
                (T.ap2 p55v45 p (p55v45 !++ p)
                  (T.ap1 p55v38 p (gshow p55v38 p) fc)
                  (T.ap2 p56v39 p (p56v39 !++ p)
                    (T.fromLitString p56v12 p " in lattice table, line ")
                    (T.ap2 p56v49 p (p56v49 !++ p)
                      (T.ap1 p56v42 p (gshow p56v42 p) fn)
                      (T.fromLitString p56v52 p ".")))))) (\ p -> T.fatal p))
  hrtLex _ _ p = T.fatal p
  

grtPWithComma prtPWithComma p =
  T.fun1 artPWithComma prtPWithComma p hrtPWithComma
  where
  
  hrtPWithComma fp p =
    T.ap3 p61v18 p (gpaThen2 p61v18 p)
      (T.fun2 T.mkLambda p61v27 p (\ fa fb p -> T.projection p61v35 p fa)) fp
      (T.ap1 p61v41 p (gpaLit p61v41 p) (T.fromLitString p61v47 p ","))
  

grtListMain prtListMain p =
  T.fun1 artListMain prtListMain p hrtListMain
  where
  
  hrtListMain fp p =
    T.ap1 p66v5 p (gpaAlts p66v5 p)
      (T.fromExpList p67v5 p
        [T.con2 p67v7 p T.Tuple2 T.aTuple2
            (T.ap2 p67v10 p (TPrelude.gflip p67v10 p) (p67v10 !== p)
              (T.fromLitString p67v12 p "]"))
            (T.ap2 p68v9 p (gpaApply p68v9 p)
              (T.ap1 p68v18 p (gpaLit p68v18 p) (T.fromLitString p68v24 p "]"))
              (T.ap1 p68v30 p (gconst p68v30 p)
                (T.con0 p68v36 p T.List T.aList)))
          ,T.con2 p70v7 p T.Tuple2 T.aTuple2
            (T.ap1 p70v9 p (gconst p70v9 p) (T.con0 p70v15 p True aTrue))
            (T.ap4 p71v9 p (gpaThen3 p71v9 p)
              (T.fun3 T.mkLambda p71v18 p
                (\ fa fb fc p ->
                  T.ap2 p71v30 p (p71v30 !++ p) fa
                    (T.fromExpList p71v33 p [fb])))
              (T.ap1 p72v17 p (gpaZeroOrMore p72v17 p)
                (T.ap1 p72v31 p (grtPWithComma p72v31 p) fp)) fp
              (T.ap1 p72v51 p (gpaLit p72v51 p)
                (T.fromLitString p72v57 p "]")))])
  

grtList prtList p =
  T.fun1 artList prtList p hrtList
  where
  
  hrtList fp p =
    T.ap3 p76v12 p (gpaThen2 p76v12 p)
      (T.fun2 T.mkLambda p76v21 p (\ fa fb p -> T.projection p76v29 p fb))
      (T.ap1 p76v33 p (gpaLit p76v33 p) (T.fromLitString p76v39 p "["))
      (T.ap1 p76v45 p (grtListMain p76v45 p) fp)
  

grtListDomain prtListDomain p = T.constUse prtListDomain p srtListDomain

srtListDomain =
  T.constDef T.mkRoot artListDomain
    (\ p -> T.ap1 p80v16 p (grtList p80v16 p) (grtDomain p80v23 p))

grtDomain prtDomain p = T.constUse prtDomain p srtDomain

srtDomain =
  T.constDef T.mkRoot artDomain
    (\ p ->
      T.ap1 p85v5 p (gpaAlts p85v5 p)
        (T.fromExpList p86v5 p
          [T.con2 p87v8 p T.Tuple2 T.aTuple2
              (T.ap2 p87v11 p (TPrelude.gflip p87v11 p) (p87v11 !== p)
                (T.fromLitString p87v13 p "("))
              (T.ap4 p87v19 p (gpaThen3 p87v19 p)
                (T.fun3 T.mkLambda p87v28 p
                  (\ fa fb fc p -> T.projection p87v38 p fb))
                (T.ap1 p87v42 p (gpaLit p87v42 p)
                  (T.fromLitString p87v48 p "(")) (grtDomain p87v53 p)
                (T.ap1 p87v63 p (gpaLit p87v63 p)
                  (T.fromLitString p87v69 p ")")))
            ,T.con2 p88v8 p T.Tuple2 T.aTuple2
              (T.ap2 p88v11 p (TPrelude.gflip p88v11 p) (p88v11 !== p)
                (T.fromLitString p88v13 p "T"))
              (T.ap2 p88v19 p (gpaApply p88v19 p)
                (T.ap1 p88v28 p (gpaLit p88v28 p)
                  (T.fromLitString p88v34 p "T"))
                (T.ap1 p88v40 p (gconst p88v40 p) (T.con0 p88v46 p Two aTwo)))
            ,T.con2 p89v8 p T.Tuple2 T.aTuple2
              (T.ap2 p89v11 p (TPrelude.gflip p89v11 p) (p89v11 !== p)
                (T.fromLitString p89v13 p "L"))
              (T.ap3 p89v19 p (gpaThen2 p89v19 p)
                (T.fun2 T.mkLambda p89v28 p
                  (\ fa fb p -> T.con1 p89v36 p Lift1 aLift1 fb))
                (T.ap1 p89v46 p (gpaLit p89v46 p)
                  (T.fromLitString p89v52 p "L")) (grtListDomain p89v57 p))
            ,T.con2 p90v8 p T.Tuple2 T.aTuple2
              (T.ap2 p90v11 p (TPrelude.gflip p90v11 p) (p90v11 !== p)
                (T.fromLitString p90v13 p "M"))
              (T.ap3 p90v19 p (gpaThen2 p90v19 p)
                (T.fun2 T.mkLambda p90v28 p
                  (\ fa fb p -> T.con1 p90v36 p Lift2 aLift2 fb))
                (T.ap1 p90v46 p (gpaLit p90v46 p)
                  (T.fromLitString p90v52 p "M")) (grtListDomain p90v57 p))
            ,T.con2 p91v8 p T.Tuple2 T.aTuple2
              (T.ap2 p91v11 p (TPrelude.gflip p91v11 p) (p91v11 !== p)
                (T.fromLitString p91v13 p "F"))
              (T.ap4 p91v19 p (gpaThen3 p91v19 p)
                (T.fun3 T.mkLambda p91v28 p
                  (\ fa fb fc p -> T.con2 p91v38 p Func aFunc fb fc))
                (T.ap1 p92v28 p (gpaLit p92v28 p)
                  (T.fromLitString p92v34 p "F")) (grtListDomain p92v39 p)
                (grtDomain p92v52 p))]))

grtPair prtPair p =
  T.fun2 artPair prtPair p hrtPair
  where
  
  hrtPair fpa fpb p =
    T.ap5 p98v6 p (gpaThen4 p98v6 p)
      (T.fun4 T.mkLambda p98v15 p
        (\ fa fb fc fd p -> T.con2 p98v27 p T.Tuple2 T.aTuple2 fb fd))
      (T.ap1 p98v35 p (gpaLit p98v35 p) (T.fromLitString p98v41 p "(")) fpa
      (T.ap1 p98v50 p (gpaLit p98v50 p) (T.fromLitString p98v56 p ","))
      (T.ap3 p99v6 p (gpaThen2 p99v6 p)
        (T.fun2 T.mkLambda p99v15 p (\ fa fb p -> T.projection p99v23 p fa)) fpb
        (T.ap1 p99v38 p (gpaLit p99v38 p) (T.fromLitString p99v44 p ")")))
  

grtTable prtTable p = T.constUse prtTable p srtTable

srtTable =
  T.constDef T.mkRoot artTable
    (\ p ->
      T.ap1 p104v5 p (grtList p104v5 p)
        (T.ap2 p104v13 p (grtPair p104v13 p) (grtDomain p104v20 p)
          (gpaNum p104v29 p)))

tReadTable = T.mkModule "ReadTable" "ReadTable.hs" Prelude.True

artReadTable = T.mkVariable tReadTable 180001 3 1 "rtReadTable" Prelude.False

artLex = T.mkVariable tReadTable 340001 3 2 "rtLex" Prelude.False

artPWithComma = T.mkVariable tReadTable 610001 3 1 "rtPWithComma" Prelude.False

artListMain = T.mkVariable tReadTable 650001 3 1 "rtListMain" Prelude.False

artList = T.mkVariable tReadTable 760001 3 1 "rtList" Prelude.False

artListDomain = T.mkVariable tReadTable 800001 3 0 "rtListDomain" Prelude.False

artDomain = T.mkVariable tReadTable 840001 3 0 "rtDomain" Prelude.False

artPair = T.mkVariable tReadTable 970001 3 2 "rtPair" Prelude.False

artTable = T.mkVariable tReadTable 1030001 3 0 "rtTable" Prelude.False

p18v1 = T.mkSrcPos tReadTable 180001

p19v6 = T.mkSrcPos tReadTable 190006

p19v11 = T.mkSrcPos tReadTable 190011

p19v20 = T.mkSrcPos tReadTable 190020

p19v26 = T.mkSrcPos tReadTable 190026

p21v15 = T.mkSrcPos tReadTable 210015

p21v22 = T.mkSrcPos tReadTable 210022

p23v15 = T.mkSrcPos tReadTable 230015

p23v62 = T.mkSrcPos tReadTable 230062

p23v23 = T.mkSrcPos tReadTable 230023

p23v72 = T.mkSrcPos tReadTable 230072

p23v65 = T.mkSrcPos tReadTable 230065

p23v75 = T.mkSrcPos tReadTable 230075

p25v15 = T.mkSrcPos tReadTable 250015

p27v15 = T.mkSrcPos tReadTable 270015

p27v62 = T.mkSrcPos tReadTable 270062

p27v23 = T.mkSrcPos tReadTable 270023

p27v72 = T.mkSrcPos tReadTable 270072

p27v65 = T.mkSrcPos tReadTable 270065

p27v75 = T.mkSrcPos tReadTable 270075

p34v1 = T.mkSrcPos tReadTable 340001

p34v14 = T.mkSrcPos tReadTable 340014

p36v21 = T.mkSrcPos tReadTable 360021

p36v29 = T.mkSrcPos tReadTable 360029

p36v30 = T.mkSrcPos tReadTable 360030

p37v20 = T.mkSrcPos tReadTable 370020

p38v21 = T.mkSrcPos tReadTable 380021

p40v28 = T.mkSrcPos tReadTable 400028

p40v20 = T.mkSrcPos tReadTable 400020

p40v24 = T.mkSrcPos tReadTable 400024

p40v29 = T.mkSrcPos tReadTable 400029

p41v28 = T.mkSrcPos tReadTable 410028

p41v20 = T.mkSrcPos tReadTable 410020

p41v24 = T.mkSrcPos tReadTable 410024

p41v29 = T.mkSrcPos tReadTable 410029

p42v28 = T.mkSrcPos tReadTable 420028

p42v20 = T.mkSrcPos tReadTable 420020

p42v24 = T.mkSrcPos tReadTable 420024

p42v29 = T.mkSrcPos tReadTable 420029

p43v28 = T.mkSrcPos tReadTable 430028

p43v20 = T.mkSrcPos tReadTable 430020

p43v24 = T.mkSrcPos tReadTable 430024

p43v29 = T.mkSrcPos tReadTable 430029

p44v28 = T.mkSrcPos tReadTable 440028

p44v20 = T.mkSrcPos tReadTable 440020

p44v24 = T.mkSrcPos tReadTable 440024

p44v29 = T.mkSrcPos tReadTable 440029

p46v45 = T.mkSrcPos tReadTable 460045

p46v37 = T.mkSrcPos tReadTable 460037

p46v41 = T.mkSrcPos tReadTable 460041

p46v46 = T.mkSrcPos tReadTable 460046

p47v45 = T.mkSrcPos tReadTable 470045

p47v37 = T.mkSrcPos tReadTable 470037

p47v41 = T.mkSrcPos tReadTable 470041

p47v46 = T.mkSrcPos tReadTable 470046

p48v45 = T.mkSrcPos tReadTable 480045

p48v37 = T.mkSrcPos tReadTable 480037

p48v41 = T.mkSrcPos tReadTable 480041

p48v46 = T.mkSrcPos tReadTable 480046

p49v45 = T.mkSrcPos tReadTable 490045

p49v37 = T.mkSrcPos tReadTable 490037

p49v41 = T.mkSrcPos tReadTable 490041

p49v46 = T.mkSrcPos tReadTable 490046

p52v6 = T.mkSrcPos tReadTable 520006

p53v33 = T.mkSrcPos tReadTable 530033

p53v6 = T.mkSrcPos tReadTable 530006

p53v11 = T.mkSrcPos tReadTable 530011

p53v12 = T.mkSrcPos tReadTable 530012

p53v22 = T.mkSrcPos tReadTable 530022

p53v34 = T.mkSrcPos tReadTable 530034

p53v43 = T.mkSrcPos tReadTable 530043

p53v53 = T.mkSrcPos tReadTable 530053

p54v6 = T.mkSrcPos tReadTable 540006

p55v6 = T.mkSrcPos tReadTable 550006

p55v35 = T.mkSrcPos tReadTable 550035

p55v14 = T.mkSrcPos tReadTable 550014

p55v45 = T.mkSrcPos tReadTable 550045

p55v38 = T.mkSrcPos tReadTable 550038

p56v39 = T.mkSrcPos tReadTable 560039

p56v12 = T.mkSrcPos tReadTable 560012

p56v49 = T.mkSrcPos tReadTable 560049

p56v42 = T.mkSrcPos tReadTable 560042

p56v52 = T.mkSrcPos tReadTable 560052

p61v1 = T.mkSrcPos tReadTable 610001

p61v18 = T.mkSrcPos tReadTable 610018

p61v27 = T.mkSrcPos tReadTable 610027

p61v35 = T.mkSrcPos tReadTable 610035

p61v41 = T.mkSrcPos tReadTable 610041

p61v47 = T.mkSrcPos tReadTable 610047

p65v1 = T.mkSrcPos tReadTable 650001

p66v5 = T.mkSrcPos tReadTable 660005

p67v5 = T.mkSrcPos tReadTable 670005

p67v7 = T.mkSrcPos tReadTable 670007

p67v10 = T.mkSrcPos tReadTable 670010

p67v12 = T.mkSrcPos tReadTable 670012

p68v9 = T.mkSrcPos tReadTable 680009

p68v18 = T.mkSrcPos tReadTable 680018

p68v24 = T.mkSrcPos tReadTable 680024

p68v30 = T.mkSrcPos tReadTable 680030

p68v36 = T.mkSrcPos tReadTable 680036

p70v7 = T.mkSrcPos tReadTable 700007

p70v9 = T.mkSrcPos tReadTable 700009

p70v15 = T.mkSrcPos tReadTable 700015

p71v9 = T.mkSrcPos tReadTable 710009

p71v18 = T.mkSrcPos tReadTable 710018

p71v30 = T.mkSrcPos tReadTable 710030

p71v33 = T.mkSrcPos tReadTable 710033

p72v17 = T.mkSrcPos tReadTable 720017

p72v31 = T.mkSrcPos tReadTable 720031

p72v51 = T.mkSrcPos tReadTable 720051

p72v57 = T.mkSrcPos tReadTable 720057

p76v1 = T.mkSrcPos tReadTable 760001

p76v12 = T.mkSrcPos tReadTable 760012

p76v21 = T.mkSrcPos tReadTable 760021

p76v29 = T.mkSrcPos tReadTable 760029

p76v33 = T.mkSrcPos tReadTable 760033

p76v39 = T.mkSrcPos tReadTable 760039

p76v45 = T.mkSrcPos tReadTable 760045

p80v1 = T.mkSrcPos tReadTable 800001

p80v16 = T.mkSrcPos tReadTable 800016

p80v23 = T.mkSrcPos tReadTable 800023

p84v1 = T.mkSrcPos tReadTable 840001

p85v5 = T.mkSrcPos tReadTable 850005

p86v5 = T.mkSrcPos tReadTable 860005

p87v8 = T.mkSrcPos tReadTable 870008

p87v11 = T.mkSrcPos tReadTable 870011

p87v13 = T.mkSrcPos tReadTable 870013

p87v19 = T.mkSrcPos tReadTable 870019

p87v28 = T.mkSrcPos tReadTable 870028

p87v38 = T.mkSrcPos tReadTable 870038

p87v42 = T.mkSrcPos tReadTable 870042

p87v48 = T.mkSrcPos tReadTable 870048

p87v53 = T.mkSrcPos tReadTable 870053

p87v63 = T.mkSrcPos tReadTable 870063

p87v69 = T.mkSrcPos tReadTable 870069

p88v8 = T.mkSrcPos tReadTable 880008

p88v11 = T.mkSrcPos tReadTable 880011

p88v13 = T.mkSrcPos tReadTable 880013

p88v19 = T.mkSrcPos tReadTable 880019

p88v28 = T.mkSrcPos tReadTable 880028

p88v34 = T.mkSrcPos tReadTable 880034

p88v40 = T.mkSrcPos tReadTable 880040

p88v46 = T.mkSrcPos tReadTable 880046

p89v8 = T.mkSrcPos tReadTable 890008

p89v11 = T.mkSrcPos tReadTable 890011

p89v13 = T.mkSrcPos tReadTable 890013

p89v19 = T.mkSrcPos tReadTable 890019

p89v28 = T.mkSrcPos tReadTable 890028

p89v36 = T.mkSrcPos tReadTable 890036

p89v46 = T.mkSrcPos tReadTable 890046

p89v52 = T.mkSrcPos tReadTable 890052

p89v57 = T.mkSrcPos tReadTable 890057

p90v8 = T.mkSrcPos tReadTable 900008

p90v11 = T.mkSrcPos tReadTable 900011

p90v13 = T.mkSrcPos tReadTable 900013

p90v19 = T.mkSrcPos tReadTable 900019

p90v28 = T.mkSrcPos tReadTable 900028

p90v36 = T.mkSrcPos tReadTable 900036

p90v46 = T.mkSrcPos tReadTable 900046

p90v52 = T.mkSrcPos tReadTable 900052

p90v57 = T.mkSrcPos tReadTable 900057

p91v8 = T.mkSrcPos tReadTable 910008

p91v11 = T.mkSrcPos tReadTable 910011

p91v13 = T.mkSrcPos tReadTable 910013

p91v19 = T.mkSrcPos tReadTable 910019

p91v28 = T.mkSrcPos tReadTable 910028

p91v38 = T.mkSrcPos tReadTable 910038

p92v28 = T.mkSrcPos tReadTable 920028

p92v34 = T.mkSrcPos tReadTable 920034

p92v39 = T.mkSrcPos tReadTable 920039

p92v52 = T.mkSrcPos tReadTable 920052

p97v1 = T.mkSrcPos tReadTable 970001

p98v6 = T.mkSrcPos tReadTable 980006

p98v15 = T.mkSrcPos tReadTable 980015

p98v27 = T.mkSrcPos tReadTable 980027

p98v35 = T.mkSrcPos tReadTable 980035

p98v41 = T.mkSrcPos tReadTable 980041

p98v50 = T.mkSrcPos tReadTable 980050

p98v56 = T.mkSrcPos tReadTable 980056

p99v6 = T.mkSrcPos tReadTable 990006

p99v15 = T.mkSrcPos tReadTable 990015

p99v23 = T.mkSrcPos tReadTable 990023

p99v38 = T.mkSrcPos tReadTable 990038

p99v44 = T.mkSrcPos tReadTable 990044

p103v1 = T.mkSrcPos tReadTable 1030001

p104v5 = T.mkSrcPos tReadTable 1040005

p104v13 = T.mkSrcPos tReadTable 1040013

p104v20 = T.mkSrcPos tReadTable 1040020

p104v29 = T.mkSrcPos tReadTable 1040029
