module TMakeDomains
  (gmdFreeTVarsIn,gmdMakeEdges,gmdTypeDependancy,gmdIsRecursiveType) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TDependancy 
import TList  (gnub)

gmdFreeTVarsIn :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun TypeDef (T.List Naam))

gmdFreeTVarsIn pmdFreeTVarsIn p =
  T.fun1 amdFreeTVarsIn pmdFreeTVarsIn p hmdFreeTVarsIn
  where
  
  hmdFreeTVarsIn (T.R (T.Tuple3 ftn ftvl fcal) _) p =
    T.ap1 p19v6 p (gutSetToList p19v6 p)
      (T.ap2 p20v9 p (gutSetSubtraction p20v9 p)
        (T.ap1 p21v12 p (gutSetFromList p21v12 p) (gallVars p21v26 p))
        (T.ap1 p22v12 p (gutSetFromList p22v12 p)
          (T.ap2 p22v31 p (p22v31 !++ p) ftvl
            (T.fromExpList p22v34 p
              [T.fromLitString p22v35 p "int",T.fromLitString p22v42 p "bool"
                ,T.fromLitString p22v50 p "char"]))))
    where
    
    gallVars pallVars p = T.constUse pallVars p sallVars
    
    sallVars =
      T.constDef p a24v9allVars
        (\ p ->
          T.ap1 p24v19 p (gconcat p24v19 p)
            (T.ap2 p24v27 p (gmap p24v27 p) (gf p24v31 p) fcal))
    
    gf pf p =
      T.fun1 a25v9f pf p hf
      where
      
      hf (T.R (T.Tuple2 fn ftel) _) p =
        T.ap1 p25v22 p (gconcat p25v22 p)
          (T.ap2 p25v30 p (gmap p25v30 p) (gallTVs p25v34 p) ftel)
      hf _ p = T.fatal p
      
    
    gallTVs pallTVs p =
      T.fun1 a26v9allTVs pallTVs p hallTVs
      where
      
      hallTVs (T.R (TDefVar fn) _) p = T.fromExpList p26v30 p [fn]
      hallTVs (T.R (TDefCons fn ftel) _) p =
        T.con2 p27v36 p T.Cons T.aCons fn
          (T.ap1 p27v37 p (gconcat p27v37 p)
            (T.ap2 p27v45 p (gmap p27v45 p) (gallTVs p27v49 p) ftel))
      hallTVs _ p = T.fatal p
      
    
  hmdFreeTVarsIn _ p = T.fatal p
  

gmdMakeEdges ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun (T.List TypeDef) (T.List (T.Tuple2 Naam Naam)))

gmdMakeEdges pmdMakeEdges p =
  T.fun1 amdMakeEdges pmdMakeEdges p hmdMakeEdges
  where
  
  hmdMakeEdges ftdl p =
    T.ap1 p36v6 p (gconcat p36v6 p)
      (T.ap2 p36v14 p (gmap p36v14 p) (gmergeFromTo p36v18 p)
        (T.ap2 p36v31 p (gzip p36v31 p) (gfroms p36v35 p) (gtos p36v41 p)))
    where
    
    gk13sel pk13sel p =
      T.fun1 a38v9k13sel pk13sel p hk13sel
      where
      
      hk13sel (T.R (T.Tuple3 fa fb fc) _) p = T.projection p38v28 p fa
      hk13sel _ p = T.fatal p
      
    
    gfroms pfroms p = T.constUse pfroms p sfroms
    
    sfroms =
      T.constDef p a39v9froms
        (\ p -> T.ap2 p39v17 p (gmap p39v17 p) (gk13sel p39v21 p) ftdl)
    
    gtos ptos p = T.constUse ptos p stos
    
    stos =
      T.constDef p a40v9tos
        (\ p -> T.ap2 p40v15 p (gmap p40v15 p) (gmdFreeTVarsIn p40v19 p) ftdl)
    
    gmergeFromTo pmergeFromTo p =
      T.fun1 a41v9mergeFromTo pmergeFromTo p hmergeFromTo
      where
      
      hmergeFromTo (T.R (T.Tuple2 ff ftol) _) p =
        T.ap1 p0v0 p
          (T.ap2 p41v32 p (TPrelude.g_foldr p41v32 p)
            (T.fun2 T.mkLambda p41v32 p
              (\ f_x f_y p ->
                T.ccase p0v0 p
                  (let
                    v0v0v1 ft p =
                      T.ap1 p41v32 p
                        (T.pa1 T.Cons T.cn1 p41v32 p T.aCons
                          (T.con2 p41v33 p T.Tuple2 T.aTuple2 ff ft)) f_y
                    v0v0v1 _ p = T.projection p41v32 p f_y in (v0v0v1)) f_x))
            ftol) (T.fromExpList p0v0 p [])
      hmergeFromTo _ p = T.fatal p
      
    
  

gmdTypeDependancy ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List TypeDef) TypeDependancy)

gmdTypeDependancy pmdTypeDependancy p =
  T.fun1 amdTypeDependancy pmdTypeDependancy p hmdTypeDependancy
  where
  
  hmdTypeDependancy ftdl p =
    T.ap2 p50v6 p (gmap p50v6 p)
      (T.ap2 p50v20 p (p50v20 !. p) (gsingleRec p50v11 p)
        (gutSetToList p50v21 p))
      (T.ap3 p50v35 p (gdeScc p50v35 p) (gins p50v41 p) (gouts p50v45 p)
        (groots p50v50 p))
    where
    
    gedges pedges p = T.constUse pedges p sedges
    
    sedges =
      T.constDef p a52v9edges
        (\ p -> T.ap1 p52v17 p (gmdMakeEdges p52v17 p) ftdl)
    
    gins pins p =
      T.fun1 a53v9ins pins p hins
      where
      
      hins fv p =
        T.ap1 p0v0 p
          (T.ap2 p53v18 p (TPrelude.g_foldr p53v18 p)
            (T.fun2 T.mkLambda p53v18 p
              (\ f_x f_y p ->
                T.ccase p0v0 p
                  (let
                    v0v0v1 (T.R (T.Tuple2 fu fw) _) p =
                      T.ap1 p53v18 p
                        (T.ap2 p53v18 p (TPrelude.g_filter p53v18 p)
                          (T.ap2 p53v41 p (p53v41 !== p) fv fw)
                          (T.pa1 T.Cons T.cn1 p53v18 p T.aCons fu)) f_y
                    v0v0v1 _ p = T.projection p53v18 p f_y in (v0v0v1)) f_x))
            (gedges p53v33 p)) (T.fromExpList p0v0 p [])
      
    
    gouts pouts p =
      T.fun1 a54v9outs pouts p houts
      where
      
      houts fv p =
        T.ap1 p0v0 p
          (T.ap2 p54v18 p (TPrelude.g_foldr p54v18 p)
            (T.fun2 T.mkLambda p54v18 p
              (\ f_x f_y p ->
                T.ccase p0v0 p
                  (let
                    v0v0v1 (T.R (T.Tuple2 fu fw) _) p =
                      T.ap1 p54v18 p
                        (T.ap2 p54v18 p (TPrelude.g_filter p54v18 p)
                          (T.ap2 p54v41 p (p54v41 !== p) fv fu)
                          (T.pa1 T.Cons T.cn1 p54v18 p T.aCons fw)) f_y
                    v0v0v1 _ p = T.projection p54v18 p f_y in (v0v0v1)) f_x))
            (gedges p54v33 p)) (T.fromExpList p0v0 p [])
      
    
    groots proots p = T.constUse proots p sroots
    
    sroots =
      T.constDef p a55v9roots
        (\ p ->
          let
            gf pf p =
              T.fun1 a57v20f pf p hf
              where
              
              hf (T.R (T.Tuple3 fa fb fc) _) p = T.projection p57v34 p fa
              hf _ p = T.fatal p
               in
            (T.ap1 p55v17 p (gnub p55v17 p)
              (T.ap2 p55v22 p (gmap p55v22 p) (gf p55v26 p) ftdl)))
    
    gsingleRec psingleRec p =
      T.fun1 a58v9singleRec psingleRec p hsingleRec
      where
      
      hsingleRec (T.R (T.Cons fa (T.R (T.Cons fb fabs) _)) _) p =
        T.con2 p58v31 p T.Tuple2 T.aTuple2 (T.con0 p58v32 p True aTrue)
          (T.con2 p58v39 p T.Cons T.aCons fa
            (T.con2 p58v41 p T.Cons T.aCons fb fabs))
      hsingleRec (T.R (T.Cons fa (T.R T.List _)) _) p =
        T.con2 p60v14 p T.Tuple2 T.aTuple2
          (T.ap2 p60v18 p (gelem p60v18 p) fa
            (T.ap1 p60v25 p (gmdFreeTVarsIn p60v25 p)
              (T.ap1 p60v40 p (gfindAIn p60v40 p) ftdl)))
          (T.fromExpList p60v55 p [fa])
        where
        
        gfindAIn pfindAIn p =
          T.fun1 a62v17findAIn pfindAIn p hfindAIn
          where
          
          hfindAIn (T.R (T.Cons (T.R (T.Tuple3 ftn ftvl fcal) _) frest) _) p =
            T.cguard p62v50 p (T.ap2 p62v50 p (p62v50 !== p) fa ftn)
              (\ p -> T.con3 p62v62 p T.Tuple3 T.aTuple3 ftn ftvl fcal)
              (\ p ->
                T.cguard p63v49 p (gotherwise p63v49 p)
                  (\ p -> T.ap1 p63v62 p (gfindAIn p63v62 p) frest)
                  (\ p -> T.fatal p))
          hfindAIn _ p = T.fatal p
          
        
      hsingleRec _ p = T.fatal p
      
    
  

gmdIsRecursiveType ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun TypeDependancy (T.Fun Naam Bool))

gmdIsRecursiveType pmdIsRecursiveType p =
  T.fun2 amdIsRecursiveType pmdIsRecursiveType p hmdIsRecursiveType
  where
  
  hmdIsRecursiveType ftypedependancy ftypeName p =
    T.ap1 p73v6 p (gsearch p73v6 p) ftypedependancy
    where
    
    gsearch psearch p =
      T.fun1 a75v9search psearch p hsearch
      where
      
      hsearch (T.R (T.Cons (T.R (T.Tuple2 frf fnames) _) frest) _) p =
        T.cguard p76v24 p (T.ap2 p76v24 p (gelem p76v24 p) ftypeName fnames)
          (\ p -> T.projection p76v40 p frf)
          (\ p ->
            T.cguard p77v14 p (gotherwise p77v14 p)
              (\ p -> T.ap1 p77v40 p (gsearch p77v40 p) frest)
              (\ p -> T.fatal p))
      hsearch _ p = T.fatal p
      
    
  

tMakeDomains = T.mkModule "MakeDomains" "MakeDomains.hs" Prelude.True

amdFreeTVarsIn =
  T.mkVariable tMakeDomains 180001 3 1 "mdFreeTVarsIn" Prelude.False

amdMakeEdges = T.mkVariable tMakeDomains 350001 3 1 "mdMakeEdges" Prelude.False

amdTypeDependancy =
  T.mkVariable tMakeDomains 490001 3 1 "mdTypeDependancy" Prelude.False

amdIsRecursiveType =
  T.mkVariable tMakeDomains 720001 3 2 "mdIsRecursiveType" Prelude.False

a24v9allVars = T.mkVariable tMakeDomains 240009 3 0 "allVars" Prelude.True

a25v9f = T.mkVariable tMakeDomains 250009 3 1 "f" Prelude.True

a26v9allTVs = T.mkVariable tMakeDomains 260009 3 1 "allTVs" Prelude.True

a38v9k13sel = T.mkVariable tMakeDomains 380009 3 1 "k13sel" Prelude.True

a39v9froms = T.mkVariable tMakeDomains 390009 3 0 "froms" Prelude.True

a40v9tos = T.mkVariable tMakeDomains 400009 3 0 "tos" Prelude.True

a41v9mergeFromTo =
  T.mkVariable tMakeDomains 410009 3 1 "mergeFromTo" Prelude.True

a52v9edges = T.mkVariable tMakeDomains 520009 3 0 "edges" Prelude.True

a53v9ins = T.mkVariable tMakeDomains 530009 3 1 "ins" Prelude.True

a54v9outs = T.mkVariable tMakeDomains 540009 3 1 "outs" Prelude.True

a55v9roots = T.mkVariable tMakeDomains 550009 3 0 "roots" Prelude.True

a58v9singleRec = T.mkVariable tMakeDomains 580009 3 1 "singleRec" Prelude.True

a57v20f = T.mkVariable tMakeDomains 570020 3 1 "f" Prelude.True

a62v17findAIn = T.mkVariable tMakeDomains 620017 3 1 "findAIn" Prelude.True

a75v9search = T.mkVariable tMakeDomains 750009 3 1 "search" Prelude.True

p18v1 = T.mkSrcPos tMakeDomains 180001

p24v9 = T.mkSrcPos tMakeDomains 240009

p24v19 = T.mkSrcPos tMakeDomains 240019

p24v27 = T.mkSrcPos tMakeDomains 240027

p24v31 = T.mkSrcPos tMakeDomains 240031

p25v9 = T.mkSrcPos tMakeDomains 250009

p25v22 = T.mkSrcPos tMakeDomains 250022

p25v30 = T.mkSrcPos tMakeDomains 250030

p25v34 = T.mkSrcPos tMakeDomains 250034

p26v9 = T.mkSrcPos tMakeDomains 260009

p26v30 = T.mkSrcPos tMakeDomains 260030

p27v36 = T.mkSrcPos tMakeDomains 270036

p27v37 = T.mkSrcPos tMakeDomains 270037

p27v45 = T.mkSrcPos tMakeDomains 270045

p27v49 = T.mkSrcPos tMakeDomains 270049

p19v6 = T.mkSrcPos tMakeDomains 190006

p20v9 = T.mkSrcPos tMakeDomains 200009

p21v12 = T.mkSrcPos tMakeDomains 210012

p21v26 = T.mkSrcPos tMakeDomains 210026

p22v12 = T.mkSrcPos tMakeDomains 220012

p22v31 = T.mkSrcPos tMakeDomains 220031

p22v34 = T.mkSrcPos tMakeDomains 220034

p22v35 = T.mkSrcPos tMakeDomains 220035

p22v42 = T.mkSrcPos tMakeDomains 220042

p22v50 = T.mkSrcPos tMakeDomains 220050

p35v1 = T.mkSrcPos tMakeDomains 350001

p38v9 = T.mkSrcPos tMakeDomains 380009

p38v28 = T.mkSrcPos tMakeDomains 380028

p39v9 = T.mkSrcPos tMakeDomains 390009

p39v17 = T.mkSrcPos tMakeDomains 390017

p39v21 = T.mkSrcPos tMakeDomains 390021

p40v9 = T.mkSrcPos tMakeDomains 400009

p40v15 = T.mkSrcPos tMakeDomains 400015

p40v19 = T.mkSrcPos tMakeDomains 400019

p41v9 = T.mkSrcPos tMakeDomains 410009

p0v0 = T.mkSrcPos tMakeDomains 0

p41v32 = T.mkSrcPos tMakeDomains 410032

p41v33 = T.mkSrcPos tMakeDomains 410033

p36v6 = T.mkSrcPos tMakeDomains 360006

p36v14 = T.mkSrcPos tMakeDomains 360014

p36v18 = T.mkSrcPos tMakeDomains 360018

p36v31 = T.mkSrcPos tMakeDomains 360031

p36v35 = T.mkSrcPos tMakeDomains 360035

p36v41 = T.mkSrcPos tMakeDomains 360041

p49v1 = T.mkSrcPos tMakeDomains 490001

p52v9 = T.mkSrcPos tMakeDomains 520009

p52v17 = T.mkSrcPos tMakeDomains 520017

p53v9 = T.mkSrcPos tMakeDomains 530009

p53v18 = T.mkSrcPos tMakeDomains 530018

p53v41 = T.mkSrcPos tMakeDomains 530041

p53v33 = T.mkSrcPos tMakeDomains 530033

p54v9 = T.mkSrcPos tMakeDomains 540009

p54v18 = T.mkSrcPos tMakeDomains 540018

p54v41 = T.mkSrcPos tMakeDomains 540041

p54v33 = T.mkSrcPos tMakeDomains 540033

p55v9 = T.mkSrcPos tMakeDomains 550009

p55v17 = T.mkSrcPos tMakeDomains 550017

p55v22 = T.mkSrcPos tMakeDomains 550022

p55v26 = T.mkSrcPos tMakeDomains 550026

p57v20 = T.mkSrcPos tMakeDomains 570020

p57v34 = T.mkSrcPos tMakeDomains 570034

p58v9 = T.mkSrcPos tMakeDomains 580009

p58v31 = T.mkSrcPos tMakeDomains 580031

p58v32 = T.mkSrcPos tMakeDomains 580032

p58v39 = T.mkSrcPos tMakeDomains 580039

p58v41 = T.mkSrcPos tMakeDomains 580041

p62v17 = T.mkSrcPos tMakeDomains 620017

p62v50 = T.mkSrcPos tMakeDomains 620050

p62v62 = T.mkSrcPos tMakeDomains 620062

p63v49 = T.mkSrcPos tMakeDomains 630049

p63v62 = T.mkSrcPos tMakeDomains 630062

p60v14 = T.mkSrcPos tMakeDomains 600014

p60v18 = T.mkSrcPos tMakeDomains 600018

p60v25 = T.mkSrcPos tMakeDomains 600025

p60v40 = T.mkSrcPos tMakeDomains 600040

p60v55 = T.mkSrcPos tMakeDomains 600055

p50v6 = T.mkSrcPos tMakeDomains 500006

p50v20 = T.mkSrcPos tMakeDomains 500020

p50v11 = T.mkSrcPos tMakeDomains 500011

p50v21 = T.mkSrcPos tMakeDomains 500021

p50v35 = T.mkSrcPos tMakeDomains 500035

p50v41 = T.mkSrcPos tMakeDomains 500041

p50v45 = T.mkSrcPos tMakeDomains 500045

p50v50 = T.mkSrcPos tMakeDomains 500050

p72v1 = T.mkSrcPos tMakeDomains 720001

p75v9 = T.mkSrcPos tMakeDomains 750009

p76v24 = T.mkSrcPos tMakeDomains 760024

p76v40 = T.mkSrcPos tMakeDomains 760040

p77v14 = T.mkSrcPos tMakeDomains 770014

p77v40 = T.mkSrcPos tMakeDomains 770040

p73v6 = T.mkSrcPos tMakeDomains 730006
