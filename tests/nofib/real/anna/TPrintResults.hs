module TPrintResults
  (gprLift,gprCross,gprCrossList,gprAllPoints,gprWidth,gprLiftsIn,gprSucc
    ,gprRoute,gprRouteMain,gprRouteMain_cross,gprPrintFunction) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 
import TBaseDefs 
import TUtils 
import TMyUtils 
import TInverse 
import TAbstractMisc 

gprLift :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun PrDomain PrDomain)

gprLift pprLift p =
  T.fun1 aprLift pprLift p hprLift
  where
  
  hprLift fd p =
    T.con2 p18v21 p T.Cons T.aCons (gnewBottom p18v12 p) fd
    where
    
    gdElemLen pdElemLen p = T.constUse pdElemLen p sdElemLen
    
    sdElemLen =
      T.constDef p a20v15dElemLen
        (\ p ->
          T.ap1 p20v26 p (glength p20v26 p)
            (T.ap1 p20v34 p (ghead p20v34 p) fd))
    
    gdBottomElem pdBottomElem p = T.constUse pdBottomElem p sdBottomElem
    
    sdBottomElem =
      T.constDef p a21v15dBottomElem
        (\ p ->
          T.ap2 p21v48 p (p21v48 !- p)
            (T.ap1 p21v29 p (gminimum p21v29 p)
              (T.ap1 p21v38 p (gconcat p21v38 p) fd))
            (T.ap1 p21v51 p (TPreludeBasic.gfromInteger p21v51 p)
                (T.conInteger p21v51 p 1)
              :: T.R Int))
    
    gnewBottom pnewBottom p = T.constUse pnewBottom p snewBottom
    
    snewBottom =
      T.constDef p a22v15newBottom
        (\ p ->
          T.ap2 p22v27 p (gcopy p22v27 p) (gdElemLen p22v32 p)
            (gdBottomElem p22v41 p))
    
  

gprCross ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun PrDomain (T.Fun PrDomain PrDomain))

gprCross pprCross p =
  T.fun2 aprCross pprCross p hprCross
  where
  
  hprCross fd1 fd2 p =
    T.ap1 p0v0 p
      (T.ap2 p29v17 p (TPrelude.g_foldr p29v17 p)
        (T.fun2 T.mkLambda p29v17 p
          (\ f_x f_y p ->
            T.ccase p0v0 p
              (let
                v0v0v1 fe1 p =
                  T.ap1 p29v17 p
                    (T.ap2 p29v17 p (TPrelude.g_foldr p29v17 p)
                      (T.fun2 T.mkLambda p29v17 p
                        (\ f_x f_y p ->
                          T.ccase p0v0 p
                            (let
                              v0v0v1 fe2 p =
                                T.ap1 p29v17 p
                                  (T.pa1 T.Cons T.cn1 p29v17 p T.aCons
                                    (T.ap2 p29v20 p (p29v20 !++ p) fe1 fe2)) f_y
                              v0v0v1 _ p = T.projection p29v17 p f_y in
                              (v0v0v1)) f_x)) fd2) f_y
                v0v0v1 _ p = T.projection p29v17 p f_y in (v0v0v1)) f_x)) fd1)
      (T.fromExpList p0v0 p [])
  

gprCrossList ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun (T.List PrDomain) PrDomain)

gprCrossList pprCrossList p =
  T.fun1 aprCrossList pprCrossList p hprCrossList
  where
  
  hprCrossList (T.R T.List _) p =
    T.fromExpList p36v25 p
      [T.fromExpList p36v26 p
          [T.ap1 p36v27 p (TPreludeBasic.gfromInteger p36v27 p)
              (T.conInteger p36v27 p 0)]]
  hprCrossList (T.R (T.Cons fd (T.R T.List _)) _) p = T.projection p37v25 p fd
  hprCrossList (T.R (T.Cons fa (T.R (T.Cons fb fabs) _)) _) p =
    T.ap2 p38v25 p (gprCross p38v25 p) fa
      (T.ap1 p38v36 p (gprCrossList p38v36 p)
        (T.con2 p38v50 p T.Cons T.aCons fb fabs))
  hprCrossList _ p = T.fatal p
  

gprAllPoints :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain (T.List Char))

gprAllPoints pprAllPoints p =
  T.fun1 aprAllPoints pprAllPoints p hprAllPoints
  where
  
  hprAllPoints fd p =
    T.ap2 p46v10 p (p46v10 !++ p) (T.fromLitString p46v6 p "{")
      (T.ap2 p46v40 p (p46v40 !++ p)
        (T.ap2 p46v13 p (ginterleave p46v13 p) (T.fromLitString p46v24 p " ")
          (T.ap1 p46v30 p
            (T.ap2 p46v31 p (p46v31 !. p) (gh p46v30 p)
              (T.ap2 p46v33 p (p46v33 !. p) (gg p46v32 p) (gf p46v34 p))) fd))
        (T.fromLitString p46v43 p "}"))
    where
    
    gf pf p =
      T.fun1 a49v9f pf p hf
      where
      
      hf (T.R Two _) p =
        T.fromExpList p49v17 p
          [T.fromExpList p49v19 p
              [T.ap1 p49v21 p (TPrelude.gnegate p49v21 p)
                    (T.ap1 p49v22 p (TPreludeBasic.gfromInteger p49v22 p)
                      (T.conInteger p49v22 p 1))
                  :: T.R Int]
            ,T.fromExpList p49v34 p
              [T.ap1 p49v35 p (TPreludeBasic.gfromInteger p49v35 p)
                    (T.conInteger p49v35 p 0)
                  :: T.R Int]]
      hf (T.R (Lift1 fds) _) p =
        T.ap1 p50v24 p (gprLift p50v24 p)
          (T.ap1 p50v32 p (gprCrossList p50v32 p)
            (T.ap2 p50v45 p (gmap p50v45 p) (gf p50v49 p) fds))
      hf (T.R (Lift2 fds) _) p =
        T.ap1 p51v24 p (gprLift p51v24 p)
          (T.ap1 p51v32 p (gprLift p51v32 p)
            (T.ap1 p51v40 p (gprCrossList p51v40 p)
              (T.ap2 p51v53 p (gmap p51v53 p) (gf p51v57 p) fds)))
      hf _ p = T.fatal p
      
    
    gg pg p =
      T.fun1 a54v9g pg p hg
      where
      
      hg fd p =
        T.ap2 p54v15 p (gmap p54v15 p)
          (T.ap1 p54v20 p (gmap p54v20 p)
            (T.ap1 p54v25 p (gmySubtract p54v25 p)
              (T.ap1 p54v37 p (gminimum p54v37 p)
                (T.ap1 p54v46 p (gconcat p54v46 p) fd)))) fd
      
    
    gh ph p =
      T.fun1 a57v9h ph p hh
      where
      
      hh fx p =
        T.ap2 p57v15 p (gmap p57v15 p)
          (T.ap1 p57v20 p (gmap p57v20 p) (gk p57v24 p))
          (T.ap1 p57v28 p (gg p57v28 p) fx)
      
    
    gk :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Char)
    
    gk pk p =
      T.fun1 a61v9k pk p hk
      where
      
      hk fn p =
        T.ap1 p61v15 p (gtoEnum p61v15 p)
          (T.ap2 p61v24 p (p61v24 !+ p) fn
            (T.ap1 p61v25 p (TPreludeBasic.gfromInteger p61v25 p)
              (T.conInteger p61v25 p 48)))
      
    
  

gprWidth :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain Int)

gprWidth pprWidth p =
  T.fun1 aprWidth pprWidth p hprWidth
  where
  
  hprWidth (T.R Two _) p =
    T.ap1 p68v23 p (TPreludeBasic.gfromInteger p68v23 p)
        (T.conInteger p68v23 p 1)
      :: T.R Int
  hprWidth (T.R (Lift1 fds) _) p =
    T.ap1 p69v23 p (gsum p69v23 p)
      (T.ap2 p69v28 p (gmap p69v28 p) (gprWidth p69v32 p) fds)
  hprWidth (T.R (Lift2 fds) _) p =
    T.ap1 p70v23 p (gsum p70v23 p)
      (T.ap2 p70v28 p (gmap p70v28 p) (gprWidth p70v32 p) fds)
  hprWidth _ p = T.fatal p
  

gprLiftsIn :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain Int)

gprLiftsIn pprLiftsIn p =
  T.fun1 aprLiftsIn pprLiftsIn p hprLiftsIn
  where
  
  hprLiftsIn (T.R Two _) p =
    T.ap1 p77v25 p (TPreludeBasic.gfromInteger p77v25 p)
        (T.conInteger p77v25 p 2)
      :: T.R Int
  hprLiftsIn (T.R (Lift1 fds) _) p =
    T.ap2 p78v27 p (p78v27 !+ p)
      (T.ap1 p78v25 p (TPreludeBasic.gfromInteger p78v25 p)
        (T.conInteger p78v25 p 1))
      (T.ap1 p78v29 p (gmaximum p78v29 p)
        (T.ap2 p78v38 p (gmap p78v38 p) (gprLiftsIn p78v42 p) fds))
  hprLiftsIn (T.R (Lift2 fds) _) p =
    T.ap2 p79v27 p (p79v27 !+ p)
      (T.ap1 p79v25 p (TPreludeBasic.gfromInteger p79v25 p)
        (T.conInteger p79v25 p 2))
      (T.ap1 p79v29 p (gmaximum p79v29 p)
        (T.ap2 p79v38 p (gmap p79v38 p) (gprLiftsIn p79v42 p) fds))
  hprLiftsIn _ p = T.fatal p
  

gprSucc :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int (T.Fun Int Int))

gprSucc pprSucc p =
  T.fun2 aprSucc pprSucc p hprSucc
  where
  
  hprSucc fn fc p = T.ap2 p86v16 p (p86v16 !+ p) fn fc
  

gprRoute ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain (T.Fun Route (T.List Char)))

gprRoute pprRoute p =
  T.fun2 aprRoute pprRoute p hprRoute
  where
  
  hprRoute fd fr p =
    let
      gk :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Char)
      gk pk p =
        T.fun1 a95v10k pk p hk
        where
        
        hk fn p =
          T.ap1 p95v16 p (gtoEnum p95v16 p)
            (T.ap2 p95v26 p (p95v26 !+ p) fn
              (T.ap1 p95v28 p (TPreludeBasic.gfromInteger p95v28 p)
                (T.conInteger p95v28 p 48)))
         in
      (T.ap2 p97v10 p (gmap p97v10 p) (gk p97v14 p)
        (T.ap2 p97v17 p (gprRouteMain p97v17 p) fd fr))
  

gprRouteMain ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Domain (T.Fun Route (T.List Int)))

gprRouteMain pprRouteMain p =
  T.fun2 aprRouteMain pprRouteMain p hprRouteMain
  where
  
  hprRouteMain (T.R Two _) (T.R Zero _) p =
    T.fromExpList p105v6 p
      [T.ap1 p105v7 p (TPreludeBasic.gfromInteger p105v7 p)
            (T.conInteger p105v7 p 0)
          :: T.R Int]
  hprRouteMain (T.R Two _) (T.R One _) p =
    T.fromExpList p107v6 p
      [T.ap1 p107v7 p (TPreludeBasic.gfromInteger p107v7 p)
            (T.conInteger p107v7 p 1)
          :: T.R Int]
  hprRouteMain (fd@(T.R (Lift1 fds) _)) (T.R Stop1 _) p =
    T.ap2 p110v6 p (gcopy p110v6 p) (T.ap1 p110v12 p (gprWidth p110v12 p) fd)
      (T.ap1 p110v23 p (TPreludeBasic.gfromInteger p110v23 p)
        (T.conInteger p110v23 p 0))
  hprRouteMain (fd@(T.R (Lift1 fds) _)) (T.R (Up1 frs) _) p =
    T.ap2 p112v6 p (gmap p112v6 p)
      (T.ap1 p112v11 p (gprSucc p112v11 p)
        (T.ap1 p112v18 p (TPreludeBasic.gfromInteger p112v18 p)
          (T.conInteger p112v18 p 1)))
      (T.ap2 p112v22 p (gprRouteMain_cross p112v22 p) fds frs)
  hprRouteMain (fd@(T.R (Lift2 fds) _)) (T.R Stop2 _) p =
    T.ap2 p115v6 p (gcopy p115v6 p) (T.ap1 p115v12 p (gprWidth p115v12 p) fd)
      (T.ap1 p115v23 p (TPreludeBasic.gfromInteger p115v23 p)
        (T.conInteger p115v23 p 0))
  hprRouteMain (fd@(T.R (Lift2 fds) _)) (T.R Up2 _) p =
    T.ap2 p117v6 p (gcopy p117v6 p) (T.ap1 p117v12 p (gprWidth p117v12 p) fd)
      (T.ap1 p117v23 p (TPreludeBasic.gfromInteger p117v23 p)
        (T.conInteger p117v23 p 1))
  hprRouteMain (fd@(T.R (Lift2 fds) _)) (T.R (UpUp2 frs) _) p =
    T.ap2 p119v6 p (gmap p119v6 p)
      (T.ap1 p119v11 p (gprSucc p119v11 p)
        (T.ap1 p119v18 p (TPreludeBasic.gfromInteger p119v18 p)
          (T.conInteger p119v18 p 2)))
      (T.ap2 p119v22 p (gprRouteMain_cross p119v22 p) fds frs)
  hprRouteMain _ _ p = T.fatal p
  

gprRouteMain_cross pprRouteMain_cross p =
  T.fun2 aprRouteMain_cross pprRouteMain_cross p hprRouteMain_cross
  where
  
  hprRouteMain_cross fds frs p =
    T.ap1 p122v6 p (gconcat p122v6 p) (gfixedRoutes p122v13 p)
    where
    
    gunFixedRoutes punFixedRoutes p = T.constUse punFixedRoutes p sunFixedRoutes
    
    sunFixedRoutes =
      T.constDef p a124v9unFixedRoutes
        (\ p ->
          T.ap3 p125v14 p (gmyZipWith2 p125v14 p) (gprRouteMain p125v25 p) fds
            frs)
    
    gcompFactors pcompFactors p = T.constUse pcompFactors p scompFactors
    
    scompFactors =
      T.constDef p a126v9compFactors
        (\ p -> T.ap2 p127v14 p (gmap p127v14 p) (gprLiftsIn p127v18 p) fds)
    
    gcompFactMax pcompFactMax p = T.constUse pcompFactMax p scompFactMax
    
    scompFactMax =
      T.constDef p a128v9compFactMax
        (\ p -> T.ap1 p129v14 p (gmaximum p129v14 p) (gcompFactors p129v22 p))
    
    gcompFactNorm pcompFactNorm p = T.constUse pcompFactNorm p scompFactNorm
    
    scompFactNorm =
      T.constDef p a130v9compFactNorm
        (\ p ->
          T.ap2 p131v14 p (gmap p131v14 p) (gsubCompFactMax p131v18 p)
            (gcompFactors p131v33 p))
    
    gfixedRoutes pfixedRoutes p = T.constUse pfixedRoutes p sfixedRoutes
    
    sfixedRoutes =
      T.constDef p a132v9fixedRoutes
        (\ p ->
          T.ap2 p133v14 p (gmap p133v14 p) (gapplyCompensationFactor p133v18 p)
            (T.ap2 p134v18 p (gmyZip2 p134v18 p) (gcompFactNorm p134v25 p)
              (gunFixedRoutes p134v38 p)))
    
    gapplyCompensationFactor papplyCompensationFactor p =
      T.fun1 a135v9applyCompensationFactor papplyCompensationFactor p
        happlyCompensationFactor
      where
      
      happlyCompensationFactor (T.R (T.Tuple2 fn froote) _) p =
        T.ap2 p136v14 p (gmap p136v14 p)
          (T.ap1 p136v19 p (gprSucc p136v19 p) fn) froote
      happlyCompensationFactor _ p = T.fatal p
      
    
    gsubCompFactMax :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Int Int)
    
    gsubCompFactMax psubCompFactMax p =
      T.fun1 a138v9subCompFactMax psubCompFactMax p hsubCompFactMax
      where
      
      hsubCompFactMax fnn p =
        T.ap2 p139v26 p (p139v26 !- p) (gcompFactMax p139v14 p) fnn
      
    
  

gprPrintFunction ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Bool
          (T.Fun StaticComponent (T.Fun Naam (T.Fun Point (T.List Char)))))

gprPrintFunction pprPrintFunction p =
  T.fun4 aprPrintFunction pprPrintFunction p hprPrintFunction
  where
  
  hprPrintFunction fmi fstatics ffName
    (T.R (T.Tuple2 (ffDomain@(T.R (Func fdss fdt) _)) (T.R (Rep frep) _)) _) p =
    T.cguard p148v29 p
      (T.ap2 p148v29 p (p148v29 !|| p)
        (T.ap1 p148v6 p (gamIsaHOF p148v6 p)
          (T.con2 p148v16 p Func aFunc fdss fdt))
        (T.ap2 p148v42 p (gelem p148v42 p) (T.con0 p148v32 p NoFormat aNoFormat)
          (T.ap1 p148v48 p (gutSCflags p148v48 p) fstatics)))
      (\ p ->
        T.ap2 p149v22 p (p149v22 !++ p)
          (T.fromLitString p149v6 p "\nFunction \"")
          (T.ap2 p149v30 p (p149v30 !++ p) ffName
            (T.ap2 p150v6 p (p150v6 !++ p)
              (T.fromLitString p149v33 p "\" has input domains:\n")
              (T.ap2 p150v29 p (p150v29 !++ p)
                (T.ap1 p150v9 p (glayn p150v9 p)
                  (T.ap2 p150v15 p (gmap p150v15 p) (gshow p150v19 p) fdss))
                (T.ap2 p151v37 p (p151v37 !++ p)
                  (T.fromLitString p151v6 p "   and output domain\n      ")
                  (T.ap2 p152v14 p (p152v14 !++ p)
                    (T.ap1 p152v6 p (gshow p152v6 p) fdt)
                    (T.ap2 p152v39 p (p152v39 !++ p)
                      (T.fromLitString p152v17 p "\n\nwith value:\n\n")
                      (T.ap2 p152v51 p (p152v51 !++ p)
                        (T.ap1 p152v42 p (gshow p152v42 p) frep)
                        (T.fromLitString p152v54 p "\n\n")))))))))
      (\ p ->
        T.cguard p154v6 p (gotherwise p154v6 p)
          (\ p ->
            T.ap2 p155v22 p (p155v22 !++ p)
              (T.fromLitString p155v6 p "\nFunction \"")
              (T.ap2 p155v30 p (p155v30 !++ p) ffName
                (T.ap2 p155v59 p (p155v59 !++ p)
                  (T.fromLitString p155v33 p "\" has input domains:\n")
                  (T.ap2 p156v21 p (p156v21 !++ p) (gnumberedPrInDs p156v6 p)
                    (T.ap2 p157v37 p (p157v37 !++ p)
                      (T.fromLitString p157v6 p "   and output domain\n      ")
                      (T.ap2 p158v22 p (p158v22 !++ p)
                        (gprettyOutDomain p158v6 p)
                        (T.ap2 p159v41 p (p159v41 !++ p)
                          (T.fromLitString p159v6 p
                            "\n\n   Output  |  Lower frontier")
                          (T.ap2 p160v43 p (p160v43 !++ p)
                            (T.fromLitString p160v8 p
                              "\n   --------+----------------\n")
                            (T.ap2 p161v56 p (p161v56 !++ p)
                              (T.ap1 p161v9 p (gconcat p161v9 p)
                                (T.ap2 p161v17 p (gmap p161v17 p) (gf p161v21 p)
                                  (T.ap1 p161v25 p
                                    (T.ap2 p161v32 p (p161v32 !. p)
                                      (greverse p161v25 p)
                                      (T.ap2 p161v37 p (p161v37 !. p)
                                        (gsort p161v33 p)
                                        (gamAllRoutes p161v38 p))) fdt)))
                              (T.fromLitString p161v59 p "\n\n"))))))))))
          (\ p -> T.fatal p))
    where
    
    gpseudoParams ppseudoParams p = T.constUse ppseudoParams p spseudoParams
    
    spseudoParams =
      T.constDef p a163v9pseudoParams
        (\ p ->
          T.ap2 p165v44 p (p165v44 !++ p)
            (T.ap3 p164v13 p (gutSureLookup p164v13 p)
              (T.ap1 p164v27 p (gutSCfreevars p164v27 p) fstatics)
              (T.fromLitString p165v20 p "prPrintFunction") ffName)
            (T.ap1 p165v47 p (gforever p165v47 p)
              (T.fromLitString p165v55 p "")))
    
    gforever pforever p =
      T.fun1 a166v9forever pforever p hforever
      where
      
      hforever fx p =
        T.con2 p166v22 p T.Cons T.aCons fx
          (T.ap1 p166v23 p (gforever p166v23 p) fx)
      
    
    ginputDomains pinputDomains p = T.constUse pinputDomains p sinputDomains
    
    sinputDomains =
      T.constDef p a168v9inputDomains (\ p -> T.projection p168v24 p fdss)
    
    goutputDomain poutputDomain p = T.constUse poutputDomain p soutputDomain
    
    soutputDomain =
      T.constDef p a170v9outputDomain (\ p -> T.projection p170v24 p fdt)
    
    gprettyInDomains pprettyInDomains p =
      T.constUse pprettyInDomains p sprettyInDomains
    
    sprettyInDomains =
      T.constDef p a172v9prettyInDomains
        (\ p ->
          T.ap2 p172v27 p (gmap p172v27 p) (gprAllPoints p172v31 p)
            (ginputDomains p172v43 p))
    
    gprettyOutDomain pprettyOutDomain p =
      T.constUse pprettyOutDomain p sprettyOutDomain
    
    sprettyOutDomain =
      T.constDef p a173v9prettyOutDomain
        (\ p ->
          T.ap1 p173v27 p (gprAllPoints p173v27 p) (goutputDomain p173v39 p))
    
    gnumberedPrInDs pnumberedPrInDs p =
      T.constUse pnumberedPrInDs p snumberedPrInDs
    
    snumberedPrInDs =
      T.constDef p a175v9numberedPrInDs
        (\ p ->
          T.ap1 p175v26 p (glayn p175v26 p)
            (T.ap2 p175v32 p (gmap p175v32 p) (gff p175v36 p)
              (T.ap2 p175v40 p (gzip p175v40 p) (gpseudoParams p175v44 p)
                (gprettyInDomains p175v57 p))))
    
    gff pff p =
      T.fun1 a176v9ff pff p hff
      where
      
      hff (T.R (T.Tuple2 (T.R T.List _) fpid) _) p = T.projection p176v26 p fpid
      hff (T.R (T.Tuple2 fname fpid) _) p =
        T.ap2 p177v30 p (p177v30 !++ p) fpid
          (T.ap2 p177v45 p (p177v45 !++ p)
            (T.fromLitString p177v33 p " (free \"")
            (T.ap2 p177v53 p (p177v53 !++ p) fname
              (T.fromLitString p177v56 p "\")")))
      hff _ p = T.fatal p
      
    
    gf pf p =
      T.fun1 a179v9f pf p hf
      where
      
      hf fop p =
        let
          gipl pipl p = T.constUse pipl p sipl
          sipl =
            T.constDef p a179v21ipl
              (\ p ->
                T.ap4 p179v27 p (ginMinInverse p179v27 p) fmi ffDomain
                  (T.con1 p179v52 p Rep aRep frep) fop) in
          (T.ap2 p180v55 p (p180v55 !++ p)
            (T.ap2 p180v21 p (gcopy p180v21 p)
              (T.ap2 p180v29 p (p180v29 !- p)
                (T.ap1 p180v27 p (TPreludeBasic.gfromInteger p180v27 p)
                  (T.conInteger p180v27 p 8))
                (T.ap1 p180v31 p (glength p180v31 p) (goutColText p180v38 p)))
              (T.conChar p180v50 p ' '))
            (T.ap2 p180v69 p (p180v69 !++ p) (goutColText p180v58 p)
              (T.ap2 p181v32 p (p181v32 !++ p)
                (T.fromLitString p181v23 p "   |  ")
                (T.ap2 p181v68 p (p181v68 !++ p)
                  (T.ap2 p181v36 p (ginterleave p181v36 p)
                    (T.fromLitString p181v47 p " and ")
                    (T.ap2 p181v56 p (gmap p181v56 p) (gg p181v60 p)
                      (gipl p181v62 p))) (T.fromLitString p181v71 p "\n")))))
        where
        
        goutColText poutColText p = T.constUse poutColText p soutColText
        
        soutColText =
          T.constDef p a183v26outColText
            (\ p -> T.ap2 p183v39 p (gprRoute p183v39 p) fdt fop)
        
      
    
    gg pg p =
      T.fun1 a184v9g pg p hg
      where
      
      hg (T.R (MkFrel frs) _) p =
        T.ap2 p184v25 p (ginterleave p184v25 p) (T.fromLitString p184v36 p " ")
          (T.ap3 p184v41 p (gmyZipWith2 p184v41 p) (gprRoute p184v52 p) fdss
            frs)
      hg _ p = T.fatal p
      
    
  hprPrintFunction fmi fstatics ffName (T.R (T.Tuple2 fds frs) _) p =
    T.cguard p189v6 p (T.ap1 p189v6 p (gamContainsFunctionSpace p189v6 p) fds)
      (\ p ->
        T.ap2 p190v22 p (p190v22 !++ p)
          (T.fromLitString p190v6 p "\nFunction \"")
          (T.ap2 p190v30 p (p190v30 !++ p) ffName
            (T.ap2 p192v7 p (p192v7 !++ p)
              (T.fromLitString p191v6 p
                "\" is a higher-order constant (yuck) in domain\n\n")
              (T.ap2 p192v18 p (p192v18 !++ p)
                (T.ap1 p192v10 p (gshow p192v10 p) fds)
                (T.ap2 p193v25 p (p193v25 !++ p)
                  (T.fromLitString p193v6 p "\n\nof value\n\n")
                  (T.ap2 p193v36 p (p193v36 !++ p)
                    (T.ap1 p193v28 p (gshow p193v28 p) frs)
                    (T.fromLitString p193v39 p "\n\n")))))))
      (\ p ->
        T.cguard p195v6 p (gotherwise p195v6 p)
          (\ p ->
            T.ap2 p196v22 p (p196v22 !++ p)
              (T.fromLitString p196v6 p "\nFunction \"")
              (T.ap2 p196v30 p (p196v30 !++ p) ffName
                (T.ap2 p196v59 p (p196v59 !++ p)
                  (T.fromLitString p196v33 p "\" is a constant point ")
                  (T.ap2 p197v20 p (p197v20 !++ p)
                    (T.ap2 p197v6 p (gprRoute p197v6 p) fds frs)
                    (T.ap2 p197v43 p (p197v43 !++ p)
                      (T.fromLitString p197v23 p " in domain \n    ")
                      (T.ap2 p198v21 p (p198v21 !++ p)
                        (T.ap1 p198v6 p (gprAllPoints p198v6 p) fds)
                        (T.fromLitString p198v24 p "\n\n")))))))
          (\ p -> T.fatal p))
  hprPrintFunction _ _ _ _ p = T.fatal p
  

tPrintResults = T.mkModule "PrintResults" "PrintResults.hs" Prelude.True

aprLift = T.mkVariable tPrintResults 180001 3 1 "prLift" Prelude.False

aprCross = T.mkVariable tPrintResults 290001 3 2 "prCross" Prelude.False

aprCrossList = T.mkVariable tPrintResults 360001 3 1 "prCrossList" Prelude.False

aprAllPoints = T.mkVariable tPrintResults 450001 3 1 "prAllPoints" Prelude.False

aprWidth = T.mkVariable tPrintResults 680001 3 1 "prWidth" Prelude.False

aprLiftsIn = T.mkVariable tPrintResults 770001 3 1 "prLiftsIn" Prelude.False

aprSucc = T.mkVariable tPrintResults 860001 3 2 "prSucc" Prelude.False

aprRoute = T.mkVariable tPrintResults 930001 3 2 "prRoute" Prelude.False

aprRouteMain =
  T.mkVariable tPrintResults 1040001 3 2 "prRouteMain" Prelude.False

aprRouteMain_cross =
  T.mkVariable tPrintResults 1210001 3 2 "prRouteMain_cross" Prelude.False

aprPrintFunction =
  T.mkVariable tPrintResults 1470001 3 4 "prPrintFunction" Prelude.False

a20v15dElemLen = T.mkVariable tPrintResults 200015 3 0 "dElemLen" Prelude.True

a21v15dBottomElem =
  T.mkVariable tPrintResults 210015 3 0 "dBottomElem" Prelude.True

a22v15newBottom = T.mkVariable tPrintResults 220015 3 0 "newBottom" Prelude.True

a49v9f = T.mkVariable tPrintResults 490009 3 1 "f" Prelude.True

a54v9g = T.mkVariable tPrintResults 540009 3 1 "g" Prelude.True

a57v9h = T.mkVariable tPrintResults 570009 3 1 "h" Prelude.True

a61v9k = T.mkVariable tPrintResults 610009 3 1 "k" Prelude.True

a95v10k = T.mkVariable tPrintResults 950010 3 1 "k" Prelude.True

a124v9unFixedRoutes =
  T.mkVariable tPrintResults 1240009 3 0 "unFixedRoutes" Prelude.True

a126v9compFactors =
  T.mkVariable tPrintResults 1260009 3 0 "compFactors" Prelude.True

a128v9compFactMax =
  T.mkVariable tPrintResults 1280009 3 0 "compFactMax" Prelude.True

a130v9compFactNorm =
  T.mkVariable tPrintResults 1300009 3 0 "compFactNorm" Prelude.True

a132v9fixedRoutes =
  T.mkVariable tPrintResults 1320009 3 0 "fixedRoutes" Prelude.True

a135v9applyCompensationFactor =
  T.mkVariable tPrintResults 1350009 3 1 "applyCompensationFactor" Prelude.True

a138v9subCompFactMax =
  T.mkVariable tPrintResults 1380009 3 1 "subCompFactMax" Prelude.True

a163v9pseudoParams =
  T.mkVariable tPrintResults 1630009 3 0 "pseudoParams" Prelude.True

a166v9forever = T.mkVariable tPrintResults 1660009 3 1 "forever" Prelude.True

a168v9inputDomains =
  T.mkVariable tPrintResults 1680009 3 0 "inputDomains" Prelude.True

a170v9outputDomain =
  T.mkVariable tPrintResults 1700009 3 0 "outputDomain" Prelude.True

a172v9prettyInDomains =
  T.mkVariable tPrintResults 1720009 3 0 "prettyInDomains" Prelude.True

a173v9prettyOutDomain =
  T.mkVariable tPrintResults 1730009 3 0 "prettyOutDomain" Prelude.True

a175v9numberedPrInDs =
  T.mkVariable tPrintResults 1750009 3 0 "numberedPrInDs" Prelude.True

a176v9ff = T.mkVariable tPrintResults 1760009 3 1 "ff" Prelude.True

a179v9f = T.mkVariable tPrintResults 1790009 3 1 "f" Prelude.True

a184v9g = T.mkVariable tPrintResults 1840009 3 1 "g" Prelude.True

a183v26outColText =
  T.mkVariable tPrintResults 1830026 3 0 "outColText" Prelude.True

a179v21ipl = T.mkVariable tPrintResults 1790021 3 0 "ipl" Prelude.True

p18v1 = T.mkSrcPos tPrintResults 180001

p20v15 = T.mkSrcPos tPrintResults 200015

p20v26 = T.mkSrcPos tPrintResults 200026

p20v34 = T.mkSrcPos tPrintResults 200034

p21v15 = T.mkSrcPos tPrintResults 210015

p21v48 = T.mkSrcPos tPrintResults 210048

p21v29 = T.mkSrcPos tPrintResults 210029

p21v38 = T.mkSrcPos tPrintResults 210038

p21v51 = T.mkSrcPos tPrintResults 210051

p22v15 = T.mkSrcPos tPrintResults 220015

p22v27 = T.mkSrcPos tPrintResults 220027

p22v32 = T.mkSrcPos tPrintResults 220032

p22v41 = T.mkSrcPos tPrintResults 220041

p18v21 = T.mkSrcPos tPrintResults 180021

p18v12 = T.mkSrcPos tPrintResults 180012

p29v1 = T.mkSrcPos tPrintResults 290001

p0v0 = T.mkSrcPos tPrintResults 0

p29v17 = T.mkSrcPos tPrintResults 290017

p29v20 = T.mkSrcPos tPrintResults 290020

p36v1 = T.mkSrcPos tPrintResults 360001

p36v25 = T.mkSrcPos tPrintResults 360025

p36v26 = T.mkSrcPos tPrintResults 360026

p36v27 = T.mkSrcPos tPrintResults 360027

p37v25 = T.mkSrcPos tPrintResults 370025

p38v25 = T.mkSrcPos tPrintResults 380025

p38v36 = T.mkSrcPos tPrintResults 380036

p38v50 = T.mkSrcPos tPrintResults 380050

p45v1 = T.mkSrcPos tPrintResults 450001

p49v9 = T.mkSrcPos tPrintResults 490009

p49v17 = T.mkSrcPos tPrintResults 490017

p49v19 = T.mkSrcPos tPrintResults 490019

p49v21 = T.mkSrcPos tPrintResults 490021

p49v22 = T.mkSrcPos tPrintResults 490022

p49v34 = T.mkSrcPos tPrintResults 490034

p49v35 = T.mkSrcPos tPrintResults 490035

p50v24 = T.mkSrcPos tPrintResults 500024

p50v32 = T.mkSrcPos tPrintResults 500032

p50v45 = T.mkSrcPos tPrintResults 500045

p50v49 = T.mkSrcPos tPrintResults 500049

p51v24 = T.mkSrcPos tPrintResults 510024

p51v32 = T.mkSrcPos tPrintResults 510032

p51v40 = T.mkSrcPos tPrintResults 510040

p51v53 = T.mkSrcPos tPrintResults 510053

p51v57 = T.mkSrcPos tPrintResults 510057

p54v9 = T.mkSrcPos tPrintResults 540009

p54v15 = T.mkSrcPos tPrintResults 540015

p54v20 = T.mkSrcPos tPrintResults 540020

p54v25 = T.mkSrcPos tPrintResults 540025

p54v37 = T.mkSrcPos tPrintResults 540037

p54v46 = T.mkSrcPos tPrintResults 540046

p57v9 = T.mkSrcPos tPrintResults 570009

p57v15 = T.mkSrcPos tPrintResults 570015

p57v20 = T.mkSrcPos tPrintResults 570020

p57v24 = T.mkSrcPos tPrintResults 570024

p57v28 = T.mkSrcPos tPrintResults 570028

p61v9 = T.mkSrcPos tPrintResults 610009

p61v15 = T.mkSrcPos tPrintResults 610015

p61v24 = T.mkSrcPos tPrintResults 610024

p61v25 = T.mkSrcPos tPrintResults 610025

p46v10 = T.mkSrcPos tPrintResults 460010

p46v6 = T.mkSrcPos tPrintResults 460006

p46v40 = T.mkSrcPos tPrintResults 460040

p46v13 = T.mkSrcPos tPrintResults 460013

p46v24 = T.mkSrcPos tPrintResults 460024

p46v30 = T.mkSrcPos tPrintResults 460030

p46v31 = T.mkSrcPos tPrintResults 460031

p46v33 = T.mkSrcPos tPrintResults 460033

p46v32 = T.mkSrcPos tPrintResults 460032

p46v34 = T.mkSrcPos tPrintResults 460034

p46v43 = T.mkSrcPos tPrintResults 460043

p68v1 = T.mkSrcPos tPrintResults 680001

p68v23 = T.mkSrcPos tPrintResults 680023

p69v23 = T.mkSrcPos tPrintResults 690023

p69v28 = T.mkSrcPos tPrintResults 690028

p69v32 = T.mkSrcPos tPrintResults 690032

p70v23 = T.mkSrcPos tPrintResults 700023

p70v28 = T.mkSrcPos tPrintResults 700028

p70v32 = T.mkSrcPos tPrintResults 700032

p77v1 = T.mkSrcPos tPrintResults 770001

p77v25 = T.mkSrcPos tPrintResults 770025

p78v27 = T.mkSrcPos tPrintResults 780027

p78v25 = T.mkSrcPos tPrintResults 780025

p78v29 = T.mkSrcPos tPrintResults 780029

p78v38 = T.mkSrcPos tPrintResults 780038

p78v42 = T.mkSrcPos tPrintResults 780042

p79v27 = T.mkSrcPos tPrintResults 790027

p79v25 = T.mkSrcPos tPrintResults 790025

p79v29 = T.mkSrcPos tPrintResults 790029

p79v38 = T.mkSrcPos tPrintResults 790038

p79v42 = T.mkSrcPos tPrintResults 790042

p86v1 = T.mkSrcPos tPrintResults 860001

p86v16 = T.mkSrcPos tPrintResults 860016

p93v1 = T.mkSrcPos tPrintResults 930001

p95v10 = T.mkSrcPos tPrintResults 950010

p95v16 = T.mkSrcPos tPrintResults 950016

p95v26 = T.mkSrcPos tPrintResults 950026

p95v28 = T.mkSrcPos tPrintResults 950028

p97v10 = T.mkSrcPos tPrintResults 970010

p97v14 = T.mkSrcPos tPrintResults 970014

p97v17 = T.mkSrcPos tPrintResults 970017

p104v1 = T.mkSrcPos tPrintResults 1040001

p105v6 = T.mkSrcPos tPrintResults 1050006

p105v7 = T.mkSrcPos tPrintResults 1050007

p107v6 = T.mkSrcPos tPrintResults 1070006

p107v7 = T.mkSrcPos tPrintResults 1070007

p110v6 = T.mkSrcPos tPrintResults 1100006

p110v12 = T.mkSrcPos tPrintResults 1100012

p110v23 = T.mkSrcPos tPrintResults 1100023

p112v6 = T.mkSrcPos tPrintResults 1120006

p112v11 = T.mkSrcPos tPrintResults 1120011

p112v18 = T.mkSrcPos tPrintResults 1120018

p112v22 = T.mkSrcPos tPrintResults 1120022

p115v6 = T.mkSrcPos tPrintResults 1150006

p115v12 = T.mkSrcPos tPrintResults 1150012

p115v23 = T.mkSrcPos tPrintResults 1150023

p117v6 = T.mkSrcPos tPrintResults 1170006

p117v12 = T.mkSrcPos tPrintResults 1170012

p117v23 = T.mkSrcPos tPrintResults 1170023

p119v6 = T.mkSrcPos tPrintResults 1190006

p119v11 = T.mkSrcPos tPrintResults 1190011

p119v18 = T.mkSrcPos tPrintResults 1190018

p119v22 = T.mkSrcPos tPrintResults 1190022

p121v1 = T.mkSrcPos tPrintResults 1210001

p124v9 = T.mkSrcPos tPrintResults 1240009

p125v14 = T.mkSrcPos tPrintResults 1250014

p125v25 = T.mkSrcPos tPrintResults 1250025

p126v9 = T.mkSrcPos tPrintResults 1260009

p127v14 = T.mkSrcPos tPrintResults 1270014

p127v18 = T.mkSrcPos tPrintResults 1270018

p128v9 = T.mkSrcPos tPrintResults 1280009

p129v14 = T.mkSrcPos tPrintResults 1290014

p129v22 = T.mkSrcPos tPrintResults 1290022

p130v9 = T.mkSrcPos tPrintResults 1300009

p131v14 = T.mkSrcPos tPrintResults 1310014

p131v18 = T.mkSrcPos tPrintResults 1310018

p131v33 = T.mkSrcPos tPrintResults 1310033

p132v9 = T.mkSrcPos tPrintResults 1320009

p133v14 = T.mkSrcPos tPrintResults 1330014

p133v18 = T.mkSrcPos tPrintResults 1330018

p134v18 = T.mkSrcPos tPrintResults 1340018

p134v25 = T.mkSrcPos tPrintResults 1340025

p134v38 = T.mkSrcPos tPrintResults 1340038

p135v9 = T.mkSrcPos tPrintResults 1350009

p136v14 = T.mkSrcPos tPrintResults 1360014

p136v19 = T.mkSrcPos tPrintResults 1360019

p138v9 = T.mkSrcPos tPrintResults 1380009

p139v26 = T.mkSrcPos tPrintResults 1390026

p139v14 = T.mkSrcPos tPrintResults 1390014

p122v6 = T.mkSrcPos tPrintResults 1220006

p122v13 = T.mkSrcPos tPrintResults 1220013

p147v1 = T.mkSrcPos tPrintResults 1470001

p163v9 = T.mkSrcPos tPrintResults 1630009

p165v44 = T.mkSrcPos tPrintResults 1650044

p164v13 = T.mkSrcPos tPrintResults 1640013

p164v27 = T.mkSrcPos tPrintResults 1640027

p165v20 = T.mkSrcPos tPrintResults 1650020

p165v47 = T.mkSrcPos tPrintResults 1650047

p165v55 = T.mkSrcPos tPrintResults 1650055

p166v9 = T.mkSrcPos tPrintResults 1660009

p166v22 = T.mkSrcPos tPrintResults 1660022

p166v23 = T.mkSrcPos tPrintResults 1660023

p168v9 = T.mkSrcPos tPrintResults 1680009

p168v24 = T.mkSrcPos tPrintResults 1680024

p170v9 = T.mkSrcPos tPrintResults 1700009

p170v24 = T.mkSrcPos tPrintResults 1700024

p172v9 = T.mkSrcPos tPrintResults 1720009

p172v27 = T.mkSrcPos tPrintResults 1720027

p172v31 = T.mkSrcPos tPrintResults 1720031

p172v43 = T.mkSrcPos tPrintResults 1720043

p173v9 = T.mkSrcPos tPrintResults 1730009

p173v27 = T.mkSrcPos tPrintResults 1730027

p173v39 = T.mkSrcPos tPrintResults 1730039

p175v9 = T.mkSrcPos tPrintResults 1750009

p175v26 = T.mkSrcPos tPrintResults 1750026

p175v32 = T.mkSrcPos tPrintResults 1750032

p175v36 = T.mkSrcPos tPrintResults 1750036

p175v40 = T.mkSrcPos tPrintResults 1750040

p175v44 = T.mkSrcPos tPrintResults 1750044

p175v57 = T.mkSrcPos tPrintResults 1750057

p176v9 = T.mkSrcPos tPrintResults 1760009

p176v26 = T.mkSrcPos tPrintResults 1760026

p177v30 = T.mkSrcPos tPrintResults 1770030

p177v45 = T.mkSrcPos tPrintResults 1770045

p177v33 = T.mkSrcPos tPrintResults 1770033

p177v53 = T.mkSrcPos tPrintResults 1770053

p177v56 = T.mkSrcPos tPrintResults 1770056

p179v9 = T.mkSrcPos tPrintResults 1790009

p183v26 = T.mkSrcPos tPrintResults 1830026

p183v39 = T.mkSrcPos tPrintResults 1830039

p179v21 = T.mkSrcPos tPrintResults 1790021

p179v27 = T.mkSrcPos tPrintResults 1790027

p179v52 = T.mkSrcPos tPrintResults 1790052

p180v55 = T.mkSrcPos tPrintResults 1800055

p180v21 = T.mkSrcPos tPrintResults 1800021

p180v29 = T.mkSrcPos tPrintResults 1800029

p180v27 = T.mkSrcPos tPrintResults 1800027

p180v31 = T.mkSrcPos tPrintResults 1800031

p180v38 = T.mkSrcPos tPrintResults 1800038

p180v50 = T.mkSrcPos tPrintResults 1800050

p180v69 = T.mkSrcPos tPrintResults 1800069

p180v58 = T.mkSrcPos tPrintResults 1800058

p181v32 = T.mkSrcPos tPrintResults 1810032

p181v23 = T.mkSrcPos tPrintResults 1810023

p181v68 = T.mkSrcPos tPrintResults 1810068

p181v36 = T.mkSrcPos tPrintResults 1810036

p181v47 = T.mkSrcPos tPrintResults 1810047

p181v56 = T.mkSrcPos tPrintResults 1810056

p181v60 = T.mkSrcPos tPrintResults 1810060

p181v62 = T.mkSrcPos tPrintResults 1810062

p181v71 = T.mkSrcPos tPrintResults 1810071

p184v9 = T.mkSrcPos tPrintResults 1840009

p184v25 = T.mkSrcPos tPrintResults 1840025

p184v36 = T.mkSrcPos tPrintResults 1840036

p184v41 = T.mkSrcPos tPrintResults 1840041

p184v52 = T.mkSrcPos tPrintResults 1840052

p148v29 = T.mkSrcPos tPrintResults 1480029

p148v6 = T.mkSrcPos tPrintResults 1480006

p148v16 = T.mkSrcPos tPrintResults 1480016

p148v42 = T.mkSrcPos tPrintResults 1480042

p148v32 = T.mkSrcPos tPrintResults 1480032

p148v48 = T.mkSrcPos tPrintResults 1480048

p149v22 = T.mkSrcPos tPrintResults 1490022

p149v6 = T.mkSrcPos tPrintResults 1490006

p149v30 = T.mkSrcPos tPrintResults 1490030

p150v6 = T.mkSrcPos tPrintResults 1500006

p149v33 = T.mkSrcPos tPrintResults 1490033

p150v29 = T.mkSrcPos tPrintResults 1500029

p150v9 = T.mkSrcPos tPrintResults 1500009

p150v15 = T.mkSrcPos tPrintResults 1500015

p150v19 = T.mkSrcPos tPrintResults 1500019

p151v37 = T.mkSrcPos tPrintResults 1510037

p151v6 = T.mkSrcPos tPrintResults 1510006

p152v14 = T.mkSrcPos tPrintResults 1520014

p152v6 = T.mkSrcPos tPrintResults 1520006

p152v39 = T.mkSrcPos tPrintResults 1520039

p152v17 = T.mkSrcPos tPrintResults 1520017

p152v51 = T.mkSrcPos tPrintResults 1520051

p152v42 = T.mkSrcPos tPrintResults 1520042

p152v54 = T.mkSrcPos tPrintResults 1520054

p154v6 = T.mkSrcPos tPrintResults 1540006

p155v22 = T.mkSrcPos tPrintResults 1550022

p155v6 = T.mkSrcPos tPrintResults 1550006

p155v30 = T.mkSrcPos tPrintResults 1550030

p155v59 = T.mkSrcPos tPrintResults 1550059

p155v33 = T.mkSrcPos tPrintResults 1550033

p156v21 = T.mkSrcPos tPrintResults 1560021

p156v6 = T.mkSrcPos tPrintResults 1560006

p157v37 = T.mkSrcPos tPrintResults 1570037

p157v6 = T.mkSrcPos tPrintResults 1570006

p158v22 = T.mkSrcPos tPrintResults 1580022

p158v6 = T.mkSrcPos tPrintResults 1580006

p159v41 = T.mkSrcPos tPrintResults 1590041

p159v6 = T.mkSrcPos tPrintResults 1590006

p160v43 = T.mkSrcPos tPrintResults 1600043

p160v8 = T.mkSrcPos tPrintResults 1600008

p161v56 = T.mkSrcPos tPrintResults 1610056

p161v9 = T.mkSrcPos tPrintResults 1610009

p161v17 = T.mkSrcPos tPrintResults 1610017

p161v21 = T.mkSrcPos tPrintResults 1610021

p161v25 = T.mkSrcPos tPrintResults 1610025

p161v32 = T.mkSrcPos tPrintResults 1610032

p161v37 = T.mkSrcPos tPrintResults 1610037

p161v33 = T.mkSrcPos tPrintResults 1610033

p161v38 = T.mkSrcPos tPrintResults 1610038

p161v59 = T.mkSrcPos tPrintResults 1610059

p189v6 = T.mkSrcPos tPrintResults 1890006

p190v22 = T.mkSrcPos tPrintResults 1900022

p190v6 = T.mkSrcPos tPrintResults 1900006

p190v30 = T.mkSrcPos tPrintResults 1900030

p192v7 = T.mkSrcPos tPrintResults 1920007

p191v6 = T.mkSrcPos tPrintResults 1910006

p192v18 = T.mkSrcPos tPrintResults 1920018

p192v10 = T.mkSrcPos tPrintResults 1920010

p193v25 = T.mkSrcPos tPrintResults 1930025

p193v6 = T.mkSrcPos tPrintResults 1930006

p193v36 = T.mkSrcPos tPrintResults 1930036

p193v28 = T.mkSrcPos tPrintResults 1930028

p193v39 = T.mkSrcPos tPrintResults 1930039

p195v6 = T.mkSrcPos tPrintResults 1950006

p196v22 = T.mkSrcPos tPrintResults 1960022

p196v6 = T.mkSrcPos tPrintResults 1960006

p196v30 = T.mkSrcPos tPrintResults 1960030

p196v59 = T.mkSrcPos tPrintResults 1960059

p196v33 = T.mkSrcPos tPrintResults 1960033

p197v20 = T.mkSrcPos tPrintResults 1970020

p197v6 = T.mkSrcPos tPrintResults 1970006

p197v43 = T.mkSrcPos tPrintResults 1970043

p197v23 = T.mkSrcPos tPrintResults 1970023

p198v21 = T.mkSrcPos tPrintResults 1980021

p198v6 = T.mkSrcPos tPrintResults 1980006

p198v24 = T.mkSrcPos tPrintResults 1980024
