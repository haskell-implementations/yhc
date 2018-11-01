module TBarakiConc3
  (gbcEApp_d,gbcEApp,gbcEdotF,gbcFdotC,gbcApplyF0,gbcApplyF0_2,gbcEdotFdotC
    ,gbcGetR,gbcGetD,gbcGetT,gbcMakeInstance,gbcClean) where

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
import TDomainExpr 
import TAbsConc3 
import TBarakiMeet 

gbcEApp_d ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun DRRSubst (T.Fun DExpr Domain))

gbcEApp_d pbcEApp_d p =
  T.fun2 abcEApp_d pbcEApp_d p hbcEApp_d
  where
  
  hbcEApp_d frho (T.R DXTwo _) p = T.con0 p29v6 p Two aTwo
  hbcEApp_d frho (T.R (DXLift1 fdxs) _) p =
    T.con1 p31v6 p Lift1 aLift1
      (T.ap2 p31v13 p (gmap p31v13 p) (T.ap1 p31v18 p (gbcEApp_d p31v18 p) frho)
        fdxs)
  hbcEApp_d frho (T.R (DXLift2 fdxs) _) p =
    T.con1 p33v6 p Lift2 aLift2
      (T.ap2 p33v13 p (gmap p33v13 p) (T.ap1 p33v18 p (gbcEApp_d p33v18 p) frho)
        fdxs)
  hbcEApp_d frho (T.R (DXFunc fdss fdt) _) p =
    T.con2 p35v6 p Func aFunc
      (T.ap2 p35v12 p (gmap p35v12 p) (T.ap1 p35v17 p (gbcEApp_d p35v17 p) frho)
        fdss) (T.ap2 p35v37 p (gbcEApp_d p35v37 p) frho fdt)
  hbcEApp_d frho (T.R (DXVar falpha) _) p =
    T.ap1 p37v6 p (gbcGetD p37v6 p)
      (T.ap3 p37v14 p (gutSureLookup p37v14 p) frho
        (T.fromLitString p37v31 p "bcEApp_d") falpha)
  hbcEApp_d _ _ p = T.fatal p
  

gbcEApp ::
  T.RefSrcPos ->
    T.RefExp -> T.R (T.Fun DRRSubst (T.Fun DExpr (T.Fun Route Route)))

gbcEApp pbcEApp p =
  T.fun3 abcEApp pbcEApp p hbcEApp
  where
  
  hbcEApp frho (T.R DXTwo _) (T.R Zero _) p = T.con0 p45v6 p Zero aZero
  hbcEApp frho (T.R DXTwo _) (T.R One _) p = T.con0 p47v6 p One aOne
  hbcEApp frho (T.R (DXLift1 fdxs) _) (T.R Stop1 _) p =
    T.con0 p50v6 p Stop1 aStop1
  hbcEApp frho (T.R (DXLift1 fdxs) _) (T.R (Up1 frs) _) p =
    T.con1 p52v6 p Up1 aUp1
      (T.ap3 p52v11 p (gmyZipWith2 p52v11 p)
        (T.ap1 p52v23 p (gbcEApp p52v23 p) frho) fdxs frs)
  hbcEApp frho (T.R (DXLift2 fdxs) _) (T.R Stop2 _) p =
    T.con0 p55v6 p Stop2 aStop2
  hbcEApp frho (T.R (DXLift2 fdxs) _) (T.R Up2 _) p = T.con0 p57v6 p Up2 aUp2
  hbcEApp frho (T.R (DXLift2 fdxs) _) (T.R (UpUp2 frs) _) p =
    T.con1 p59v6 p UpUp2 aUpUp2
      (T.ap3 p59v13 p (gmyZipWith2 p59v13 p)
        (T.ap1 p59v25 p (gbcEApp p59v25 p) frho) fdxs frs)
  hbcEApp frho (T.R (DXVar falpha) _) (T.R Zero _) p =
    T.ap1 p62v6 p (gbcGetR p62v6 p)
      (T.ap3 p62v14 p (gutSureLookup p62v14 p) frho
        (T.fromLitString p62v31 p "bcEApp(1)") falpha)
  hbcEApp frho (T.R (DXVar falpha) _) (T.R One _) p =
    T.ap1 p64v6 p (gbcGetT p64v6 p)
      (T.ap3 p64v14 p (gutSureLookup p64v14 p) frho
        (T.fromLitString p64v31 p "bcEApp(2)") falpha)
  hbcEApp frho (T.R (DXFunc fdxss fdxt) _) (T.R (Rep frep) _) p =
    let
      grepDomain prepDomain p = T.constUse prepDomain p srepDomain
      srepDomain =
        T.constDef p a67v10repDomain
          (\ p ->
            T.ap1 p67v22 p (gdxApplyDSubst_2 p67v22 p)
              (T.con2 p67v39 p DXFunc aDXFunc fdxss fdxt)) in
      (T.ap5 p69v10 p (gbcEdotFdotC p69v10 p) frho fdxt (grepDomain p69v30 p)
        frep fdxss)
  hbcEApp _ _ _ p = T.fatal p
  

gbcEdotF ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R (T.Fun DRRSubst (T.Fun DExpr (T.Fun Domain (T.Fun Rep Rep))))

gbcEdotF pbcEdotF p =
  T.fun4 abcEdotF pbcEdotF p hbcEdotF
  where
  
  hbcEdotF frho (T.R DXTwo _) (fd@(T.R (Func _ (T.R Two _)) _))
    (ff@(T.R (RepTwo _) _)) p =
    T.projection p91v6 p ff
  hbcEdotF frho (T.R (DXLift1 fdxs) _)
    (fd@(T.R (Func fdss (T.R (Lift1 fdts) _)) _)) (ff@(T.R (Rep1 flf fhfs) _))
    p =
    let
      ghf_domains phf_domains p = T.constUse phf_domains p shf_domains
      shf_domains =
        T.constDef p a99v10hf_domains
          (\ p ->
            T.ap2 p99v23 p (gmap p99v23 p)
              (T.ap1 p99v28 p (gavUncurry p99v28 p) fdss) fdts)
      gnew_hfs pnew_hfs p = T.constUse pnew_hfs p snew_hfs
      snew_hfs =
        T.constDef p a100v10new_hfs
          (\ p ->
            T.ap4 p100v20 p (gmyZipWith3 p100v20 p)
              (T.ap1 p100v32 p (gbcEdotF p100v32 p) frho) fdxs
              (ghf_domains p100v49 p) fhfs) in
      (T.con2 p102v10 p Rep1 aRep1 flf (gnew_hfs p102v18 p))
  hbcEdotF frho (T.R (DXLift2 fdxs) _)
    (fd@(T.R (Func fdss (T.R (Lift2 fdts) _)) _))
    (ff@(T.R (Rep2 flf fmf fhfs) _)) p =
    let
      ghf_domains phf_domains p = T.constUse phf_domains p shf_domains
      shf_domains =
        T.constDef p a110v10hf_domains
          (\ p ->
            T.ap2 p110v23 p (gmap p110v23 p)
              (T.ap1 p110v28 p (gavUncurry p110v28 p) fdss) fdts)
      gnew_hfs pnew_hfs p = T.constUse pnew_hfs p snew_hfs
      snew_hfs =
        T.constDef p a111v10new_hfs
          (\ p ->
            T.ap4 p111v20 p (gmyZipWith3 p111v20 p)
              (T.ap1 p111v32 p (gbcEdotF p111v32 p) frho) fdxs
              (ghf_domains p111v49 p) fhfs) in
      (T.con3 p113v10 p Rep2 aRep2 flf fmf (gnew_hfs p113v21 p))
  hbcEdotF frho (T.R (DXVar falpha) _) (fd@(T.R (Func fds2 (T.R Two _)) _))
    (ff@(T.R (RepTwo ff1f0) _)) p =
    let
      gf_top pf_top p = T.constUse pf_top p sf_top
      sf_top =
        T.constDef p a121v10f_top
          (\ p -> T.ap1 p121v23 p (gavTopR_aux_2 p121v23 p) fds2)
      gf_top_rep pf_top_rep p = T.constUse pf_top_rep p sf_top_rep
      sf_top_rep =
        T.constDef p a122v10f_top_rep
          (\ p -> T.con1 p122v23 p RepTwo aRepTwo (gf_top p122v30 p))
      gevb pevb p =
        T.fun1 a124v10evb pevb p hevb
        where
        
        hevb (T.R Two _) p = T.projection p124v20 p ff
        hevb (T.R (Lift1 fes) _) p =
          T.con2 p125v27 p Rep1 aRep1 ff1f0
            (T.ap2 p125v38 p (gmap p125v38 p) (gevb p125v42 p) fes)
        hevb (T.R (Lift2 fes) _) p =
          T.con3 p126v27 p Rep2 aRep2 ff1f0 ff1f0
            (T.ap2 p126v43 p (gmap p126v43 p) (gevb p126v47 p) fes)
        hevb _ p = T.fatal p
        
      gev pev p =
        T.fun2 a128v10ev pev p hev
        where
        
        hev (T.R Two _) (T.R Zero _) p = T.projection p128v38 p ff
        hev (T.R Two _) (T.R One _) p = gf_top_rep p129v38 p
        hev (T.R (Lift1 fes) _) (T.R Stop1 _) p =
          T.con2 p131v38 p Rep1 aRep1 ff1f0
            (T.ap2 p131v49 p (gmap p131v49 p) (gevb p131v53 p) fes)
        hev (T.R (Lift1 fes) _) (T.R (Up1 frs) _) p =
          T.con2 p132v38 p Rep1 aRep1 (gf_top p132v43 p)
            (T.ap3 p132v50 p (gmyZipWith2 p132v50 p) (gev p132v61 p) fes frs)
        hev (T.R (Lift2 fes) _) (T.R Stop2 _) p =
          T.con3 p134v38 p Rep2 aRep2 ff1f0 ff1f0
            (T.ap2 p134v54 p (gmap p134v54 p) (gevb p134v58 p) fes)
        hev (T.R (Lift2 fes) _) (T.R Up2 _) p =
          T.con3 p135v38 p Rep2 aRep2 (gf_top p135v43 p) ff1f0
            (T.ap2 p135v55 p (gmap p135v55 p) (gevb p135v59 p) fes)
        hev (T.R (Lift2 fes) _) (T.R (UpUp2 frs) _) p =
          T.con3 p136v38 p Rep2 aRep2 (gf_top p136v43 p) (gf_top p136v49 p)
            (T.ap3 p136v56 p (gmyZipWith2 p136v56 p) (gev p136v67 p) fes frs)
        hev _ _ p = T.fatal p
        
      gevLookup pevLookup p = T.constUse pevLookup p sevLookup
      sevLookup =
        T.constDef p a138v10evLookup
          (\ p ->
            T.ap3 p138v21 p (gutSureLookup p138v21 p) frho
              (T.fromLitString p138v38 p "bcEdotF") falpha)
      gevD pevD p = T.constUse pevD p sevD
      sevD =
        T.constDef p a139v10evD
          (\ p -> T.ap1 p139v16 p (gbcGetD p139v16 p) (gevLookup p139v23 p))
      gevR pevR p = T.constUse pevR p sevR
      sevR =
        T.constDef p a140v10evR
          (\ p -> T.ap1 p140v16 p (gbcGetR p140v16 p) (gevLookup p140v23 p)) in
      (T.ap2 p142v10 p (gev p142v10 p) (gevD p142v13 p) (gevR p142v17 p))
  hbcEdotF _ _ _ _ p = T.fatal p
  

gbcFdotC ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun DRRSubst
          (T.Fun (T.List DExpr)
            (T.Fun (T.List Domain) (T.Fun Domain (T.Fun Rep Rep)))))

gbcFdotC pbcFdotC p =
  T.fun5 abcFdotC pbcFdotC p hbcFdotC
  where
  
  hbcFdotC frho fdxs fnewDs (T.R (Func fdss fdt) _) frep p =
    let
      gnew_rep pnew_rep p = T.constUse pnew_rep p snew_rep
      snew_rep =
        T.constDef p a162v10new_rep
          (\ p ->
            T.ap3 p162v20 p (gbcApplyF0 p162v20 p) (geapp_pt p162v30 p) fnewDs
              frep)
      geapp_pt peapp_pt p =
        T.fun1 a163v10eapp_pt peapp_pt p heapp_pt
        where
        
        heapp_pt (T.R (MkFrel ffels) _) p =
          T.con1 p164v14 p MkFrel aMkFrel
            (T.ap3 p164v22 p (gmyZipWith2 p164v22 p)
              (T.ap1 p164v34 p (gbcEApp p164v34 p) frho) fdxs ffels)
        heapp_pt _ p = T.fatal p
         in (gnew_rep p166v10 p)
  hbcFdotC _ _ _ _ _ p = T.fatal p
  

gbcApplyF0 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun FrontierElem FrontierElem)
          (T.Fun (T.List Domain) (T.Fun Rep Rep)))

gbcApplyF0 pbcApplyF0 p =
  T.fun3 abcApplyF0 pbcApplyF0 p hbcApplyF0
  where
  
  hbcApplyF0 ff fdss (T.R (RepTwo ffr) _) p =
    T.con1 p180v6 p RepTwo aRepTwo
      (T.ap3 p180v14 p (gbcApplyF0_2 p180v14 p) ff fdss ffr)
  hbcApplyF0 ff fdss (T.R (Rep1 flf fhfs) _) p =
    let
      gnew_lf pnew_lf p = T.constUse pnew_lf p snew_lf
      snew_lf =
        T.constDef p a183v10new_lf
          (\ p -> T.ap3 p183v19 p (gbcApplyF0_2 p183v19 p) ff fdss flf)
      gnew_hfs pnew_hfs p = T.constUse pnew_hfs p snew_hfs
      snew_hfs =
        T.constDef p a184v10new_hfs
          (\ p ->
            T.ap2 p184v20 p (gmap p184v20 p)
              (T.ap2 p184v25 p (gbcApplyF0 p184v25 p) ff fdss) fhfs) in
      (T.con2 p186v10 p Rep1 aRep1 (gnew_lf p186v15 p) (gnew_hfs p186v22 p))
  hbcApplyF0 ff fdss (T.R (Rep2 flf fmf fhfs) _) p =
    let
      gnew_lf pnew_lf p = T.constUse pnew_lf p snew_lf
      snew_lf =
        T.constDef p a189v10new_lf
          (\ p -> T.ap3 p189v19 p (gbcApplyF0_2 p189v19 p) ff fdss flf)
      gnew_mf pnew_mf p = T.constUse pnew_mf p snew_mf
      snew_mf =
        T.constDef p a190v10new_mf
          (\ p -> T.ap3 p190v19 p (gbcApplyF0_2 p190v19 p) ff fdss fmf)
      gnew_hfs pnew_hfs p = T.constUse pnew_hfs p snew_hfs
      snew_hfs =
        T.constDef p a191v10new_hfs
          (\ p ->
            T.ap2 p191v20 p (gmap p191v20 p)
              (T.ap2 p191v25 p (gbcApplyF0 p191v25 p) ff fdss) fhfs) in
      (T.con3 p193v10 p Rep2 aRep2 (gnew_lf p193v15 p) (gnew_mf p193v22 p)
        (gnew_hfs p193v29 p))
  hbcApplyF0 _ _ _ p = T.fatal p
  

gbcApplyF0_2 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun FrontierElem FrontierElem)
          (T.Fun (T.List Domain) (T.Fun Frontier Frontier)))

gbcApplyF0_2 pbcApplyF0_2 p =
  T.fun3 abcApplyF0_2 pbcApplyF0_2 p hbcApplyF0_2
  where
  
  hbcApplyF0_2 ff fdss (ffr@(T.R (Min1Max0 far ff1 ff0) _)) p =
    let
      gnew_f0 pnew_f0 p = T.constUse pnew_f0 p snew_f0
      snew_f0 =
        T.constDef p a204v10new_f0
          (\ p -> T.ap2 p204v19 p (gmap p204v19 p) ff ff0)
      gnew_f1 pnew_f1 p = T.constUse pnew_f1 p snew_f1
      snew_f1 =
        T.constDef p a205v10new_f1 (\ p -> T.con0 p205v19 p T.List T.aList) in
      (T.con3 p207v10 p Min1Max0 aMin1Max0 far (gnew_f1 p207v22 p)
        (gnew_f0 p207v29 p))
  hbcApplyF0_2 _ _ _ p = T.fatal p
  

gbcEdotFdotC ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun DRRSubst
          (T.Fun DExpr (T.Fun Domain (T.Fun Rep (T.Fun (T.List DExpr) Route)))))

gbcEdotFdotC pbcEdotFdotC p =
  T.fun5 abcEdotFdotC pbcEdotFdotC p hbcEdotFdotC
  where
  
  hbcEdotFdotC frho fg_e (ffDomain@(T.R (Func ffds ffdt) _)) ff ff_cs p =
    let
      gf_dot_c pf_dot_c p = T.constUse pf_dot_c p sf_dot_c
      sf_dot_c =
        T.constDef p a227v10f_dot_c
          (\ p ->
            T.ap5 p227v28 p (gbcFdotC p227v28 p) frho ff_cs (gnewDs p227v45 p)
              ffDomain ff)
      gnewDs pnewDs p = T.constUse pnewDs p snewDs
      snewDs =
        T.constDef p a228v10newDs
          (\ p ->
            T.ap2 p228v28 p (gmap p228v28 p)
              (T.ap1 p228v33 p (gbcEApp_d p228v33 p) frho) ff_cs)
      gfd_dot_c pfd_dot_c p = T.constUse pfd_dot_c p sfd_dot_c
      sfd_dot_c =
        T.constDef p a229v10fd_dot_c
          (\ p -> T.con2 p229v28 p Func aFunc (gnewDs p229v33 p) ffdt)
      ge_dot_f_dot_c pe_dot_f_dot_c p =
        T.constUse pe_dot_f_dot_c p se_dot_f_dot_c
      se_dot_f_dot_c =
        T.constDef p a230v10e_dot_f_dot_c
          (\ p ->
            T.ap4 p230v28 p (gbcEdotF p230v28 p) frho fg_e (gfd_dot_c p230v44 p)
              (gf_dot_c p230v53 p)) in
      (T.con1 p232v10 p Rep aRep (ge_dot_f_dot_c p232v14 p))
  hbcEdotFdotC _ _ _ _ _ p = T.fatal p
  

gbcGetR pbcGetR p =
  T.fun1 abcGetR pbcGetR p hbcGetR
  where
  
  hbcGetR (T.R (T.Tuple3 fd fr ft) _) p = T.projection p237v20 p fr
  hbcGetR _ p = T.fatal p
  

gbcGetD pbcGetD p =
  T.fun1 abcGetD pbcGetD p hbcGetD
  where
  
  hbcGetD (T.R (T.Tuple3 fd fr ft) _) p = T.projection p238v20 p fd
  hbcGetD _ p = T.fatal p
  

gbcGetT pbcGetT p =
  T.fun1 abcGetT pbcGetT p hbcGetT
  where
  
  hbcGetT (T.R (T.Tuple3 fd fr ft) _) p = T.projection p239v20 p ft
  hbcGetT _ p = T.fatal p
  

gbcMakeInstance ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun Bool
          (T.Fun Int
            (T.Fun ACMode (T.Fun DExpr (T.Fun DSubst (T.Fun Route Route))))))

gbcMakeInstance pbcMakeInstance p =
  T.fun6 abcMakeInstance pbcMakeInstance p hbcMakeInstance
  where
  
  hbcMakeInstance fuse_baraki fthreshold fs_or_l
    (fsimplest_dirty@(T.R
        (DXFunc fargs_f_functors_dirty fresult_g_functor_dirty) _)) frho_d
    (ff@(T.R (Rep ffRep) _)) p =
    let
      gsimplest psimplest p = T.constUse psimplest p ssimplest
      ssimplest =
        T.constDef p a282v8simplest
          (\ p -> T.ap1 p283v13 p (gbcClean p283v13 p) fsimplest_dirty)
      gargs_f_functors pargs_f_functors p =
        T.constUse pargs_f_functors p sargs_f_functors
      gresult_g_functor pargs_f_functors p =
        T.constUse pargs_f_functors p sresult_g_functor
      j282v18args_f_functors =
        case gsimplest p282v8 p of
          T.R (DXFunc fargs_f_functors fresult_g_functor) kargs_f_functors ->
            (kargs_f_functors,fargs_f_functors,fresult_g_functor)
          _ -> T.fatal p
      sargs_f_functors =
        T.constDef p a282v25args_f_functors
          (\ _ ->
            case j282v18args_f_functors of
              (kargs_f_functors,fargs_f_functors,fresult_g_functor) ->
                T.projection p282v25 kargs_f_functors fargs_f_functors)
      sresult_g_functor =
        T.constDef p a282v41result_g_functor
          (\ _ ->
            case j282v18args_f_functors of
              (kargs_f_functors,fargs_f_functors,fresult_g_functor) ->
                T.projection p282v41 kargs_f_functors fresult_g_functor)
      gsimplestD psimplestD p = T.constUse psimplestD p ssimplestD
      ssimplestD =
        T.constDef p a289v8simplestD
          (\ p ->
            T.ap1 p289v20 p (gdxApplyDSubst_2 p289v20 p) (gsimplest p289v36 p))
      gdomainVarNames pdomainVarNames p =
        T.constUse pdomainVarNames p sdomainVarNames
      sdomainVarNames =
        T.constDef p a295v8domainVarNames
          (\ p -> T.ap2 p295v25 p (gmap p295v25 p) (gfirst p295v29 p) frho_d)
      gbigInstanceDomains pbigInstanceDomains p =
        T.constUse pbigInstanceDomains p sbigInstanceDomains
      sbigInstanceDomains =
        T.constDef p a301v8bigInstanceDomains
          (\ p -> T.ap2 p301v29 p (gmap p301v29 p) (gsecond p301v33 p) frho_d)
      gbaseInstance pbaseInstance p = T.constUse pbaseInstance p sbaseInstance
      sbaseInstance =
        T.constDef p a309v8baseInstance
          (\ p ->
            T.ap2 p309v23 p (gmyAll p309v23 p)
              (T.ap2 p309v30 p (TPrelude.gflip p309v30 p) (p309v30 !== p)
                (T.con0 p309v32 p Two aTwo)) (gbigInstanceDomains p309v37 p))
      gbarakiApplicable pbarakiApplicable p =
        T.constUse pbarakiApplicable p sbarakiApplicable
      sbarakiApplicable =
        T.constDef p a317v8barakiApplicable
          (\ p ->
            T.ap2 p318v73 p (p318v73 !&& p)
              (T.ap2 p318v13 p (gmyAll p318v13 p)
                (T.ap2 p318v23 p (p318v23 !. p) (gnot p318v20 p)
                  (gamContainsFunctionSpace p318v24 p))
                (gbigInstanceDomains p318v54 p))
              (T.ap2 p319v71 p (p319v71 !&& p)
                (T.ap1 p319v20 p
                  (T.ap2 p319v23 p (p319v23 !. p) (gnot p319v20 p)
                    (gdxContainsFnSpace p319v24 p))
                  (gresult_g_functor p319v54 p))
                (T.ap2 p320v13 p (gmyAll p320v13 p)
                  (T.ap2 p320v23 p (p320v23 !. p) (gnot p320v20 p)
                    (gdxContainsSubsidiaryFnSpace p320v24 p))
                  (gargs_f_functors p320v54 p))))
      gcanUseOpt pcanUseOpt p = T.constUse pcanUseOpt p scanUseOpt
      scanUseOpt =
        T.constDef p a330v8canUseOpt
          (\ p ->
            T.ap2 p331v13 p (gall p331v13 p)
              (T.ap2 p331v21 p (p331v21 !. p) (gnot p331v18 p)
                (gdxContainsFnSpace p331v22 p)) (gargs_f_functors p331v41 p))
      gbigInstancePoints pbigInstancePoints p =
        T.constUse pbigInstancePoints p sbigInstancePoints
      sbigInstancePoints =
        T.constDef p a337v8bigInstancePoints
          (\ p ->
            let
              gindividualIndices pindividualIndices p =
                T.constUse pindividualIndices p sindividualIndices
              sindividualIndices =
                T.constDef p a339v17individualIndices
                  (\ p ->
                    T.cif p340v22 p (gcanUseOpt p340v29 p)
                      (\ p ->
                        T.ap2 p341v29 p (gmap p341v29 p)
                          (gamMeetIRoutes p341v33 p)
                          (gbigInstanceDomains p341v47 p))
                      (\ p ->
                        T.ap2 p342v29 p (gmap p342v29 p)
                          (gamAllRoutesMinusTopJONES p342v33 p)
                          (gbigInstanceDomains p342v58 p)))
              gallIndices pallIndices p = T.constUse pallIndices p sallIndices
              sallIndices =
                T.constDef p a343v17allIndices
                  (\ p ->
                    T.ap1 p344v22 p (gmyCartesianProduct p344v22 p)
                      (gindividualIndices p344v41 p)) in
              (T.ap2 p346v17 p (gtake p346v17 p)
                (T.ap2 p346v23 p (gmax p346v23 p)
                  (T.ap1 p346v27 p (TPreludeBasic.gfromInteger p346v27 p)
                    (T.conInteger p346v27 p 1)) fthreshold)
                (gallIndices p346v40 p)))
      ginstanceTops pinstanceTops p = T.constUse pinstanceTops p sinstanceTops
      sinstanceTops =
        T.constDef p a352v8instanceTops
          (\ p ->
            T.ap2 p352v23 p (gmap p352v23 p) (gavTopR p352v27 p)
              (gbigInstanceDomains p352v34 p))
      gallRhos pallRhos p = T.constUse pallRhos p sallRhos
      sallRhos =
        T.constDef p a358v8allRhos
          (\ p ->
            let
              gmakeOneRho pmakeOneRho p =
                T.fun1 a359v17makeOneRho pmakeOneRho p hmakeOneRho
                where
                
                hmakeOneRho frs p =
                  T.ap5 p360v21 p (gmyZipWith4 p360v21 p)
                    (T.fun4 T.mkLambda p360v33 p
                      (\ fn fd fr ft p ->
                        T.con2 p360v45 p T.Tuple2 T.aTuple2 fn
                          (T.con3 p360v49 p T.Tuple3 T.aTuple3 fd fr ft)))
                    (gdomainVarNames p361v25 p) (gbigInstanceDomains p361v40 p)
                    frs (ginstanceTops p361v62 p)
                 in
              (T.ap2 p363v17 p (gmap p363v17 p) (gmakeOneRho p363v21 p)
                (gbigInstancePoints p363v32 p)))
      gall_edotfdotc pall_edotfdotc p =
        T.constUse pall_edotfdotc p sall_edotfdotc
      sall_edotfdotc =
        T.constDef p a369v8all_edotfdotc
          (\ p ->
            let
              gmakeOneEdotFdotC pmakeOneEdotFdotC p =
                T.fun1 a370v17makeOneEdotFdotC pmakeOneEdotFdotC p
                  hmakeOneEdotFdotC
                where
                
                hmakeOneEdotFdotC frho p =
                  T.ap5 p371v22 p (gbcEdotFdotC p371v22 p) frho
                    (gresult_g_functor p371v38 p) (gsimplestD p371v55 p) ffRep
                    (gargs_f_functors p372v39 p)
                 in
              (T.ap2 p374v17 p (gmap p374v17 p) (gmakeOneEdotFdotC p374v21 p)
                (gallRhos p374v38 p)))
      gbig_function_domain pbig_function_domain p =
        T.constUse pbig_function_domain p sbig_function_domain
      sbig_function_domain =
        T.constDef p a380v8big_function_domain
          (\ p ->
            T.ap2 p380v30 p (gdxApplyDSubst p380v30 p) frho_d
              (gsimplest p380v50 p)) in
      (T.cif p383v8 p (gbaseInstance p383v18 p)
        (\ p -> T.projection p384v18 p ff)
        (\ p ->
          T.cif p385v13 p
            (T.ap2 p385v33 p (p385v33 !|| p)
              (T.ap1 p385v18 p (gnot p385v18 p) fuse_baraki)
              (T.ap1 p385v36 p (gnot p385v36 p) (gbarakiApplicable p385v40 p)))
            (\ p ->
              T.ap4 p386v18 p (gacMakeInstance p386v18 p) fs_or_l
                (gsimplest p386v40 p) frho_d ff)
            (\ p ->
              T.ap2 p387v18 p (gbmNorm p387v18 p)
                (gbig_function_domain p387v25 p)
                (T.ap2 p387v46 p (gfoldl1 p387v46 p) (gbmGLB p387v53 p)
                  (gall_edotfdotc p387v59 p)))))
  hbcMakeInstance fuse_baraki fthreshold fs_or_l fsimplest frho_d ff p =
    T.ap4 p403v6 p (gacMakeInstance p403v6 p) fs_or_l fsimplest frho_d ff
  

gbcClean :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun DExpr DExpr)

gbcClean pbcClean p =
  T.fun1 abcClean pbcClean p hbcClean
  where
  
  hbcClean (T.R DXTwo _) p = T.con0 p410v31 p DXTwo aDXTwo
  hbcClean (T.R (DXLift1 (T.R T.List _)) _) p = T.con0 p411v31 p DXTwo aDXTwo
  hbcClean (T.R (DXLift2 (T.R T.List _)) _) p =
    T.con1 p412v31 p DXLift1 aDXLift1
      (T.fromExpList p412v39 p [T.con0 p412v40 p DXTwo aDXTwo])
  hbcClean (T.R (DXLift1 fdxs) _) p =
    T.con1 p413v31 p DXLift1 aDXLift1
      (T.ap2 p413v40 p (gmap p413v40 p) (gbcClean p413v44 p) fdxs)
  hbcClean (T.R (DXLift2 fdxs) _) p =
    T.con1 p414v31 p DXLift2 aDXLift2
      (T.ap2 p414v40 p (gmap p414v40 p) (gbcClean p414v44 p) fdxs)
  hbcClean (T.R (DXVar fv) _) p = T.con1 p415v31 p DXVar aDXVar fv
  hbcClean (T.R (DXFunc fdxss fdxt) _) p =
    T.con2 p416v31 p DXFunc aDXFunc
      (T.ap2 p416v39 p (gmap p416v39 p) (gbcClean p416v43 p) fdxss)
      (T.ap1 p416v58 p (gbcClean p416v58 p) fdxt)
  hbcClean _ p = T.fatal p
  

tBarakiConc3 = T.mkModule "BarakiConc3" "BarakiConc3.hs" Prelude.True

abcEApp_d = T.mkVariable tBarakiConc3 280001 3 2 "bcEApp_d" Prelude.False

abcEApp = T.mkVariable tBarakiConc3 440001 3 3 "bcEApp" Prelude.False

abcEdotF = T.mkVariable tBarakiConc3 860001 3 4 "bcEdotF" Prelude.False

abcFdotC = T.mkVariable tBarakiConc3 1610001 3 5 "bcFdotC" Prelude.False

abcApplyF0 = T.mkVariable tBarakiConc3 1790001 3 3 "bcApplyF0" Prelude.False

abcApplyF0_2 = T.mkVariable tBarakiConc3 2030001 3 3 "bcApplyF0_2" Prelude.False

abcEdotFdotC = T.mkVariable tBarakiConc3 2260001 3 5 "bcEdotFdotC" Prelude.False

abcGetR = T.mkVariable tBarakiConc3 2370001 3 1 "bcGetR" Prelude.False

abcGetD = T.mkVariable tBarakiConc3 2380001 3 1 "bcGetD" Prelude.False

abcGetT = T.mkVariable tBarakiConc3 2390001 3 1 "bcGetT" Prelude.False

abcMakeInstance =
  T.mkVariable tBarakiConc3 2700001 3 6 "bcMakeInstance" Prelude.False

abcClean = T.mkVariable tBarakiConc3 4100001 3 1 "bcClean" Prelude.False

a67v10repDomain = T.mkVariable tBarakiConc3 670010 3 0 "repDomain" Prelude.True

a99v10hf_domains =
  T.mkVariable tBarakiConc3 990010 3 0 "hf_domains" Prelude.True

a100v10new_hfs = T.mkVariable tBarakiConc3 1000010 3 0 "new_hfs" Prelude.True

a110v10hf_domains =
  T.mkVariable tBarakiConc3 1100010 3 0 "hf_domains" Prelude.True

a111v10new_hfs = T.mkVariable tBarakiConc3 1110010 3 0 "new_hfs" Prelude.True

a121v10f_top = T.mkVariable tBarakiConc3 1210010 3 0 "f_top" Prelude.True

a122v10f_top_rep =
  T.mkVariable tBarakiConc3 1220010 3 0 "f_top_rep" Prelude.True

a124v10evb = T.mkVariable tBarakiConc3 1240010 3 1 "evb" Prelude.True

a128v10ev = T.mkVariable tBarakiConc3 1280010 3 2 "ev" Prelude.True

a138v10evLookup = T.mkVariable tBarakiConc3 1380010 3 0 "evLookup" Prelude.True

a139v10evD = T.mkVariable tBarakiConc3 1390010 3 0 "evD" Prelude.True

a140v10evR = T.mkVariable tBarakiConc3 1400010 3 0 "evR" Prelude.True

a162v10new_rep = T.mkVariable tBarakiConc3 1620010 3 0 "new_rep" Prelude.True

a163v10eapp_pt = T.mkVariable tBarakiConc3 1630010 3 1 "eapp_pt" Prelude.True

a183v10new_lf = T.mkVariable tBarakiConc3 1830010 3 0 "new_lf" Prelude.True

a184v10new_hfs = T.mkVariable tBarakiConc3 1840010 3 0 "new_hfs" Prelude.True

a189v10new_lf = T.mkVariable tBarakiConc3 1890010 3 0 "new_lf" Prelude.True

a190v10new_mf = T.mkVariable tBarakiConc3 1900010 3 0 "new_mf" Prelude.True

a191v10new_hfs = T.mkVariable tBarakiConc3 1910010 3 0 "new_hfs" Prelude.True

a204v10new_f0 = T.mkVariable tBarakiConc3 2040010 3 0 "new_f0" Prelude.True

a205v10new_f1 = T.mkVariable tBarakiConc3 2050010 3 0 "new_f1" Prelude.True

a227v10f_dot_c = T.mkVariable tBarakiConc3 2270010 3 0 "f_dot_c" Prelude.True

a228v10newDs = T.mkVariable tBarakiConc3 2280010 3 0 "newDs" Prelude.True

a229v10fd_dot_c = T.mkVariable tBarakiConc3 2290010 3 0 "fd_dot_c" Prelude.True

a230v10e_dot_f_dot_c =
  T.mkVariable tBarakiConc3 2300010 3 0 "e_dot_f_dot_c" Prelude.True

a282v8simplest = T.mkVariable tBarakiConc3 2820008 3 0 "simplest" Prelude.True

a282v25args_f_functors =
  T.mkVariable tBarakiConc3 2820025 3 0 "args_f_functors" Prelude.True

a282v41result_g_functor =
  T.mkVariable tBarakiConc3 2820041 3 0 "result_g_functor" Prelude.True

a289v8simplestD = T.mkVariable tBarakiConc3 2890008 3 0 "simplestD" Prelude.True

a295v8domainVarNames =
  T.mkVariable tBarakiConc3 2950008 3 0 "domainVarNames" Prelude.True

a301v8bigInstanceDomains =
  T.mkVariable tBarakiConc3 3010008 3 0 "bigInstanceDomains" Prelude.True

a309v8baseInstance =
  T.mkVariable tBarakiConc3 3090008 3 0 "baseInstance" Prelude.True

a317v8barakiApplicable =
  T.mkVariable tBarakiConc3 3170008 3 0 "barakiApplicable" Prelude.True

a330v8canUseOpt = T.mkVariable tBarakiConc3 3300008 3 0 "canUseOpt" Prelude.True

a337v8bigInstancePoints =
  T.mkVariable tBarakiConc3 3370008 3 0 "bigInstancePoints" Prelude.True

a352v8instanceTops =
  T.mkVariable tBarakiConc3 3520008 3 0 "instanceTops" Prelude.True

a358v8allRhos = T.mkVariable tBarakiConc3 3580008 3 0 "allRhos" Prelude.True

a369v8all_edotfdotc =
  T.mkVariable tBarakiConc3 3690008 3 0 "all_edotfdotc" Prelude.True

a380v8big_function_domain =
  T.mkVariable tBarakiConc3 3800008 3 0 "big_function_domain" Prelude.True

a339v17individualIndices =
  T.mkVariable tBarakiConc3 3390017 3 0 "individualIndices" Prelude.True

a343v17allIndices =
  T.mkVariable tBarakiConc3 3430017 3 0 "allIndices" Prelude.True

a359v17makeOneRho =
  T.mkVariable tBarakiConc3 3590017 3 1 "makeOneRho" Prelude.True

a370v17makeOneEdotFdotC =
  T.mkVariable tBarakiConc3 3700017 3 1 "makeOneEdotFdotC" Prelude.True

p28v1 = T.mkSrcPos tBarakiConc3 280001

p29v6 = T.mkSrcPos tBarakiConc3 290006

p31v6 = T.mkSrcPos tBarakiConc3 310006

p31v13 = T.mkSrcPos tBarakiConc3 310013

p31v18 = T.mkSrcPos tBarakiConc3 310018

p33v6 = T.mkSrcPos tBarakiConc3 330006

p33v13 = T.mkSrcPos tBarakiConc3 330013

p33v18 = T.mkSrcPos tBarakiConc3 330018

p35v6 = T.mkSrcPos tBarakiConc3 350006

p35v12 = T.mkSrcPos tBarakiConc3 350012

p35v17 = T.mkSrcPos tBarakiConc3 350017

p35v37 = T.mkSrcPos tBarakiConc3 350037

p37v6 = T.mkSrcPos tBarakiConc3 370006

p37v14 = T.mkSrcPos tBarakiConc3 370014

p37v31 = T.mkSrcPos tBarakiConc3 370031

p44v1 = T.mkSrcPos tBarakiConc3 440001

p45v6 = T.mkSrcPos tBarakiConc3 450006

p47v6 = T.mkSrcPos tBarakiConc3 470006

p50v6 = T.mkSrcPos tBarakiConc3 500006

p52v6 = T.mkSrcPos tBarakiConc3 520006

p52v11 = T.mkSrcPos tBarakiConc3 520011

p52v23 = T.mkSrcPos tBarakiConc3 520023

p55v6 = T.mkSrcPos tBarakiConc3 550006

p57v6 = T.mkSrcPos tBarakiConc3 570006

p59v6 = T.mkSrcPos tBarakiConc3 590006

p59v13 = T.mkSrcPos tBarakiConc3 590013

p59v25 = T.mkSrcPos tBarakiConc3 590025

p62v6 = T.mkSrcPos tBarakiConc3 620006

p62v14 = T.mkSrcPos tBarakiConc3 620014

p62v31 = T.mkSrcPos tBarakiConc3 620031

p64v6 = T.mkSrcPos tBarakiConc3 640006

p64v14 = T.mkSrcPos tBarakiConc3 640014

p64v31 = T.mkSrcPos tBarakiConc3 640031

p67v10 = T.mkSrcPos tBarakiConc3 670010

p67v22 = T.mkSrcPos tBarakiConc3 670022

p67v39 = T.mkSrcPos tBarakiConc3 670039

p69v10 = T.mkSrcPos tBarakiConc3 690010

p69v30 = T.mkSrcPos tBarakiConc3 690030

p86v1 = T.mkSrcPos tBarakiConc3 860001

p91v6 = T.mkSrcPos tBarakiConc3 910006

p99v10 = T.mkSrcPos tBarakiConc3 990010

p99v23 = T.mkSrcPos tBarakiConc3 990023

p99v28 = T.mkSrcPos tBarakiConc3 990028

p100v10 = T.mkSrcPos tBarakiConc3 1000010

p100v20 = T.mkSrcPos tBarakiConc3 1000020

p100v32 = T.mkSrcPos tBarakiConc3 1000032

p100v49 = T.mkSrcPos tBarakiConc3 1000049

p102v10 = T.mkSrcPos tBarakiConc3 1020010

p102v18 = T.mkSrcPos tBarakiConc3 1020018

p110v10 = T.mkSrcPos tBarakiConc3 1100010

p110v23 = T.mkSrcPos tBarakiConc3 1100023

p110v28 = T.mkSrcPos tBarakiConc3 1100028

p111v10 = T.mkSrcPos tBarakiConc3 1110010

p111v20 = T.mkSrcPos tBarakiConc3 1110020

p111v32 = T.mkSrcPos tBarakiConc3 1110032

p111v49 = T.mkSrcPos tBarakiConc3 1110049

p113v10 = T.mkSrcPos tBarakiConc3 1130010

p113v21 = T.mkSrcPos tBarakiConc3 1130021

p121v10 = T.mkSrcPos tBarakiConc3 1210010

p121v23 = T.mkSrcPos tBarakiConc3 1210023

p122v10 = T.mkSrcPos tBarakiConc3 1220010

p122v23 = T.mkSrcPos tBarakiConc3 1220023

p122v30 = T.mkSrcPos tBarakiConc3 1220030

p124v10 = T.mkSrcPos tBarakiConc3 1240010

p124v20 = T.mkSrcPos tBarakiConc3 1240020

p125v27 = T.mkSrcPos tBarakiConc3 1250027

p125v38 = T.mkSrcPos tBarakiConc3 1250038

p125v42 = T.mkSrcPos tBarakiConc3 1250042

p126v27 = T.mkSrcPos tBarakiConc3 1260027

p126v43 = T.mkSrcPos tBarakiConc3 1260043

p126v47 = T.mkSrcPos tBarakiConc3 1260047

p128v10 = T.mkSrcPos tBarakiConc3 1280010

p128v38 = T.mkSrcPos tBarakiConc3 1280038

p129v38 = T.mkSrcPos tBarakiConc3 1290038

p131v38 = T.mkSrcPos tBarakiConc3 1310038

p131v49 = T.mkSrcPos tBarakiConc3 1310049

p131v53 = T.mkSrcPos tBarakiConc3 1310053

p132v38 = T.mkSrcPos tBarakiConc3 1320038

p132v43 = T.mkSrcPos tBarakiConc3 1320043

p132v50 = T.mkSrcPos tBarakiConc3 1320050

p132v61 = T.mkSrcPos tBarakiConc3 1320061

p134v38 = T.mkSrcPos tBarakiConc3 1340038

p134v54 = T.mkSrcPos tBarakiConc3 1340054

p134v58 = T.mkSrcPos tBarakiConc3 1340058

p135v38 = T.mkSrcPos tBarakiConc3 1350038

p135v43 = T.mkSrcPos tBarakiConc3 1350043

p135v55 = T.mkSrcPos tBarakiConc3 1350055

p135v59 = T.mkSrcPos tBarakiConc3 1350059

p136v38 = T.mkSrcPos tBarakiConc3 1360038

p136v43 = T.mkSrcPos tBarakiConc3 1360043

p136v49 = T.mkSrcPos tBarakiConc3 1360049

p136v56 = T.mkSrcPos tBarakiConc3 1360056

p136v67 = T.mkSrcPos tBarakiConc3 1360067

p138v10 = T.mkSrcPos tBarakiConc3 1380010

p138v21 = T.mkSrcPos tBarakiConc3 1380021

p138v38 = T.mkSrcPos tBarakiConc3 1380038

p139v10 = T.mkSrcPos tBarakiConc3 1390010

p139v16 = T.mkSrcPos tBarakiConc3 1390016

p139v23 = T.mkSrcPos tBarakiConc3 1390023

p140v10 = T.mkSrcPos tBarakiConc3 1400010

p140v16 = T.mkSrcPos tBarakiConc3 1400016

p140v23 = T.mkSrcPos tBarakiConc3 1400023

p142v10 = T.mkSrcPos tBarakiConc3 1420010

p142v13 = T.mkSrcPos tBarakiConc3 1420013

p142v17 = T.mkSrcPos tBarakiConc3 1420017

p161v1 = T.mkSrcPos tBarakiConc3 1610001

p162v10 = T.mkSrcPos tBarakiConc3 1620010

p162v20 = T.mkSrcPos tBarakiConc3 1620020

p162v30 = T.mkSrcPos tBarakiConc3 1620030

p163v10 = T.mkSrcPos tBarakiConc3 1630010

p164v14 = T.mkSrcPos tBarakiConc3 1640014

p164v22 = T.mkSrcPos tBarakiConc3 1640022

p164v34 = T.mkSrcPos tBarakiConc3 1640034

p166v10 = T.mkSrcPos tBarakiConc3 1660010

p179v1 = T.mkSrcPos tBarakiConc3 1790001

p180v6 = T.mkSrcPos tBarakiConc3 1800006

p180v14 = T.mkSrcPos tBarakiConc3 1800014

p183v10 = T.mkSrcPos tBarakiConc3 1830010

p183v19 = T.mkSrcPos tBarakiConc3 1830019

p184v10 = T.mkSrcPos tBarakiConc3 1840010

p184v20 = T.mkSrcPos tBarakiConc3 1840020

p184v25 = T.mkSrcPos tBarakiConc3 1840025

p186v10 = T.mkSrcPos tBarakiConc3 1860010

p186v15 = T.mkSrcPos tBarakiConc3 1860015

p186v22 = T.mkSrcPos tBarakiConc3 1860022

p189v10 = T.mkSrcPos tBarakiConc3 1890010

p189v19 = T.mkSrcPos tBarakiConc3 1890019

p190v10 = T.mkSrcPos tBarakiConc3 1900010

p190v19 = T.mkSrcPos tBarakiConc3 1900019

p191v10 = T.mkSrcPos tBarakiConc3 1910010

p191v20 = T.mkSrcPos tBarakiConc3 1910020

p191v25 = T.mkSrcPos tBarakiConc3 1910025

p193v10 = T.mkSrcPos tBarakiConc3 1930010

p193v15 = T.mkSrcPos tBarakiConc3 1930015

p193v22 = T.mkSrcPos tBarakiConc3 1930022

p193v29 = T.mkSrcPos tBarakiConc3 1930029

p203v1 = T.mkSrcPos tBarakiConc3 2030001

p204v10 = T.mkSrcPos tBarakiConc3 2040010

p204v19 = T.mkSrcPos tBarakiConc3 2040019

p205v10 = T.mkSrcPos tBarakiConc3 2050010

p205v19 = T.mkSrcPos tBarakiConc3 2050019

p207v10 = T.mkSrcPos tBarakiConc3 2070010

p207v22 = T.mkSrcPos tBarakiConc3 2070022

p207v29 = T.mkSrcPos tBarakiConc3 2070029

p226v1 = T.mkSrcPos tBarakiConc3 2260001

p227v10 = T.mkSrcPos tBarakiConc3 2270010

p227v28 = T.mkSrcPos tBarakiConc3 2270028

p227v45 = T.mkSrcPos tBarakiConc3 2270045

p228v10 = T.mkSrcPos tBarakiConc3 2280010

p228v28 = T.mkSrcPos tBarakiConc3 2280028

p228v33 = T.mkSrcPos tBarakiConc3 2280033

p229v10 = T.mkSrcPos tBarakiConc3 2290010

p229v28 = T.mkSrcPos tBarakiConc3 2290028

p229v33 = T.mkSrcPos tBarakiConc3 2290033

p230v10 = T.mkSrcPos tBarakiConc3 2300010

p230v28 = T.mkSrcPos tBarakiConc3 2300028

p230v44 = T.mkSrcPos tBarakiConc3 2300044

p230v53 = T.mkSrcPos tBarakiConc3 2300053

p232v10 = T.mkSrcPos tBarakiConc3 2320010

p232v14 = T.mkSrcPos tBarakiConc3 2320014

p237v1 = T.mkSrcPos tBarakiConc3 2370001

p237v20 = T.mkSrcPos tBarakiConc3 2370020

p238v1 = T.mkSrcPos tBarakiConc3 2380001

p238v20 = T.mkSrcPos tBarakiConc3 2380020

p239v1 = T.mkSrcPos tBarakiConc3 2390001

p239v20 = T.mkSrcPos tBarakiConc3 2390020

p270v1 = T.mkSrcPos tBarakiConc3 2700001

p282v8 = T.mkSrcPos tBarakiConc3 2820008

p283v13 = T.mkSrcPos tBarakiConc3 2830013

p282v25 = T.mkSrcPos tBarakiConc3 2820025

p282v41 = T.mkSrcPos tBarakiConc3 2820041

p289v8 = T.mkSrcPos tBarakiConc3 2890008

p289v20 = T.mkSrcPos tBarakiConc3 2890020

p289v36 = T.mkSrcPos tBarakiConc3 2890036

p295v8 = T.mkSrcPos tBarakiConc3 2950008

p295v25 = T.mkSrcPos tBarakiConc3 2950025

p295v29 = T.mkSrcPos tBarakiConc3 2950029

p301v8 = T.mkSrcPos tBarakiConc3 3010008

p301v29 = T.mkSrcPos tBarakiConc3 3010029

p301v33 = T.mkSrcPos tBarakiConc3 3010033

p309v8 = T.mkSrcPos tBarakiConc3 3090008

p309v23 = T.mkSrcPos tBarakiConc3 3090023

p309v30 = T.mkSrcPos tBarakiConc3 3090030

p309v32 = T.mkSrcPos tBarakiConc3 3090032

p309v37 = T.mkSrcPos tBarakiConc3 3090037

p317v8 = T.mkSrcPos tBarakiConc3 3170008

p318v73 = T.mkSrcPos tBarakiConc3 3180073

p318v13 = T.mkSrcPos tBarakiConc3 3180013

p318v23 = T.mkSrcPos tBarakiConc3 3180023

p318v20 = T.mkSrcPos tBarakiConc3 3180020

p318v24 = T.mkSrcPos tBarakiConc3 3180024

p318v54 = T.mkSrcPos tBarakiConc3 3180054

p319v71 = T.mkSrcPos tBarakiConc3 3190071

p319v20 = T.mkSrcPos tBarakiConc3 3190020

p319v23 = T.mkSrcPos tBarakiConc3 3190023

p319v24 = T.mkSrcPos tBarakiConc3 3190024

p319v54 = T.mkSrcPos tBarakiConc3 3190054

p320v13 = T.mkSrcPos tBarakiConc3 3200013

p320v23 = T.mkSrcPos tBarakiConc3 3200023

p320v20 = T.mkSrcPos tBarakiConc3 3200020

p320v24 = T.mkSrcPos tBarakiConc3 3200024

p320v54 = T.mkSrcPos tBarakiConc3 3200054

p330v8 = T.mkSrcPos tBarakiConc3 3300008

p331v13 = T.mkSrcPos tBarakiConc3 3310013

p331v21 = T.mkSrcPos tBarakiConc3 3310021

p331v18 = T.mkSrcPos tBarakiConc3 3310018

p331v22 = T.mkSrcPos tBarakiConc3 3310022

p331v41 = T.mkSrcPos tBarakiConc3 3310041

p337v8 = T.mkSrcPos tBarakiConc3 3370008

p339v17 = T.mkSrcPos tBarakiConc3 3390017

p340v22 = T.mkSrcPos tBarakiConc3 3400022

p340v29 = T.mkSrcPos tBarakiConc3 3400029

p341v29 = T.mkSrcPos tBarakiConc3 3410029

p341v33 = T.mkSrcPos tBarakiConc3 3410033

p341v47 = T.mkSrcPos tBarakiConc3 3410047

p342v29 = T.mkSrcPos tBarakiConc3 3420029

p342v33 = T.mkSrcPos tBarakiConc3 3420033

p342v58 = T.mkSrcPos tBarakiConc3 3420058

p343v17 = T.mkSrcPos tBarakiConc3 3430017

p344v22 = T.mkSrcPos tBarakiConc3 3440022

p344v41 = T.mkSrcPos tBarakiConc3 3440041

p346v17 = T.mkSrcPos tBarakiConc3 3460017

p346v23 = T.mkSrcPos tBarakiConc3 3460023

p346v27 = T.mkSrcPos tBarakiConc3 3460027

p346v40 = T.mkSrcPos tBarakiConc3 3460040

p352v8 = T.mkSrcPos tBarakiConc3 3520008

p352v23 = T.mkSrcPos tBarakiConc3 3520023

p352v27 = T.mkSrcPos tBarakiConc3 3520027

p352v34 = T.mkSrcPos tBarakiConc3 3520034

p358v8 = T.mkSrcPos tBarakiConc3 3580008

p359v17 = T.mkSrcPos tBarakiConc3 3590017

p360v21 = T.mkSrcPos tBarakiConc3 3600021

p360v33 = T.mkSrcPos tBarakiConc3 3600033

p360v45 = T.mkSrcPos tBarakiConc3 3600045

p360v49 = T.mkSrcPos tBarakiConc3 3600049

p361v25 = T.mkSrcPos tBarakiConc3 3610025

p361v40 = T.mkSrcPos tBarakiConc3 3610040

p361v62 = T.mkSrcPos tBarakiConc3 3610062

p363v17 = T.mkSrcPos tBarakiConc3 3630017

p363v21 = T.mkSrcPos tBarakiConc3 3630021

p363v32 = T.mkSrcPos tBarakiConc3 3630032

p369v8 = T.mkSrcPos tBarakiConc3 3690008

p370v17 = T.mkSrcPos tBarakiConc3 3700017

p371v22 = T.mkSrcPos tBarakiConc3 3710022

p371v38 = T.mkSrcPos tBarakiConc3 3710038

p371v55 = T.mkSrcPos tBarakiConc3 3710055

p372v39 = T.mkSrcPos tBarakiConc3 3720039

p374v17 = T.mkSrcPos tBarakiConc3 3740017

p374v21 = T.mkSrcPos tBarakiConc3 3740021

p374v38 = T.mkSrcPos tBarakiConc3 3740038

p380v8 = T.mkSrcPos tBarakiConc3 3800008

p380v30 = T.mkSrcPos tBarakiConc3 3800030

p380v50 = T.mkSrcPos tBarakiConc3 3800050

p383v8 = T.mkSrcPos tBarakiConc3 3830008

p383v18 = T.mkSrcPos tBarakiConc3 3830018

p384v18 = T.mkSrcPos tBarakiConc3 3840018

p385v13 = T.mkSrcPos tBarakiConc3 3850013

p385v33 = T.mkSrcPos tBarakiConc3 3850033

p385v18 = T.mkSrcPos tBarakiConc3 3850018

p385v36 = T.mkSrcPos tBarakiConc3 3850036

p385v40 = T.mkSrcPos tBarakiConc3 3850040

p386v18 = T.mkSrcPos tBarakiConc3 3860018

p386v40 = T.mkSrcPos tBarakiConc3 3860040

p387v18 = T.mkSrcPos tBarakiConc3 3870018

p387v25 = T.mkSrcPos tBarakiConc3 3870025

p387v46 = T.mkSrcPos tBarakiConc3 3870046

p387v53 = T.mkSrcPos tBarakiConc3 3870053

p387v59 = T.mkSrcPos tBarakiConc3 3870059

p403v6 = T.mkSrcPos tBarakiConc3 4030006

p410v1 = T.mkSrcPos tBarakiConc3 4100001

p410v31 = T.mkSrcPos tBarakiConc3 4100031

p411v31 = T.mkSrcPos tBarakiConc3 4110031

p412v31 = T.mkSrcPos tBarakiConc3 4120031

p412v39 = T.mkSrcPos tBarakiConc3 4120039

p412v40 = T.mkSrcPos tBarakiConc3 4120040

p413v31 = T.mkSrcPos tBarakiConc3 4130031

p413v40 = T.mkSrcPos tBarakiConc3 4130040

p413v44 = T.mkSrcPos tBarakiConc3 4130044

p414v31 = T.mkSrcPos tBarakiConc3 4140031

p414v40 = T.mkSrcPos tBarakiConc3 4140040

p414v44 = T.mkSrcPos tBarakiConc3 4140044

p415v31 = T.mkSrcPos tBarakiConc3 4150031

p416v31 = T.mkSrcPos tBarakiConc3 4160031

p416v39 = T.mkSrcPos tBarakiConc3 4160039

p416v43 = T.mkSrcPos tBarakiConc3 4160043

p416v58 = T.mkSrcPos tBarakiConc3 4160058
