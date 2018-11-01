module TFrontierDATAFN2
  (gfdImprove,gfdFind,gfdFind_aux,gfdIdent,gfdLo1,gfdHi1,gfdLo2,gfdMid2,gfdHi2
    ,gfdIsZero,gfdFs2,gfdFs_aux) where

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
import TAbstractEval2 
import TAbsConc3 
import TFrontierMisc2 

gfdImprove ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun (T.Fun Route Route)
          (T.Fun MemoList
            (T.Fun (T.List Domain)
              (T.Fun Bool
                (T.Fun (T.List FrontierElem)
                  (T.Fun (T.List FrontierElem)
                    (T.Tuple2 (T.List FrontierElem) (T.List FrontierElem))))))))

gfdImprove pfdImprove p =
  T.fun6 afdImprove pfdImprove p hfdImprove
  where
  
  hfdImprove fcoerce fmemo fdss fnaive fmax0_super fmin1_super p =
    let
      gzero_result_pairs pzero_result_pairs p =
        T.constUse pzero_result_pairs p szero_result_pairs
      gone_result_pairs pzero_result_pairs p =
        T.constUse pzero_result_pairs p sone_result_pairs
      j36v10zero_result_pairs =
        case
          T.ap2 p37v15 p (gsplitList p37v15 p)
            (T.ap2 p37v34 p (p37v34 !. p) (gfdIsZero p37v26 p)
              (T.ap2 p37v41 p (p37v41 !. p) fcoerce (gsecond p37v42 p)))
            fmemo of
          T.R (T.Tuple2 fzero_result_pairs fone_result_pairs)
              kzero_result_pairs ->
            (kzero_result_pairs,fzero_result_pairs,fone_result_pairs)
          _ -> T.fatal p
      szero_result_pairs =
        T.constDef p a36v11zero_result_pairs
          (\ _ ->
            case j36v10zero_result_pairs of
              (kzero_result_pairs,fzero_result_pairs,fone_result_pairs) ->
                T.projection p36v11 kzero_result_pairs fzero_result_pairs)
      sone_result_pairs =
        T.constDef p a36v30one_result_pairs
          (\ _ ->
            case j36v10zero_result_pairs of
              (kzero_result_pairs,fzero_result_pairs,fone_result_pairs) ->
                T.projection p36v30 kzero_result_pairs fone_result_pairs)
      gzero_nodes_max pzero_nodes_max p =
        T.constUse pzero_nodes_max p szero_nodes_max
      szero_nodes_max =
        T.constDef p a38v10zero_nodes_max
          (\ p ->
            T.ap1 p39v15 p (gavMaxfrel p39v15 p)
              (T.ap2 p39v26 p (gmap p39v26 p)
                (T.ap2 p39v38 p (p39v38 !. p)
                  (T.pa0 MkFrel T.cn1 p39v31 p aMkFrel) (gfirst p39v40 p))
                (gzero_result_pairs p39v47 p)))
      gone_nodes_min pone_nodes_min p =
        T.constUse pone_nodes_min p sone_nodes_min
      sone_nodes_min =
        T.constDef p a40v10one_nodes_min
          (\ p ->
            T.ap1 p41v15 p (gavMinfrel p41v15 p)
              (T.ap2 p41v26 p (gmap p41v26 p)
                (T.ap2 p41v38 p (p41v38 !. p)
                  (T.pa0 MkFrel T.cn1 p41v31 p aMkFrel) (gfirst p41v40 p))
                (gone_result_pairs p41v47 p)))
      gnew_max0 pnew_max0 p = T.constUse pnew_max0 p snew_max0
      snew_max0 =
        T.constDef p a42v10new_max0
          (\ p ->
            T.ap2 p44v16 p (gavLUBmax0frontier p44v16 p)
              (T.cif p43v16 p fnaive
                (\ p ->
                  T.fromExpList p43v30 p
                    [T.con1 p43v31 p MkFrel aMkFrel
                        (T.ap2 p43v39 p (gmap p43v39 p) (gavTopR p43v43 p)
                          fdss)]) (\ p -> T.projection p43v61 p fmax0_super))
              (T.ap2 p45v16 p (gspMax0FromMin1 p45v16 p) fdss
                (gone_nodes_min p45v35 p)))
      gnew_min1 pnew_min1 p = T.constUse pnew_min1 p snew_min1
      snew_min1 =
        T.constDef p a46v10new_min1
          (\ p ->
            T.ap2 p48v16 p (gavGLBmin1frontier p48v16 p)
              (T.cif p47v16 p fnaive
                (\ p ->
                  T.fromExpList p47v30 p
                    [T.con1 p47v31 p MkFrel aMkFrel
                        (T.ap2 p47v39 p (gmap p47v39 p) (gavBottomR p47v43 p)
                          fdss)]) (\ p -> T.projection p47v64 p fmin1_super))
              (T.ap2 p49v16 p (gspMin1FromMax0 p49v16 p) fdss
                (gzero_nodes_max p49v35 p))) in
      (T.con2 p51v10 p T.Tuple2 T.aTuple2 (gnew_max0 p51v11 p)
        (gnew_min1 p51v21 p))
  

gfdFind ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun ACMode
          (T.Fun (HExpr Naam)
            (T.Fun Domain
              (T.Fun (T.List Domain)
                (T.Fun (T.List Domain)
                  (T.Fun Rep
                    (T.Fun (T.Fun Route Route)
                      (T.Fun Bool
                        (T.Fun (T.List FrontierElem)
                          (T.Fun MemoList (T.Tuple2 Rep MemoList)))))))))))

gfdFind pfdFind p =
  T.fun10 afdFind pfdFind p hfdFind
  where
  
  hfdFind fs_or_l fhexpr (T.R (Func fdss (T.R Two _)) _) fsmall_argds fbig_argds
    (T.R (RepTwo (T.R (Min1Max0 far_prev fmin1_prev fmax0_prev) _)) _) fcoerce
    fnaive fmin1_ilf fmemo p =
    let
      gbetter_max0 pbetter_max0 p = T.constUse pbetter_max0 p sbetter_max0
      gbetter_min1 pbetter_max0 p = T.constUse pbetter_max0 p sbetter_min1
      j72v10better_max0 =
        case
          T.ap6 p73v15 p (gfdImprove p73v15 p) fcoerce fmemo fsmall_argds fnaive
            fmax0_prev fmin1_prev of
          T.R (T.Tuple2 fbetter_max0 fbetter_min1) kbetter_max0 ->
            (kbetter_max0,fbetter_max0,fbetter_min1)
          _ -> T.fatal p
      sbetter_max0 =
        T.constDef p a72v11better_max0
          (\ _ ->
            case j72v10better_max0 of
              (kbetter_max0,fbetter_max0,fbetter_min1) ->
                T.projection p72v11 kbetter_max0 fbetter_max0)
      sbetter_min1 =
        T.constDef p a72v24better_min1
          (\ _ ->
            case j72v10better_max0 of
              (kbetter_max0,fbetter_max0,fbetter_min1) ->
                T.projection p72v24 kbetter_max0 fbetter_min1)
      gfr pfr p = T.constUse pfr p sfr
      gnew_memo_additions pfr p = T.constUse pfr p snew_memo_additions
      j74v10fr =
        case
          T.ap7 p75v15 p (gfdFs2 p75v15 p) fs_or_l fhexpr fsmall_argds
            fbig_argds (gbetter_min1 p76v56 p) (gbetter_max0 p77v21 p)
            fcoerce of
          T.R (T.Tuple2 ffr fnew_memo_additions) kfr ->
            (kfr,ffr,fnew_memo_additions)
          _ -> T.fatal p
      sfr =
        T.constDef p a74v11fr
          (\ _ ->
            case j74v10fr of
              (kfr,ffr,fnew_memo_additions) -> T.projection p74v11 kfr ffr)
      snew_memo_additions =
        T.constDef p a74v15new_memo_additions
          (\ _ ->
            case j74v10fr of
              (kfr,ffr,fnew_memo_additions) ->
                T.projection p74v15 kfr fnew_memo_additions) in
      (T.con2 p79v10 p T.Tuple2 T.aTuple2
        (T.con1 p79v11 p RepTwo aRepTwo (gfr p79v18 p))
        (gnew_memo_additions p80v11 p))
  hfdFind fs_or_l fhexpr (T.R (Func fdss (T.R (Lift1 fdts) _)) _) fsmall_argds
    fbig_argds
    (T.R
      (Rep1 (T.R (Min1Max0 far_prev fmin1_prev_lf fmax0_prev_lf) _) fprev_hfs)
      _) fcoerce fnaive fmin1_ilf fmemo p =
    let
      gbetter_lf_max0 pbetter_lf_max0 p =
        T.constUse pbetter_lf_max0 p sbetter_lf_max0
      gbetter_lf_min1 pbetter_lf_max0 p =
        T.constUse pbetter_lf_max0 p sbetter_lf_min1
      j87v10better_lf_max0 =
        case
          T.ap6 p88v15 p (gfdImprove p88v15 p)
            (T.ap2 p88v31 p (p88v31 !. p) (gfdLo1 p88v26 p) fcoerce) fmemo
            fsmall_argds fnaive fmax0_prev_lf fmin1_prev_lf of
          T.R (T.Tuple2 fbetter_lf_max0 fbetter_lf_min1) kbetter_lf_max0 ->
            (kbetter_lf_max0,fbetter_lf_max0,fbetter_lf_min1)
          _ -> T.fatal p
      sbetter_lf_max0 =
        T.constDef p a87v11better_lf_max0
          (\ _ ->
            case j87v10better_lf_max0 of
              (kbetter_lf_max0,fbetter_lf_max0,fbetter_lf_min1) ->
                T.projection p87v11 kbetter_lf_max0 fbetter_lf_max0)
      sbetter_lf_min1 =
        T.constDef p a87v27better_lf_min1
          (\ _ ->
            case j87v10better_lf_max0 of
              (kbetter_lf_max0,fbetter_lf_max0,fbetter_lf_min1) ->
                T.projection p87v27 kbetter_lf_max0 fbetter_lf_min1)
      glofact plofact p = T.constUse plofact p slofact
      glofact_memo_additions plofact p =
        T.constUse plofact p slofact_memo_additions
      j90v10lofact =
        case
          T.ap7 p91v15 p (gfdFs2 p91v15 p) fs_or_l fhexpr fsmall_argds
            fbig_argds (gbetter_lf_min1 p92v56 p) (gbetter_lf_max0 p93v24 p)
            (T.ap2 p93v45 p (p93v45 !. p) (gfdLo1 p93v40 p) fcoerce) of
          T.R (T.Tuple2 flofact flofact_memo_additions) klofact ->
            (klofact,flofact,flofact_memo_additions)
          _ -> T.fatal p
      slofact =
        T.constDef p a90v11lofact
          (\ _ ->
            case j90v10lofact of
              (klofact,flofact,flofact_memo_additions) ->
                T.projection p90v11 klofact flofact)
      slofact_memo_additions =
        T.constDef p a90v19lofact_memo_additions
          (\ _ ->
            case j90v10lofact of
              (klofact,flofact,flofact_memo_additions) ->
                T.projection p90v19 klofact flofact_memo_additions)
      guseful_lofact_memo puseful_lofact_memo p =
        T.constUse puseful_lofact_memo p suseful_lofact_memo
      suseful_lofact_memo =
        T.constDef p a94v10useful_lofact_memo
          (\ p ->
            T.ap2 p95v15 p (gfilter p95v15 p)
              (T.ap2 p95v26 p (p95v26 !. p) (gnot p95v23 p)
                (T.ap2 p95v35 p (p95v35 !. p) (gfdIsZero p95v27 p)
                  (T.ap2 p95v41 p (p95v41 !. p) (gfdLo1 p95v36 p)
                    (T.ap2 p95v48 p (p95v48 !. p) fcoerce (gsecond p95v49 p)))))
              (T.ap2 p96v45 p (p96v45 !++ p) (glofact_memo_additions p96v23 p)
                fmemo))
      gmin1_lofact pmin1_lofact p = T.constUse pmin1_lofact p smin1_lofact
      smin1_lofact =
        T.constDef p a98v10min1_lofact
          (\ p ->
            T.ccase p99v15 p
              (let
                v99v15v1 (T.R (Min1Max0 flf_ar flf_f1 flf_f0) _) p =
                  T.projection p99v61 p flf_f1
                v99v15v1 _ p = T.fatal p in (v99v15v1)) (glofact p99v20 p))
      ghifacts phifacts p = T.constUse phifacts p shifacts
      ghifact_memo_additions phifacts p =
        T.constUse phifacts p shifact_memo_additions
      j101v10hifacts =
        case
          T.ap11 p102v15 p (gfdFind_aux p102v15 p) fs_or_l fsmall_argds
            fbig_argds fdts fhexpr fprev_hfs fcoerce
            (guseful_lofact_memo p103v26 p) (T.con0 p103v45 p False aFalse)
            (gmin1_lofact p103v51 p) fnaive of
          T.R (T.Tuple2 fhifacts fhifact_memo_additions) khifacts ->
            (khifacts,fhifacts,fhifact_memo_additions)
          _ -> T.fatal p
      shifacts =
        T.constDef p a101v11hifacts
          (\ _ ->
            case j101v10hifacts of
              (khifacts,fhifacts,fhifact_memo_additions) ->
                T.projection p101v11 khifacts fhifacts)
      shifact_memo_additions =
        T.constDef p a101v20hifact_memo_additions
          (\ _ ->
            case j101v10hifacts of
              (khifacts,fhifacts,fhifact_memo_additions) ->
                T.projection p101v20 khifacts fhifact_memo_additions) in
      (T.con2 p105v10 p T.Tuple2 T.aTuple2
        (T.con2 p105v11 p Rep1 aRep1 (glofact p105v16 p) (ghifacts p105v23 p))
        (T.ap2 p106v33 p (p106v33 !++ p) (glofact_memo_additions p106v11 p)
          (ghifact_memo_additions p106v36 p)))
  hfdFind fs_or_l fhexpr (T.R (Func fdss (T.R (Lift2 fdts) _)) _) fsmall_argds
    fbig_argds
    (T.R
      (Rep2 (T.R (Min1Max0 far_prev_lf fmin1_prev_lf fmax0_prev_lf) _)
        (T.R (Min1Max0 far_prev_mf fmin1_prev_mf fmax0_prev_mf) _) fprev_hfs) _)
    fcoerce fnaive fmin1_ilf fmemo p =
    let
      gbetter_lf_max0 pbetter_lf_max0 p =
        T.constUse pbetter_lf_max0 p sbetter_lf_max0
      gbetter_lf_min1 pbetter_lf_max0 p =
        T.constUse pbetter_lf_max0 p sbetter_lf_min1
      j114v10better_lf_max0 =
        case
          T.ap6 p115v15 p (gfdImprove p115v15 p)
            (T.ap2 p115v31 p (p115v31 !. p) (gfdLo2 p115v26 p) fcoerce) fmemo
            fsmall_argds fnaive fmax0_prev_lf fmin1_prev_lf of
          T.R (T.Tuple2 fbetter_lf_max0 fbetter_lf_min1) kbetter_lf_max0 ->
            (kbetter_lf_max0,fbetter_lf_max0,fbetter_lf_min1)
          _ -> T.fatal p
      sbetter_lf_max0 =
        T.constDef p a114v11better_lf_max0
          (\ _ ->
            case j114v10better_lf_max0 of
              (kbetter_lf_max0,fbetter_lf_max0,fbetter_lf_min1) ->
                T.projection p114v11 kbetter_lf_max0 fbetter_lf_max0)
      sbetter_lf_min1 =
        T.constDef p a114v27better_lf_min1
          (\ _ ->
            case j114v10better_lf_max0 of
              (kbetter_lf_max0,fbetter_lf_max0,fbetter_lf_min1) ->
                T.projection p114v27 kbetter_lf_max0 fbetter_lf_min1)
      glofact plofact p = T.constUse plofact p slofact
      glofact_memo_additions plofact p =
        T.constUse plofact p slofact_memo_additions
      j117v10lofact =
        case
          T.ap7 p118v15 p (gfdFs2 p118v15 p) fs_or_l fhexpr fsmall_argds
            fbig_argds (gbetter_lf_min1 p119v56 p) (gbetter_lf_max0 p120v42 p)
            (T.ap2 p120v63 p (p120v63 !. p) (gfdLo2 p120v58 p) fcoerce) of
          T.R (T.Tuple2 flofact flofact_memo_additions) klofact ->
            (klofact,flofact,flofact_memo_additions)
          _ -> T.fatal p
      slofact =
        T.constDef p a117v11lofact
          (\ _ ->
            case j117v10lofact of
              (klofact,flofact,flofact_memo_additions) ->
                T.projection p117v11 klofact flofact)
      slofact_memo_additions =
        T.constDef p a117v19lofact_memo_additions
          (\ _ ->
            case j117v10lofact of
              (klofact,flofact,flofact_memo_additions) ->
                T.projection p117v19 klofact flofact_memo_additions)
      guseful_lofact_memo puseful_lofact_memo p =
        T.constUse puseful_lofact_memo p suseful_lofact_memo
      suseful_lofact_memo =
        T.constDef p a121v10useful_lofact_memo
          (\ p ->
            T.ap2 p122v15 p (gfilter p122v15 p)
              (T.ap2 p122v26 p (p122v26 !. p) (gnot p122v23 p)
                (T.ap2 p122v35 p (p122v35 !. p) (gfdIsZero p122v27 p)
                  (T.ap2 p122v41 p (p122v41 !. p) (gfdLo2 p122v36 p)
                    (T.ap2 p122v48 p (p122v48 !. p) fcoerce
                      (gsecond p122v49 p)))))
              (T.ap2 p123v45 p (p123v45 !++ p)
                (glofact_memo_additions p123v23 p) fmemo))
      gmin1_lofact pmin1_lofact p = T.constUse pmin1_lofact p smin1_lofact
      smin1_lofact =
        T.constDef p a125v10min1_lofact
          (\ p ->
            T.ccase p126v15 p
              (let
                v126v15v1 (T.R (Min1Max0 flf_ar flf_f1 flf_f0) _) p =
                  T.projection p126v61 p flf_f1
                v126v15v1 _ p = T.fatal p in (v126v15v1)) (glofact p126v20 p))
      gbetter_mf_max0 pbetter_mf_max0 p =
        T.constUse pbetter_mf_max0 p sbetter_mf_max0
      gbetter_mf_min1 pbetter_mf_max0 p =
        T.constUse pbetter_mf_max0 p sbetter_mf_min1
      j128v10better_mf_max0 =
        case
          T.ap6 p129v15 p (gfdImprove p129v15 p)
            (T.ap2 p129v32 p (p129v32 !. p) (gfdMid2 p129v26 p) fcoerce)
            (guseful_lofact_memo p129v41 p) fsmall_argds fnaive fmax0_prev_mf
            fmin1_prev_mf of
          T.R (T.Tuple2 fbetter_mf_max0 fbetter_mf_min1) kbetter_mf_max0 ->
            (kbetter_mf_max0,fbetter_mf_max0,fbetter_mf_min1)
          _ -> T.fatal p
      sbetter_mf_max0 =
        T.constDef p a128v11better_mf_max0
          (\ _ ->
            case j128v10better_mf_max0 of
              (kbetter_mf_max0,fbetter_mf_max0,fbetter_mf_min1) ->
                T.projection p128v11 kbetter_mf_max0 fbetter_mf_max0)
      sbetter_mf_min1 =
        T.constDef p a128v27better_mf_min1
          (\ _ ->
            case j128v10better_mf_max0 of
              (kbetter_mf_max0,fbetter_mf_max0,fbetter_mf_min1) ->
                T.projection p128v27 kbetter_mf_max0 fbetter_mf_min1)
      gmidfact pmidfact p = T.constUse pmidfact p smidfact
      gmidfact_memo_additions pmidfact p =
        T.constUse pmidfact p smidfact_memo_additions
      j131v10midfact =
        case
          T.ap7 p132v15 p (gfdFs2 p132v15 p) fs_or_l fhexpr fsmall_argds
            fbig_argds (gbetter_mf_min1 p133v59 p) (gbetter_mf_max0 p134v43 p)
            (T.ap2 p134v65 p (p134v65 !. p) (gfdMid2 p134v59 p) fcoerce) of
          T.R (T.Tuple2 fmidfact fmidfact_memo_additions) kmidfact ->
            (kmidfact,fmidfact,fmidfact_memo_additions)
          _ -> T.fatal p
      smidfact =
        T.constDef p a131v11midfact
          (\ _ ->
            case j131v10midfact of
              (kmidfact,fmidfact,fmidfact_memo_additions) ->
                T.projection p131v11 kmidfact fmidfact)
      smidfact_memo_additions =
        T.constDef p a131v20midfact_memo_additions
          (\ _ ->
            case j131v10midfact of
              (kmidfact,fmidfact,fmidfact_memo_additions) ->
                T.projection p131v20 kmidfact fmidfact_memo_additions)
      guseful_midfact_memo puseful_midfact_memo p =
        T.constUse puseful_midfact_memo p suseful_midfact_memo
      suseful_midfact_memo =
        T.constDef p a135v10useful_midfact_memo
          (\ p ->
            T.ap2 p136v15 p (gfilter p136v15 p)
              (T.ap2 p136v26 p (p136v26 !. p) (gnot p136v23 p)
                (T.ap2 p136v35 p (p136v35 !. p) (gfdIsZero p136v27 p)
                  (T.ap2 p136v42 p (p136v42 !. p) (gfdMid2 p136v36 p)
                    (T.ap2 p136v49 p (p136v49 !. p) fcoerce
                      (gsecond p136v50 p)))))
              (T.ap2 p137v46 p (p137v46 !++ p)
                (gmidfact_memo_additions p137v23 p)
                (guseful_lofact_memo p137v49 p)))
      gmin1_midfact pmin1_midfact p = T.constUse pmin1_midfact p smin1_midfact
      smin1_midfact =
        T.constDef p a139v10min1_midfact
          (\ p ->
            T.ccase p140v15 p
              (let
                v140v15v1 (T.R (Min1Max0 fmf_ar fmf_f1 fmf_f0) _) p =
                  T.projection p140v62 p fmf_f1
                v140v15v1 _ p = T.fatal p in (v140v15v1)) (gmidfact p140v20 p))
      ghifacts phifacts p = T.constUse phifacts p shifacts
      ghifact_memo_additions phifacts p =
        T.constUse phifacts p shifact_memo_additions
      j142v10hifacts =
        case
          T.ap11 p143v15 p (gfdFind_aux p143v15 p) fs_or_l fsmall_argds
            fbig_argds fdts fhexpr fprev_hfs fcoerce
            (guseful_midfact_memo p144v26 p) (T.con0 p144v46 p True aTrue)
            (gmin1_midfact p144v51 p) fnaive of
          T.R (T.Tuple2 fhifacts fhifact_memo_additions) khifacts ->
            (khifacts,fhifacts,fhifact_memo_additions)
          _ -> T.fatal p
      shifacts =
        T.constDef p a142v11hifacts
          (\ _ ->
            case j142v10hifacts of
              (khifacts,fhifacts,fhifact_memo_additions) ->
                T.projection p142v11 khifacts fhifacts)
      shifact_memo_additions =
        T.constDef p a142v20hifact_memo_additions
          (\ _ ->
            case j142v10hifacts of
              (khifacts,fhifacts,fhifact_memo_additions) ->
                T.projection p142v20 khifacts fhifact_memo_additions) in
      (T.con2 p146v10 p T.Tuple2 T.aTuple2
        (T.con3 p146v11 p Rep2 aRep2 (glofact p146v16 p) (gmidfact p146v23 p)
          (ghifacts p146v31 p))
        (T.ap2 p147v33 p (p147v33 !++ p) (glofact_memo_additions p147v11 p)
          (T.ap2 p148v34 p (p148v34 !++ p) (gmidfact_memo_additions p148v11 p)
            (ghifact_memo_additions p148v37 p))))
  hfdFind _ _ _ _ _ _ _ _ _ _ p = T.fatal p
  

gfdFind_aux ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun ACMode
          (T.Fun (T.List Domain)
            (T.Fun (T.List Domain)
              (T.Fun (T.List Domain)
                (T.Fun (HExpr Naam)
                  (T.Fun (T.List Rep)
                    (T.Fun (T.Fun Route Route)
                      (T.Fun MemoList
                        (T.Fun Bool
                          (T.Fun (T.List FrontierElem)
                            (T.Fun Bool
                              (T.Tuple2 (T.List Rep) MemoList))))))))))))

gfdFind_aux pfdFind_aux p =
  T.fun11 afdFind_aux pfdFind_aux p hfdFind_aux
  where
  
  hfdFind_aux fs_or_l fsmall_argds fbig_argds fdts fhexpr fprev_hfs fcoerce
    finitial_hf_memo fdouble_lift fmin1_ilf fnaive p =
    let
      ghigh_coerce phigh_coerce p = T.constUse phigh_coerce p shigh_coerce
      shigh_coerce =
        T.constDef p a170v10high_coerce
          (\ p ->
            T.cif p171v15 p fdouble_lift (\ p -> gfdHi2 p171v35 p)
              (\ p -> gfdHi1 p171v46 p))
      gsmall_hifact_domains psmall_hifact_domains p =
        T.constUse psmall_hifact_domains p ssmall_hifact_domains
      ssmall_hifact_domains =
        T.constDef p a172v10small_hifact_domains
          (\ p ->
            T.ap2 p173v15 p (gmap p173v15 p)
              (T.ap1 p173v20 p (gavUncurry p173v20 p) fsmall_argds) fdts)
      ghifact_info_tuples phifact_info_tuples p =
        T.constUse phifact_info_tuples p shifact_info_tuples
      shifact_info_tuples =
        T.constDef p a174v10hifact_info_tuples
          (\ p ->
            T.ap5 p175v15 p (gmyZipWith4 p175v15 p) (gmkTuple p175v26 p)
              (T.ap2 p175v38 p (gmyIntsFromTo p175v38 p)
                (T.ap1 p175v35 p (TPreludeBasic.gfromInteger p175v35 p)
                  (T.conInteger p175v35 p 0))
                (T.ap2 p175v64 p (p175v64 !- p)
                  (T.ap1 p175v53 p (glength p175v53 p) fdts)
                  (T.ap1 p175v66 p (TPreludeBasic.gfromInteger p175v66 p)
                    (T.conInteger p175v66 p 1)))) fprev_hfs
              (T.ap2 p176v36 p (gmap p176v36 p) (gavBottomR p176v40 p) fdts)
              (gsmall_hifact_domains p176v55 p))
      gmkTuple pmkTuple p =
        T.fun4 a177v10mkTuple pmkTuple p hmkTuple
        where
        
        hmkTuple fn fhf_prev fbottom fsmall_hf_domain p =
          T.con3 p178v15 p T.Tuple3 T.aTuple3 fsmall_hf_domain
            (T.ap2 p178v55 p (p178v55 !. p)
              (T.ap2 p178v34 p (ghigh_coerce p178v34 p) fbottom fn) fcoerce)
            fhf_prev
        
      ghf_memo_additions phf_memo_additions p =
        T.constUse phf_memo_additions p shf_memo_additions
      ghifacts phf_memo_additions p = T.constUse phf_memo_additions p shifacts
      j180v10hf_memo_additions =
        case
          T.ap3 p181v15 p (gmapAccuml p181v15 p) (gdoOne p181v25 p)
            (T.con0 p181v31 p T.List T.aList) (ghifact_info_tuples p181v34 p) of
          T.R (T.Tuple2 fhf_memo_additions fhifacts) khf_memo_additions ->
            (khf_memo_additions,fhf_memo_additions,fhifacts)
          _ -> T.fatal p
      shf_memo_additions =
        T.constDef p a180v11hf_memo_additions
          (\ _ ->
            case j180v10hf_memo_additions of
              (khf_memo_additions,fhf_memo_additions,fhifacts) ->
                T.projection p180v11 khf_memo_additions fhf_memo_additions)
      shifacts =
        T.constDef p a180v30hifacts
          (\ _ ->
            case j180v10hf_memo_additions of
              (khf_memo_additions,fhf_memo_additions,fhifacts) ->
                T.projection p180v30 khf_memo_additions fhifacts)
      gdoOne pdoOne p =
        T.fun2 a183v10doOne pdoOne p hdoOne
        where
        
        hdoOne fmemo_adds_so_far (T.R (T.Tuple3 fhf_dom fhf_coerce fhf_preev) _)
          p =
          let
            grep prep p = T.constUse prep p srep
            gmore_memo_additions prep p = T.constUse prep p smore_memo_additions
            j184v19rep =
              case
                T.ap10 p185v24 p (gfdFind p185v24 p) fs_or_l fhexpr fhf_dom
                  fsmall_argds fbig_argds fhf_preev fhf_coerce fnaive fmin1_ilf
                  (T.ap2 p187v49 p (p187v49 !++ p) fmemo_adds_so_far
                    finitial_hf_memo) of
                T.R (T.Tuple2 frep fmore_memo_additions) krep ->
                  (krep,frep,fmore_memo_additions)
                _ -> T.fatal p
            srep =
              T.constDef p a184v20rep
                (\ _ ->
                  case j184v19rep of
                    (krep,frep,fmore_memo_additions) ->
                      T.projection p184v20 krep frep)
            smore_memo_additions =
              T.constDef p a184v25more_memo_additions
                (\ _ ->
                  case j184v19rep of
                    (krep,frep,fmore_memo_additions) ->
                      T.projection p184v25 krep fmore_memo_additions) in
            (T.con2 p188v19 p T.Tuple2 T.aTuple2
              (T.ap2 p188v40 p (p188v40 !++ p) (gmore_memo_additions p188v20 p)
                fmemo_adds_so_far) (grep p188v61 p))
        hdoOne _ _ p = T.fatal p
         in
      (T.con2 p190v10 p T.Tuple2 T.aTuple2 (ghifacts p190v11 p)
        (ghf_memo_additions p190v20 p))
  

gfdIdent :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route Route)

gfdIdent pfdIdent p =
  T.fun1 afdIdent pfdIdent p hfdIdent
  where
  
  hfdIdent fp p = T.projection p196v14 p fp
  

gfdLo1 :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route Route)

gfdLo1 pfdLo1 p =
  T.fun1 afdLo1 pfdLo1 p hfdLo1
  where
  
  hfdLo1 (T.R Stop1 _) p = T.con0 p199v19 p Zero aZero
  hfdLo1 (T.R (Up1 frs) _) p = T.con0 p200v19 p One aOne
  hfdLo1 _ p = T.fatal p
  

gfdHi1 ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route (T.Fun Int (T.Fun Route Route)))

gfdHi1 pfdHi1 p =
  T.fun3 afdHi1 pfdHi1 p hfdHi1
  where
  
  hfdHi1 fbottom fn (T.R Stop1 _) p = T.projection p203v28 p fbottom
  hfdHi1 fbottom fn (T.R (Up1 frs) _) p = T.ap2 p204v31 p (p204v31 !## p) frs fn
  hfdHi1 _ _ _ p = T.fatal p
  

gfdLo2 :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route Route)

gfdLo2 pfdLo2 p =
  T.fun1 afdLo2 pfdLo2 p hfdLo2
  where
  
  hfdLo2 (T.R Stop2 _) p = T.con0 p207v21 p Zero aZero
  hfdLo2 (T.R Up2 _) p = T.con0 p208v21 p One aOne
  hfdLo2 (T.R (UpUp2 frs) _) p = T.con0 p209v21 p One aOne
  hfdLo2 _ p = T.fatal p
  

gfdMid2 :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route Route)

gfdMid2 pfdMid2 p =
  T.fun1 afdMid2 pfdMid2 p hfdMid2
  where
  
  hfdMid2 (T.R Stop2 _) p = T.con0 p212v22 p Zero aZero
  hfdMid2 (T.R Up2 _) p = T.con0 p213v22 p Zero aZero
  hfdMid2 (T.R (UpUp2 frs) _) p = T.con0 p214v22 p One aOne
  hfdMid2 _ p = T.fatal p
  

gfdHi2 ::
  T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route (T.Fun Int (T.Fun Route Route)))

gfdHi2 pfdHi2 p =
  T.fun3 afdHi2 pfdHi2 p hfdHi2
  where
  
  hfdHi2 fbottom fn (T.R Stop2 _) p = T.projection p217v30 p fbottom
  hfdHi2 fbottom fn (T.R Up2 _) p = T.projection p218v30 p fbottom
  hfdHi2 fbottom fn (T.R (UpUp2 frs) _) p =
    T.ap2 p219v33 p (p219v33 !## p) frs fn
  hfdHi2 _ _ _ p = T.fatal p
  

gfdIsZero :: T.RefSrcPos -> T.RefExp -> T.R (T.Fun Route Bool)

gfdIsZero pfdIsZero p =
  T.fun1 afdIsZero pfdIsZero p hfdIsZero
  where
  
  hfdIsZero fx p =
    T.ccase p222v14 p
      (let
        v222v14v1 (T.R Zero _) p = T.con0 p222v33 p True aTrue
        v222v14v1 (T.R One _) p = T.con0 p222v46 p False aFalse
        v222v14v1 _ p = T.fatal p in (v222v14v1)) fx
  

gfdFs2 ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun ACMode
          (T.Fun (HExpr Naam)
            (T.Fun (T.List Domain)
              (T.Fun (T.List Domain)
                (T.Fun (T.List FrontierElem)
                  (T.Fun (T.List FrontierElem)
                    (T.Fun (T.Fun Route Route)
                      (T.Tuple2 Frontier MemoList))))))))

gfdFs2 pfdFs2 p =
  T.fun7 afdFs2 pfdFs2 p hfdFs2
  where
  
  hfdFs2 fs_or_l fhexpr fsmall_argds fbig_argds fmin1_prev fmax0_prev fcoerce
    p =
    let
      ginitial_yy pinitial_yy p = T.constUse pinitial_yy p sinitial_yy
      sinitial_yy =
        T.constDef p a237v10initial_yy
          (\ p -> T.projection p238v15 p fmax0_prev)
      ginitial_xx pinitial_xx p = T.constUse pinitial_xx p sinitial_xx
      sinitial_xx =
        T.constDef p a239v10initial_xx
          (\ p -> T.projection p240v15 p fmin1_prev)
      gfinal_yy pfinal_yy p = T.constUse pfinal_yy p sfinal_yy
      gfinal_xx pfinal_yy p = T.constUse pfinal_yy p sfinal_xx
      gfinalMemo pfinal_yy p = T.constUse pfinal_yy p sfinalMemo
      j241v10final_yy =
        case
          T.ap10 p242v15 p (gfdFs_aux p242v15 p) fs_or_l fhexpr fsmall_argds
            fbig_argds fcoerce (ginitial_yy p243v24 p) (ginitial_xx p243v35 p)
            (T.con0 p243v46 p True aTrue) (T.con0 p243v51 p T.List T.aList)
            (T.ap2 p243v55 p (gutRandomInts p243v55 p)
              (T.ap1 p243v68 p (TPreludeBasic.gfromInteger p243v68 p)
                (T.conInteger p243v68 p 1))
              (T.ap1 p243v70 p (TPreludeBasic.gfromInteger p243v70 p)
                (T.conInteger p243v70 p 2))) of
          T.R (T.Tuple3 ffinal_yy ffinal_xx ffinalMemo) kfinal_yy ->
            (kfinal_yy,ffinal_yy,ffinal_xx,ffinalMemo)
          _ -> T.fatal p
      sfinal_yy =
        T.constDef p a241v11final_yy
          (\ _ ->
            case j241v10final_yy of
              (kfinal_yy,ffinal_yy,ffinal_xx,ffinalMemo) ->
                T.projection p241v11 kfinal_yy ffinal_yy)
      sfinal_xx =
        T.constDef p a241v21final_xx
          (\ _ ->
            case j241v10final_yy of
              (kfinal_yy,ffinal_yy,ffinal_xx,ffinalMemo) ->
                T.projection p241v21 kfinal_yy ffinal_xx)
      sfinalMemo =
        T.constDef p a241v31finalMemo
          (\ _ ->
            case j241v10final_yy of
              (kfinal_yy,ffinal_yy,ffinal_xx,ffinalMemo) ->
                T.projection p241v31 kfinal_yy ffinalMemo)
      gresult presult p = T.constUse presult p sresult
      sresult =
        T.constDef p a244v10result
          (\ p ->
            T.con3 p245v15 p Min1Max0 aMin1Max0
              (T.ap1 p245v25 p (glength p245v25 p) fsmall_argds)
              (gfinal_xx p245v45 p) (gfinal_yy p245v54 p)) in
      (T.con2 p250v10 p T.Tuple2 T.aTuple2 (gresult p250v11 p)
        (gfinalMemo p250v19 p))
  

gfdFs_aux ::
  T.RefSrcPos ->
    T.RefExp ->
      T.R
        (T.Fun ACMode
          (T.Fun (HExpr Naam)
            (T.Fun (T.List Domain)
              (T.Fun (T.List Domain)
                (T.Fun (T.Fun Route Route)
                  (T.Fun (T.List FrontierElem)
                    (T.Fun (T.List FrontierElem)
                      (T.Fun Bool
                        (T.Fun MemoList
                          (T.Fun (T.List Int)
                            (T.Tuple3 (T.List FrontierElem)
                              (T.List FrontierElem) MemoList)))))))))))

gfdFs_aux pfdFs_aux p =
  T.fun10 afdFs_aux pfdFs_aux p hfdFs_aux
  where
  
  hfdFs_aux fs_or_l fhexpr fsmall_argds fbig_argds fcoerce ftrial_max_yy
    ftrial_min_xx ffromTop fmemo frands p =
    let
      gedgez pedgez p = T.constUse pedgez p sedgez
      sedgez =
        T.constDef p a273v10edgez
          (\ p ->
            T.ap4 p274v15 p (gfmSelect p274v15 p)
              (T.ap1 p274v25 p (ghead p274v25 p) frands) ftrial_min_xx
              ftrial_max_yy ffromTop)
      gargs pargs p = T.constUse pargs p sargs
      j275v10args =
        case gedgez p276v15 p of
          T.R (Just (T.R (MkFrel fargs) _)) kargs -> (kargs,fargs)
          _ -> T.fatal p
      sargs =
        T.constDef p a275v23args
          (\ _ ->
            case j275v10args of
              (kargs,fargs) -> T.projection p275v23 kargs fargs)
      gargs_at_proper_sizes pargs_at_proper_sizes p =
        T.constUse pargs_at_proper_sizes p sargs_at_proper_sizes
      sargs_at_proper_sizes =
        T.constDef p a277v10args_at_proper_sizes
          (\ p ->
            T.ap4 p278v15 p (gmyZipWith3 p278v15 p)
              (T.ap1 p278v27 p (gacConc p278v27 p) fs_or_l) fbig_argds
              fsmall_argds (gargs p278v64 p))
      gevald_app pevald_app p = T.constUse pevald_app p sevald_app
      sevald_app =
        T.constDef p a279v10evald_app
          (\ p ->
            T.ap2 p280v15 p (gaeEvalExact p280v15 p) fhexpr
              (T.ap2 p280v34 p (gmap p280v34 p)
                (T.pa0 HPoint T.cn1 p280v38 p aHPoint)
                (gargs_at_proper_sizes p280v45 p)))
      gcoerced_evald_app pcoerced_evald_app p =
        T.constUse pcoerced_evald_app p scoerced_evald_app
      scoerced_evald_app =
        T.constDef p a281v10coerced_evald_app
          (\ p -> T.ap1 p282v15 p fcoerce (gevald_app p282v22 p))
      grevised_max_yy prevised_max_yy p =
        T.constUse prevised_max_yy p srevised_max_yy
      srevised_max_yy =
        T.constDef p a283v10revised_max_yy
          (\ p ->
            T.ap3 p284v15 p (gfmReviseMaxYY p284v15 p) fsmall_argds
              ftrial_max_yy (T.con1 p284v55 p MkFrel aMkFrel (gargs p284v62 p)))
      grevised_min_xx prevised_min_xx p =
        T.constUse prevised_min_xx p srevised_min_xx
      srevised_min_xx =
        T.constDef p a285v10revised_min_xx
          (\ p ->
            T.ap3 p286v15 p (gfmReviseMinXX p286v15 p) fsmall_argds
              ftrial_min_xx (T.con1 p286v55 p MkFrel aMkFrel (gargs p286v62 p)))
      gnew_memo pnew_memo p = T.constUse pnew_memo p snew_memo
      snew_memo =
        T.constDef p a287v10new_memo
          (\ p ->
            T.con2 p288v33 p T.Cons T.aCons
              (T.con2 p288v15 p T.Tuple2 T.aTuple2 (gargs p288v16 p)
                (gevald_app p288v22 p)) fmemo) in
      (T.cif p290v10 p
        (T.ap1 p290v18 p (gfmIsNothing p290v18 p) (gedgez p290v30 p))
        (\ p ->
          T.con3 p291v18 p T.Tuple3 T.aTuple3
            (T.ap1 p291v19 p (gsort p291v19 p) ftrial_max_yy)
            (T.ap1 p291v38 p (gsort p291v38 p) ftrial_min_xx) fmemo)
        (\ p ->
          T.cif p293v10 p
            (T.ap2 p293v36 p (p293v36 !== p) (gcoerced_evald_app p293v18 p)
              (T.con0 p293v39 p One aOne))
            (\ p ->
              T.ap10 p294v18 p (gfdFs_aux p294v18 p) fs_or_l fhexpr fsmall_argds
                fbig_argds fcoerce (grevised_max_yy p296v27 p) ftrial_min_xx
                (T.con0 p296v55 p False aFalse) (gnew_memo p296v61 p)
                (T.ap1 p296v71 p (gtail p296v71 p) frands))
            (\ p ->
              T.cif p298v10 p
                (T.ap2 p298v36 p (p298v36 !== p) (gcoerced_evald_app p298v18 p)
                  (T.con0 p298v39 p Zero aZero))
                (\ p ->
                  T.ap10 p299v18 p (gfdFs_aux p299v18 p) fs_or_l fhexpr
                    fsmall_argds fbig_argds fcoerce ftrial_max_yy
                    (grevised_min_xx p301v40 p) (T.con0 p301v55 p True aTrue)
                    (gnew_memo p301v60 p)
                    (T.ap1 p301v70 p (gtail p301v70 p) frands))
                (\ p ->
                  T.ap1 p302v18 p (gpanic p302v18 p)
                    (T.fromLitString p302v24 p "fdFs_aux")))))
  

tFrontierDATAFN2 =
  T.mkModule "FrontierDATAFN2" "FrontierDATAFN2.hs" Prelude.True

afdImprove = T.mkVariable tFrontierDATAFN2 340001 3 6 "fdImprove" Prelude.False

afdFind = T.mkVariable tFrontierDATAFN2 680001 3 10 "fdFind" Prelude.False

afdFind_aux =
  T.mkVariable tFrontierDATAFN2 1670001 3 11 "fdFind_aux" Prelude.False

afdIdent = T.mkVariable tFrontierDATAFN2 1960001 3 1 "fdIdent" Prelude.False

afdLo1 = T.mkVariable tFrontierDATAFN2 1990001 3 1 "fdLo1" Prelude.False

afdHi1 = T.mkVariable tFrontierDATAFN2 2030001 3 3 "fdHi1" Prelude.False

afdLo2 = T.mkVariable tFrontierDATAFN2 2070001 3 1 "fdLo2" Prelude.False

afdMid2 = T.mkVariable tFrontierDATAFN2 2120001 3 1 "fdMid2" Prelude.False

afdHi2 = T.mkVariable tFrontierDATAFN2 2170001 3 3 "fdHi2" Prelude.False

afdIsZero = T.mkVariable tFrontierDATAFN2 2220001 3 1 "fdIsZero" Prelude.False

afdFs2 = T.mkVariable tFrontierDATAFN2 2360001 3 7 "fdFs2" Prelude.False

afdFs_aux = T.mkVariable tFrontierDATAFN2 2710001 3 10 "fdFs_aux" Prelude.False

a36v11zero_result_pairs =
  T.mkVariable tFrontierDATAFN2 360011 3 0 "zero_result_pairs" Prelude.True

a36v30one_result_pairs =
  T.mkVariable tFrontierDATAFN2 360030 3 0 "one_result_pairs" Prelude.True

a38v10zero_nodes_max =
  T.mkVariable tFrontierDATAFN2 380010 3 0 "zero_nodes_max" Prelude.True

a40v10one_nodes_min =
  T.mkVariable tFrontierDATAFN2 400010 3 0 "one_nodes_min" Prelude.True

a42v10new_max0 =
  T.mkVariable tFrontierDATAFN2 420010 3 0 "new_max0" Prelude.True

a46v10new_min1 =
  T.mkVariable tFrontierDATAFN2 460010 3 0 "new_min1" Prelude.True

a72v11better_max0 =
  T.mkVariable tFrontierDATAFN2 720011 3 0 "better_max0" Prelude.True

a72v24better_min1 =
  T.mkVariable tFrontierDATAFN2 720024 3 0 "better_min1" Prelude.True

a74v11fr = T.mkVariable tFrontierDATAFN2 740011 3 0 "fr" Prelude.True

a74v15new_memo_additions =
  T.mkVariable tFrontierDATAFN2 740015 3 0 "new_memo_additions" Prelude.True

a87v11better_lf_max0 =
  T.mkVariable tFrontierDATAFN2 870011 3 0 "better_lf_max0" Prelude.True

a87v27better_lf_min1 =
  T.mkVariable tFrontierDATAFN2 870027 3 0 "better_lf_min1" Prelude.True

a90v11lofact = T.mkVariable tFrontierDATAFN2 900011 3 0 "lofact" Prelude.True

a90v19lofact_memo_additions =
  T.mkVariable tFrontierDATAFN2 900019 3 0 "lofact_memo_additions" Prelude.True

a94v10useful_lofact_memo =
  T.mkVariable tFrontierDATAFN2 940010 3 0 "useful_lofact_memo" Prelude.True

a98v10min1_lofact =
  T.mkVariable tFrontierDATAFN2 980010 3 0 "min1_lofact" Prelude.True

a101v11hifacts =
  T.mkVariable tFrontierDATAFN2 1010011 3 0 "hifacts" Prelude.True

a101v20hifact_memo_additions =
  T.mkVariable tFrontierDATAFN2 1010020 3 0 "hifact_memo_additions" Prelude.True

a114v11better_lf_max0 =
  T.mkVariable tFrontierDATAFN2 1140011 3 0 "better_lf_max0" Prelude.True

a114v27better_lf_min1 =
  T.mkVariable tFrontierDATAFN2 1140027 3 0 "better_lf_min1" Prelude.True

a117v11lofact = T.mkVariable tFrontierDATAFN2 1170011 3 0 "lofact" Prelude.True

a117v19lofact_memo_additions =
  T.mkVariable tFrontierDATAFN2 1170019 3 0 "lofact_memo_additions" Prelude.True

a121v10useful_lofact_memo =
  T.mkVariable tFrontierDATAFN2 1210010 3 0 "useful_lofact_memo" Prelude.True

a125v10min1_lofact =
  T.mkVariable tFrontierDATAFN2 1250010 3 0 "min1_lofact" Prelude.True

a128v11better_mf_max0 =
  T.mkVariable tFrontierDATAFN2 1280011 3 0 "better_mf_max0" Prelude.True

a128v27better_mf_min1 =
  T.mkVariable tFrontierDATAFN2 1280027 3 0 "better_mf_min1" Prelude.True

a131v11midfact =
  T.mkVariable tFrontierDATAFN2 1310011 3 0 "midfact" Prelude.True

a131v20midfact_memo_additions =
  T.mkVariable tFrontierDATAFN2 1310020 3 0 "midfact_memo_additions"
    Prelude.True

a135v10useful_midfact_memo =
  T.mkVariable tFrontierDATAFN2 1350010 3 0 "useful_midfact_memo" Prelude.True

a139v10min1_midfact =
  T.mkVariable tFrontierDATAFN2 1390010 3 0 "min1_midfact" Prelude.True

a142v11hifacts =
  T.mkVariable tFrontierDATAFN2 1420011 3 0 "hifacts" Prelude.True

a142v20hifact_memo_additions =
  T.mkVariable tFrontierDATAFN2 1420020 3 0 "hifact_memo_additions" Prelude.True

a170v10high_coerce =
  T.mkVariable tFrontierDATAFN2 1700010 3 0 "high_coerce" Prelude.True

a172v10small_hifact_domains =
  T.mkVariable tFrontierDATAFN2 1720010 3 0 "small_hifact_domains" Prelude.True

a174v10hifact_info_tuples =
  T.mkVariable tFrontierDATAFN2 1740010 3 0 "hifact_info_tuples" Prelude.True

a177v10mkTuple =
  T.mkVariable tFrontierDATAFN2 1770010 3 4 "mkTuple" Prelude.True

a180v11hf_memo_additions =
  T.mkVariable tFrontierDATAFN2 1800011 3 0 "hf_memo_additions" Prelude.True

a180v30hifacts =
  T.mkVariable tFrontierDATAFN2 1800030 3 0 "hifacts" Prelude.True

a183v10doOne = T.mkVariable tFrontierDATAFN2 1830010 3 2 "doOne" Prelude.True

a184v20rep = T.mkVariable tFrontierDATAFN2 1840020 3 0 "rep" Prelude.True

a184v25more_memo_additions =
  T.mkVariable tFrontierDATAFN2 1840025 3 0 "more_memo_additions" Prelude.True

a237v10initial_yy =
  T.mkVariable tFrontierDATAFN2 2370010 3 0 "initial_yy" Prelude.True

a239v10initial_xx =
  T.mkVariable tFrontierDATAFN2 2390010 3 0 "initial_xx" Prelude.True

a241v11final_yy =
  T.mkVariable tFrontierDATAFN2 2410011 3 0 "final_yy" Prelude.True

a241v21final_xx =
  T.mkVariable tFrontierDATAFN2 2410021 3 0 "final_xx" Prelude.True

a241v31finalMemo =
  T.mkVariable tFrontierDATAFN2 2410031 3 0 "finalMemo" Prelude.True

a244v10result = T.mkVariable tFrontierDATAFN2 2440010 3 0 "result" Prelude.True

a273v10edgez = T.mkVariable tFrontierDATAFN2 2730010 3 0 "edgez" Prelude.True

a275v23args = T.mkVariable tFrontierDATAFN2 2750023 3 0 "args" Prelude.True

a277v10args_at_proper_sizes =
  T.mkVariable tFrontierDATAFN2 2770010 3 0 "args_at_proper_sizes" Prelude.True

a279v10evald_app =
  T.mkVariable tFrontierDATAFN2 2790010 3 0 "evald_app" Prelude.True

a281v10coerced_evald_app =
  T.mkVariable tFrontierDATAFN2 2810010 3 0 "coerced_evald_app" Prelude.True

a283v10revised_max_yy =
  T.mkVariable tFrontierDATAFN2 2830010 3 0 "revised_max_yy" Prelude.True

a285v10revised_min_xx =
  T.mkVariable tFrontierDATAFN2 2850010 3 0 "revised_min_xx" Prelude.True

a287v10new_memo =
  T.mkVariable tFrontierDATAFN2 2870010 3 0 "new_memo" Prelude.True

p34v1 = T.mkSrcPos tFrontierDATAFN2 340001

p36v11 = T.mkSrcPos tFrontierDATAFN2 360011

p36v30 = T.mkSrcPos tFrontierDATAFN2 360030

p37v15 = T.mkSrcPos tFrontierDATAFN2 370015

p37v34 = T.mkSrcPos tFrontierDATAFN2 370034

p37v26 = T.mkSrcPos tFrontierDATAFN2 370026

p37v41 = T.mkSrcPos tFrontierDATAFN2 370041

p37v42 = T.mkSrcPos tFrontierDATAFN2 370042

p38v10 = T.mkSrcPos tFrontierDATAFN2 380010

p39v15 = T.mkSrcPos tFrontierDATAFN2 390015

p39v26 = T.mkSrcPos tFrontierDATAFN2 390026

p39v38 = T.mkSrcPos tFrontierDATAFN2 390038

p39v31 = T.mkSrcPos tFrontierDATAFN2 390031

p39v40 = T.mkSrcPos tFrontierDATAFN2 390040

p39v47 = T.mkSrcPos tFrontierDATAFN2 390047

p40v10 = T.mkSrcPos tFrontierDATAFN2 400010

p41v15 = T.mkSrcPos tFrontierDATAFN2 410015

p41v26 = T.mkSrcPos tFrontierDATAFN2 410026

p41v38 = T.mkSrcPos tFrontierDATAFN2 410038

p41v31 = T.mkSrcPos tFrontierDATAFN2 410031

p41v40 = T.mkSrcPos tFrontierDATAFN2 410040

p41v47 = T.mkSrcPos tFrontierDATAFN2 410047

p42v10 = T.mkSrcPos tFrontierDATAFN2 420010

p44v16 = T.mkSrcPos tFrontierDATAFN2 440016

p43v16 = T.mkSrcPos tFrontierDATAFN2 430016

p43v30 = T.mkSrcPos tFrontierDATAFN2 430030

p43v31 = T.mkSrcPos tFrontierDATAFN2 430031

p43v39 = T.mkSrcPos tFrontierDATAFN2 430039

p43v43 = T.mkSrcPos tFrontierDATAFN2 430043

p43v61 = T.mkSrcPos tFrontierDATAFN2 430061

p45v16 = T.mkSrcPos tFrontierDATAFN2 450016

p45v35 = T.mkSrcPos tFrontierDATAFN2 450035

p46v10 = T.mkSrcPos tFrontierDATAFN2 460010

p48v16 = T.mkSrcPos tFrontierDATAFN2 480016

p47v16 = T.mkSrcPos tFrontierDATAFN2 470016

p47v30 = T.mkSrcPos tFrontierDATAFN2 470030

p47v31 = T.mkSrcPos tFrontierDATAFN2 470031

p47v39 = T.mkSrcPos tFrontierDATAFN2 470039

p47v43 = T.mkSrcPos tFrontierDATAFN2 470043

p47v64 = T.mkSrcPos tFrontierDATAFN2 470064

p49v16 = T.mkSrcPos tFrontierDATAFN2 490016

p49v35 = T.mkSrcPos tFrontierDATAFN2 490035

p51v10 = T.mkSrcPos tFrontierDATAFN2 510010

p51v11 = T.mkSrcPos tFrontierDATAFN2 510011

p51v21 = T.mkSrcPos tFrontierDATAFN2 510021

p68v1 = T.mkSrcPos tFrontierDATAFN2 680001

p72v11 = T.mkSrcPos tFrontierDATAFN2 720011

p72v24 = T.mkSrcPos tFrontierDATAFN2 720024

p73v15 = T.mkSrcPos tFrontierDATAFN2 730015

p74v11 = T.mkSrcPos tFrontierDATAFN2 740011

p74v15 = T.mkSrcPos tFrontierDATAFN2 740015

p75v15 = T.mkSrcPos tFrontierDATAFN2 750015

p76v56 = T.mkSrcPos tFrontierDATAFN2 760056

p77v21 = T.mkSrcPos tFrontierDATAFN2 770021

p79v10 = T.mkSrcPos tFrontierDATAFN2 790010

p79v11 = T.mkSrcPos tFrontierDATAFN2 790011

p79v18 = T.mkSrcPos tFrontierDATAFN2 790018

p80v11 = T.mkSrcPos tFrontierDATAFN2 800011

p87v11 = T.mkSrcPos tFrontierDATAFN2 870011

p87v27 = T.mkSrcPos tFrontierDATAFN2 870027

p88v15 = T.mkSrcPos tFrontierDATAFN2 880015

p88v31 = T.mkSrcPos tFrontierDATAFN2 880031

p88v26 = T.mkSrcPos tFrontierDATAFN2 880026

p90v11 = T.mkSrcPos tFrontierDATAFN2 900011

p90v19 = T.mkSrcPos tFrontierDATAFN2 900019

p91v15 = T.mkSrcPos tFrontierDATAFN2 910015

p92v56 = T.mkSrcPos tFrontierDATAFN2 920056

p93v24 = T.mkSrcPos tFrontierDATAFN2 930024

p93v45 = T.mkSrcPos tFrontierDATAFN2 930045

p93v40 = T.mkSrcPos tFrontierDATAFN2 930040

p94v10 = T.mkSrcPos tFrontierDATAFN2 940010

p95v15 = T.mkSrcPos tFrontierDATAFN2 950015

p95v26 = T.mkSrcPos tFrontierDATAFN2 950026

p95v23 = T.mkSrcPos tFrontierDATAFN2 950023

p95v35 = T.mkSrcPos tFrontierDATAFN2 950035

p95v27 = T.mkSrcPos tFrontierDATAFN2 950027

p95v41 = T.mkSrcPos tFrontierDATAFN2 950041

p95v36 = T.mkSrcPos tFrontierDATAFN2 950036

p95v48 = T.mkSrcPos tFrontierDATAFN2 950048

p95v49 = T.mkSrcPos tFrontierDATAFN2 950049

p96v45 = T.mkSrcPos tFrontierDATAFN2 960045

p96v23 = T.mkSrcPos tFrontierDATAFN2 960023

p98v10 = T.mkSrcPos tFrontierDATAFN2 980010

p99v15 = T.mkSrcPos tFrontierDATAFN2 990015

p99v20 = T.mkSrcPos tFrontierDATAFN2 990020

p99v61 = T.mkSrcPos tFrontierDATAFN2 990061

p101v11 = T.mkSrcPos tFrontierDATAFN2 1010011

p101v20 = T.mkSrcPos tFrontierDATAFN2 1010020

p102v15 = T.mkSrcPos tFrontierDATAFN2 1020015

p103v26 = T.mkSrcPos tFrontierDATAFN2 1030026

p103v45 = T.mkSrcPos tFrontierDATAFN2 1030045

p103v51 = T.mkSrcPos tFrontierDATAFN2 1030051

p105v10 = T.mkSrcPos tFrontierDATAFN2 1050010

p105v11 = T.mkSrcPos tFrontierDATAFN2 1050011

p105v16 = T.mkSrcPos tFrontierDATAFN2 1050016

p105v23 = T.mkSrcPos tFrontierDATAFN2 1050023

p106v33 = T.mkSrcPos tFrontierDATAFN2 1060033

p106v11 = T.mkSrcPos tFrontierDATAFN2 1060011

p106v36 = T.mkSrcPos tFrontierDATAFN2 1060036

p114v11 = T.mkSrcPos tFrontierDATAFN2 1140011

p114v27 = T.mkSrcPos tFrontierDATAFN2 1140027

p115v15 = T.mkSrcPos tFrontierDATAFN2 1150015

p115v31 = T.mkSrcPos tFrontierDATAFN2 1150031

p115v26 = T.mkSrcPos tFrontierDATAFN2 1150026

p117v11 = T.mkSrcPos tFrontierDATAFN2 1170011

p117v19 = T.mkSrcPos tFrontierDATAFN2 1170019

p118v15 = T.mkSrcPos tFrontierDATAFN2 1180015

p119v56 = T.mkSrcPos tFrontierDATAFN2 1190056

p120v42 = T.mkSrcPos tFrontierDATAFN2 1200042

p120v63 = T.mkSrcPos tFrontierDATAFN2 1200063

p120v58 = T.mkSrcPos tFrontierDATAFN2 1200058

p121v10 = T.mkSrcPos tFrontierDATAFN2 1210010

p122v15 = T.mkSrcPos tFrontierDATAFN2 1220015

p122v26 = T.mkSrcPos tFrontierDATAFN2 1220026

p122v23 = T.mkSrcPos tFrontierDATAFN2 1220023

p122v35 = T.mkSrcPos tFrontierDATAFN2 1220035

p122v27 = T.mkSrcPos tFrontierDATAFN2 1220027

p122v41 = T.mkSrcPos tFrontierDATAFN2 1220041

p122v36 = T.mkSrcPos tFrontierDATAFN2 1220036

p122v48 = T.mkSrcPos tFrontierDATAFN2 1220048

p122v49 = T.mkSrcPos tFrontierDATAFN2 1220049

p123v45 = T.mkSrcPos tFrontierDATAFN2 1230045

p123v23 = T.mkSrcPos tFrontierDATAFN2 1230023

p125v10 = T.mkSrcPos tFrontierDATAFN2 1250010

p126v15 = T.mkSrcPos tFrontierDATAFN2 1260015

p126v20 = T.mkSrcPos tFrontierDATAFN2 1260020

p126v61 = T.mkSrcPos tFrontierDATAFN2 1260061

p128v11 = T.mkSrcPos tFrontierDATAFN2 1280011

p128v27 = T.mkSrcPos tFrontierDATAFN2 1280027

p129v15 = T.mkSrcPos tFrontierDATAFN2 1290015

p129v32 = T.mkSrcPos tFrontierDATAFN2 1290032

p129v26 = T.mkSrcPos tFrontierDATAFN2 1290026

p129v41 = T.mkSrcPos tFrontierDATAFN2 1290041

p131v11 = T.mkSrcPos tFrontierDATAFN2 1310011

p131v20 = T.mkSrcPos tFrontierDATAFN2 1310020

p132v15 = T.mkSrcPos tFrontierDATAFN2 1320015

p133v59 = T.mkSrcPos tFrontierDATAFN2 1330059

p134v43 = T.mkSrcPos tFrontierDATAFN2 1340043

p134v65 = T.mkSrcPos tFrontierDATAFN2 1340065

p134v59 = T.mkSrcPos tFrontierDATAFN2 1340059

p135v10 = T.mkSrcPos tFrontierDATAFN2 1350010

p136v15 = T.mkSrcPos tFrontierDATAFN2 1360015

p136v26 = T.mkSrcPos tFrontierDATAFN2 1360026

p136v23 = T.mkSrcPos tFrontierDATAFN2 1360023

p136v35 = T.mkSrcPos tFrontierDATAFN2 1360035

p136v27 = T.mkSrcPos tFrontierDATAFN2 1360027

p136v42 = T.mkSrcPos tFrontierDATAFN2 1360042

p136v36 = T.mkSrcPos tFrontierDATAFN2 1360036

p136v49 = T.mkSrcPos tFrontierDATAFN2 1360049

p136v50 = T.mkSrcPos tFrontierDATAFN2 1360050

p137v46 = T.mkSrcPos tFrontierDATAFN2 1370046

p137v23 = T.mkSrcPos tFrontierDATAFN2 1370023

p137v49 = T.mkSrcPos tFrontierDATAFN2 1370049

p139v10 = T.mkSrcPos tFrontierDATAFN2 1390010

p140v15 = T.mkSrcPos tFrontierDATAFN2 1400015

p140v20 = T.mkSrcPos tFrontierDATAFN2 1400020

p140v62 = T.mkSrcPos tFrontierDATAFN2 1400062

p142v11 = T.mkSrcPos tFrontierDATAFN2 1420011

p142v20 = T.mkSrcPos tFrontierDATAFN2 1420020

p143v15 = T.mkSrcPos tFrontierDATAFN2 1430015

p144v26 = T.mkSrcPos tFrontierDATAFN2 1440026

p144v46 = T.mkSrcPos tFrontierDATAFN2 1440046

p144v51 = T.mkSrcPos tFrontierDATAFN2 1440051

p146v10 = T.mkSrcPos tFrontierDATAFN2 1460010

p146v11 = T.mkSrcPos tFrontierDATAFN2 1460011

p146v16 = T.mkSrcPos tFrontierDATAFN2 1460016

p146v23 = T.mkSrcPos tFrontierDATAFN2 1460023

p146v31 = T.mkSrcPos tFrontierDATAFN2 1460031

p147v33 = T.mkSrcPos tFrontierDATAFN2 1470033

p147v11 = T.mkSrcPos tFrontierDATAFN2 1470011

p148v34 = T.mkSrcPos tFrontierDATAFN2 1480034

p148v11 = T.mkSrcPos tFrontierDATAFN2 1480011

p148v37 = T.mkSrcPos tFrontierDATAFN2 1480037

p167v1 = T.mkSrcPos tFrontierDATAFN2 1670001

p170v10 = T.mkSrcPos tFrontierDATAFN2 1700010

p171v15 = T.mkSrcPos tFrontierDATAFN2 1710015

p171v35 = T.mkSrcPos tFrontierDATAFN2 1710035

p171v46 = T.mkSrcPos tFrontierDATAFN2 1710046

p172v10 = T.mkSrcPos tFrontierDATAFN2 1720010

p173v15 = T.mkSrcPos tFrontierDATAFN2 1730015

p173v20 = T.mkSrcPos tFrontierDATAFN2 1730020

p174v10 = T.mkSrcPos tFrontierDATAFN2 1740010

p175v15 = T.mkSrcPos tFrontierDATAFN2 1750015

p175v26 = T.mkSrcPos tFrontierDATAFN2 1750026

p175v38 = T.mkSrcPos tFrontierDATAFN2 1750038

p175v35 = T.mkSrcPos tFrontierDATAFN2 1750035

p175v64 = T.mkSrcPos tFrontierDATAFN2 1750064

p175v53 = T.mkSrcPos tFrontierDATAFN2 1750053

p175v66 = T.mkSrcPos tFrontierDATAFN2 1750066

p176v36 = T.mkSrcPos tFrontierDATAFN2 1760036

p176v40 = T.mkSrcPos tFrontierDATAFN2 1760040

p176v55 = T.mkSrcPos tFrontierDATAFN2 1760055

p177v10 = T.mkSrcPos tFrontierDATAFN2 1770010

p178v15 = T.mkSrcPos tFrontierDATAFN2 1780015

p178v55 = T.mkSrcPos tFrontierDATAFN2 1780055

p178v34 = T.mkSrcPos tFrontierDATAFN2 1780034

p180v11 = T.mkSrcPos tFrontierDATAFN2 1800011

p180v30 = T.mkSrcPos tFrontierDATAFN2 1800030

p181v15 = T.mkSrcPos tFrontierDATAFN2 1810015

p181v25 = T.mkSrcPos tFrontierDATAFN2 1810025

p181v31 = T.mkSrcPos tFrontierDATAFN2 1810031

p181v34 = T.mkSrcPos tFrontierDATAFN2 1810034

p183v10 = T.mkSrcPos tFrontierDATAFN2 1830010

p184v20 = T.mkSrcPos tFrontierDATAFN2 1840020

p184v25 = T.mkSrcPos tFrontierDATAFN2 1840025

p185v24 = T.mkSrcPos tFrontierDATAFN2 1850024

p187v49 = T.mkSrcPos tFrontierDATAFN2 1870049

p188v19 = T.mkSrcPos tFrontierDATAFN2 1880019

p188v40 = T.mkSrcPos tFrontierDATAFN2 1880040

p188v20 = T.mkSrcPos tFrontierDATAFN2 1880020

p188v61 = T.mkSrcPos tFrontierDATAFN2 1880061

p190v10 = T.mkSrcPos tFrontierDATAFN2 1900010

p190v11 = T.mkSrcPos tFrontierDATAFN2 1900011

p190v20 = T.mkSrcPos tFrontierDATAFN2 1900020

p196v1 = T.mkSrcPos tFrontierDATAFN2 1960001

p196v14 = T.mkSrcPos tFrontierDATAFN2 1960014

p199v1 = T.mkSrcPos tFrontierDATAFN2 1990001

p199v19 = T.mkSrcPos tFrontierDATAFN2 1990019

p200v19 = T.mkSrcPos tFrontierDATAFN2 2000019

p203v1 = T.mkSrcPos tFrontierDATAFN2 2030001

p203v28 = T.mkSrcPos tFrontierDATAFN2 2030028

p204v31 = T.mkSrcPos tFrontierDATAFN2 2040031

p207v1 = T.mkSrcPos tFrontierDATAFN2 2070001

p207v21 = T.mkSrcPos tFrontierDATAFN2 2070021

p208v21 = T.mkSrcPos tFrontierDATAFN2 2080021

p209v21 = T.mkSrcPos tFrontierDATAFN2 2090021

p212v1 = T.mkSrcPos tFrontierDATAFN2 2120001

p212v22 = T.mkSrcPos tFrontierDATAFN2 2120022

p213v22 = T.mkSrcPos tFrontierDATAFN2 2130022

p214v22 = T.mkSrcPos tFrontierDATAFN2 2140022

p217v1 = T.mkSrcPos tFrontierDATAFN2 2170001

p217v30 = T.mkSrcPos tFrontierDATAFN2 2170030

p218v30 = T.mkSrcPos tFrontierDATAFN2 2180030

p219v33 = T.mkSrcPos tFrontierDATAFN2 2190033

p222v1 = T.mkSrcPos tFrontierDATAFN2 2220001

p222v14 = T.mkSrcPos tFrontierDATAFN2 2220014

p222v33 = T.mkSrcPos tFrontierDATAFN2 2220033

p222v46 = T.mkSrcPos tFrontierDATAFN2 2220046

p236v1 = T.mkSrcPos tFrontierDATAFN2 2360001

p237v10 = T.mkSrcPos tFrontierDATAFN2 2370010

p238v15 = T.mkSrcPos tFrontierDATAFN2 2380015

p239v10 = T.mkSrcPos tFrontierDATAFN2 2390010

p240v15 = T.mkSrcPos tFrontierDATAFN2 2400015

p241v11 = T.mkSrcPos tFrontierDATAFN2 2410011

p241v21 = T.mkSrcPos tFrontierDATAFN2 2410021

p241v31 = T.mkSrcPos tFrontierDATAFN2 2410031

p242v15 = T.mkSrcPos tFrontierDATAFN2 2420015

p243v24 = T.mkSrcPos tFrontierDATAFN2 2430024

p243v35 = T.mkSrcPos tFrontierDATAFN2 2430035

p243v46 = T.mkSrcPos tFrontierDATAFN2 2430046

p243v51 = T.mkSrcPos tFrontierDATAFN2 2430051

p243v55 = T.mkSrcPos tFrontierDATAFN2 2430055

p243v68 = T.mkSrcPos tFrontierDATAFN2 2430068

p243v70 = T.mkSrcPos tFrontierDATAFN2 2430070

p244v10 = T.mkSrcPos tFrontierDATAFN2 2440010

p245v15 = T.mkSrcPos tFrontierDATAFN2 2450015

p245v25 = T.mkSrcPos tFrontierDATAFN2 2450025

p245v45 = T.mkSrcPos tFrontierDATAFN2 2450045

p245v54 = T.mkSrcPos tFrontierDATAFN2 2450054

p250v10 = T.mkSrcPos tFrontierDATAFN2 2500010

p250v11 = T.mkSrcPos tFrontierDATAFN2 2500011

p250v19 = T.mkSrcPos tFrontierDATAFN2 2500019

p271v1 = T.mkSrcPos tFrontierDATAFN2 2710001

p273v10 = T.mkSrcPos tFrontierDATAFN2 2730010

p274v15 = T.mkSrcPos tFrontierDATAFN2 2740015

p274v25 = T.mkSrcPos tFrontierDATAFN2 2740025

p275v23 = T.mkSrcPos tFrontierDATAFN2 2750023

p276v15 = T.mkSrcPos tFrontierDATAFN2 2760015

p277v10 = T.mkSrcPos tFrontierDATAFN2 2770010

p278v15 = T.mkSrcPos tFrontierDATAFN2 2780015

p278v27 = T.mkSrcPos tFrontierDATAFN2 2780027

p278v64 = T.mkSrcPos tFrontierDATAFN2 2780064

p279v10 = T.mkSrcPos tFrontierDATAFN2 2790010

p280v15 = T.mkSrcPos tFrontierDATAFN2 2800015

p280v34 = T.mkSrcPos tFrontierDATAFN2 2800034

p280v38 = T.mkSrcPos tFrontierDATAFN2 2800038

p280v45 = T.mkSrcPos tFrontierDATAFN2 2800045

p281v10 = T.mkSrcPos tFrontierDATAFN2 2810010

p282v15 = T.mkSrcPos tFrontierDATAFN2 2820015

p282v22 = T.mkSrcPos tFrontierDATAFN2 2820022

p283v10 = T.mkSrcPos tFrontierDATAFN2 2830010

p284v15 = T.mkSrcPos tFrontierDATAFN2 2840015

p284v55 = T.mkSrcPos tFrontierDATAFN2 2840055

p284v62 = T.mkSrcPos tFrontierDATAFN2 2840062

p285v10 = T.mkSrcPos tFrontierDATAFN2 2850010

p286v15 = T.mkSrcPos tFrontierDATAFN2 2860015

p286v55 = T.mkSrcPos tFrontierDATAFN2 2860055

p286v62 = T.mkSrcPos tFrontierDATAFN2 2860062

p287v10 = T.mkSrcPos tFrontierDATAFN2 2870010

p288v33 = T.mkSrcPos tFrontierDATAFN2 2880033

p288v15 = T.mkSrcPos tFrontierDATAFN2 2880015

p288v16 = T.mkSrcPos tFrontierDATAFN2 2880016

p288v22 = T.mkSrcPos tFrontierDATAFN2 2880022

p290v10 = T.mkSrcPos tFrontierDATAFN2 2900010

p290v18 = T.mkSrcPos tFrontierDATAFN2 2900018

p290v30 = T.mkSrcPos tFrontierDATAFN2 2900030

p291v18 = T.mkSrcPos tFrontierDATAFN2 2910018

p291v19 = T.mkSrcPos tFrontierDATAFN2 2910019

p291v38 = T.mkSrcPos tFrontierDATAFN2 2910038

p293v10 = T.mkSrcPos tFrontierDATAFN2 2930010

p293v36 = T.mkSrcPos tFrontierDATAFN2 2930036

p293v18 = T.mkSrcPos tFrontierDATAFN2 2930018

p293v39 = T.mkSrcPos tFrontierDATAFN2 2930039

p294v18 = T.mkSrcPos tFrontierDATAFN2 2940018

p296v27 = T.mkSrcPos tFrontierDATAFN2 2960027

p296v55 = T.mkSrcPos tFrontierDATAFN2 2960055

p296v61 = T.mkSrcPos tFrontierDATAFN2 2960061

p296v71 = T.mkSrcPos tFrontierDATAFN2 2960071

p298v10 = T.mkSrcPos tFrontierDATAFN2 2980010

p298v36 = T.mkSrcPos tFrontierDATAFN2 2980036

p298v18 = T.mkSrcPos tFrontierDATAFN2 2980018

p298v39 = T.mkSrcPos tFrontierDATAFN2 2980039

p299v18 = T.mkSrcPos tFrontierDATAFN2 2990018

p301v40 = T.mkSrcPos tFrontierDATAFN2 3010040

p301v55 = T.mkSrcPos tFrontierDATAFN2 3010055

p301v60 = T.mkSrcPos tFrontierDATAFN2 3010060

p301v70 = T.mkSrcPos tFrontierDATAFN2 3010070

p302v18 = T.mkSrcPos tFrontierDATAFN2 3020018

p302v24 = T.mkSrcPos tFrontierDATAFN2 3020024
