module TBaseDefs
  (AList(),DefnGroup(),ST(),ATree(..),Reply(..),NameSupply(),Oseq(),Iseq()
    ,Set(..),Bag(),Flag(..),gbdDefaultSettings,gbdDryRunSettings,SAInfo(..)
    ,ExceptionInt(..),DomainInt(),DInt(),OneFuncSize(),Sequence(),Naam(),Alter()
    ,AlterP(),ScValue(),ScValueP(),CoreProgram(),CoreProgramP(),AtomicProgram()
    ,TypeDef(),ConstrAlt(),TDefExpr(..),CExpr(),CExprP(..),AnnExpr()
    ,AnnExpr'(..),AnnDefn(),AnnAlt(),AnnProgram(),Eqn(..),TVName(),Message()
    ,TExpr(..),TypeScheme(..),Subst(),TcTypeEnv(),TypeEnv(),TypeNameSupply()
    ,TypeInfo(),TypeDependancy(),Point(),FrontierElem(..),Frontier(..)
    ,Domain(..),Route(..),Rep(..),DExpr(..),RSubst(),DSubst(),DRRSubst()
    ,DExprEnv(),ConstrElem(..),ACMode(..),MemoList(),AppInfo(..),HExpr(..)
    ,PrPoint(),PrDomain(),Token(),PResult(..),Parser(),PartialExpr(..)
    ,StaticComponent(),aALeaf,aABranch,aOk,aFail,aMkSet,aTypecheck,aSimp
    ,aNoCaseOpt,aShowHExpr,aNoPretty,aNoFormat,aNoBaraki,aSimpleInv,aPolyLim
    ,aMonoLim,aForceAll,aDryRun,aLowerLim,aUpperLim,aScaleUp,aSAResult,aSASearch
    ,aSASizes,aSAHExpr,aSASL,aSAGiveUp,aMkExInt,aTDefVar,aTDefCons,aEVar,aENum
    ,aEConstr,aEAp,aELet,aECase,aELam,aAVar,aANum,aAConstr,aAAp,aALet,aACase
    ,aALam,aEqnNVC,aTVar,aTArr,aTCons,aScheme,aMkFrel,aMin1Max0,aTwo,aLift1
    ,aLift2,aFunc,aZero,aOne,aStop1,aUp1,aStop2,aUp2,aUpUp2,aRep,aRepTwo,aRep1
    ,aRep2,aDXTwo,aDXLift1,aDXLift2,aDXFunc,aDXVar,aConstrRec,aConstrVar,aSafe
    ,aLive,aA2,aALo1,aAHi1,aALo2,aAMid2,aAHi2,aHApp,aHVAp,aHLam,aHVar,aHMeet
    ,aHPoint,aHTable,aPFail,aPOk,aNoOp,aFoundOp) where

import qualified Prelude 
import qualified Hat as T 
import qualified TPreludeBasic 
import TPrelude 

type AList a b = T.List (T.Tuple2 a b)

type DefnGroup a = T.List (T.Tuple2 Bool (T.List a))

type ST a b = T.Fun b (T.Tuple2 a b)

data ATree a b =
  ALeaf  | ABranch (T.R (ATree a b)) (T.R a) (T.R b) (T.R (ATree a b)) (T.R Int)

instance T.WrapVal (ATree a b)
  where
  
  T.wrapVal pwrapVal (kwrapVal@ALeaf) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aALeaf)
  T.wrapVal pwrapVal
    (kwrapVal@(ABranch (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal)
        (T.R _ z4wrapVal) (T.R _ z5wrapVal))) p =
    T.R kwrapVal
      (T.mkValueApp5 p pwrapVal aABranch z1wrapVal z2wrapVal z3wrapVal z4wrapVal
        z5wrapVal)
  

instance (Eq a,Eq b) => Eq (ATree a b)
  where
  
  (!==) (%==) p =
    T.ufun2 (+$!=$@==) (%==) p (*==)
    where
    
    (*==) (T.R ALeaf _) (T.R ALeaf _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R (ABranch fy1 fy2 fy3 fy4 fy5) _)
      (T.R (ABranch fy6 fy7 fy8 fy9 fy10) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy6)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy7)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy3 fy8)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy4 fy9)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy5 fy10))))
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

data Reply a b = Ok (T.R a) | Fail (T.R b)

instance T.WrapVal (Reply a b)
  where
  
  T.wrapVal pwrapVal (kwrapVal@(Ok (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aOk z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(Fail (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aFail z1wrapVal)
  

instance (Eq a,Eq b) => Eq (Reply a b)
  where
  
  (!==) (%==) p =
    T.ufun2 (+$@=$@==) (%==) p (*==)
    where
    
    (*==) (T.R (Ok fy1) _) (T.R (Ok fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (Fail fy1) _) (T.R (Fail fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

type NameSupply = Int

type Oseq = T.Fun Int (T.Fun Int (T.List Char))

type Iseq = T.Fun Oseq Oseq

data Set a = MkSet (T.R (T.List a))

instance T.WrapVal (Set a)
  where
  
  T.wrapVal pwrapVal (kwrapVal@(MkSet (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aMkSet z1wrapVal)
  

instance Eq a => Eq (Set a)
  where
  
  (!==) (%==) p =
    T.ufun2 (+&$=$&==) (%==) p (*==)
    where
    
    (*==) (T.R (MkSet fy1) _) (T.R (MkSet fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

type Bag a = T.List a

data Flag =
  Typecheck  | Simp  | NoCaseOpt  | ShowHExpr  | NoPretty  | NoFormat 
  | NoBaraki  | SimpleInv  | PolyLim (T.R Int) | MonoLim (T.R Int) | ForceAll 
  | DryRun  | LowerLim (T.R Int) | UpperLim (T.R Int) | ScaleUp (T.R Int)

instance T.WrapVal Flag
  where
  
  T.wrapVal pwrapVal (kwrapVal@Typecheck) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aTypecheck)
  T.wrapVal pwrapVal (kwrapVal@Simp) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aSimp)
  T.wrapVal pwrapVal (kwrapVal@NoCaseOpt) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aNoCaseOpt)
  T.wrapVal pwrapVal (kwrapVal@ShowHExpr) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aShowHExpr)
  T.wrapVal pwrapVal (kwrapVal@NoPretty) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aNoPretty)
  T.wrapVal pwrapVal (kwrapVal@NoFormat) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aNoFormat)
  T.wrapVal pwrapVal (kwrapVal@NoBaraki) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aNoBaraki)
  T.wrapVal pwrapVal (kwrapVal@SimpleInv) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aSimpleInv)
  T.wrapVal pwrapVal (kwrapVal@(PolyLim (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aPolyLim z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(MonoLim (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aMonoLim z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@ForceAll) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aForceAll)
  T.wrapVal pwrapVal (kwrapVal@DryRun) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aDryRun)
  T.wrapVal pwrapVal (kwrapVal@(LowerLim (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aLowerLim z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(UpperLim (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aUpperLim z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(ScaleUp (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aScaleUp z1wrapVal)
  

instance Eq Flag
  where
  
  (!==) (%==) p =
    T.ufun2 (+++=$%==) (%==) p (*==)
    where
    
    (*==) (T.R Typecheck _) (T.R Typecheck _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R Simp _) (T.R Simp _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R NoCaseOpt _) (T.R NoCaseOpt _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R ShowHExpr _) (T.R ShowHExpr _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R NoPretty _) (T.R NoPretty _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R NoFormat _) (T.R NoFormat _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R NoBaraki _) (T.R NoBaraki _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R SimpleInv _) (T.R SimpleInv _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R (PolyLim fy1) _) (T.R (PolyLim fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (MonoLim fy1) _) (T.R (MonoLim fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R ForceAll _) (T.R ForceAll _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R DryRun _) (T.R DryRun _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R (LowerLim fy1) _) (T.R (LowerLim fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (UpperLim fy1) _) (T.R (UpperLim fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (ScaleUp fy1) _) (T.R (ScaleUp fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

gbdDefaultSettings pbdDefaultSettings p =
  T.constUse pbdDefaultSettings p sbdDefaultSettings

sbdDefaultSettings =
  T.constDef T.mkRoot abdDefaultSettings
    (\ p ->
      T.fromExpList p69v6 p
        [T.con1 p69v7 p PolyLim aPolyLim
            (T.ap1 p69v15 p (TPreludeBasic.gfromInteger p69v15 p)
              (T.conInteger p69v15 p 10000))
          ,T.con1 p69v22 p MonoLim aMonoLim
            (T.ap1 p69v30 p (TPreludeBasic.gfromInteger p69v30 p)
              (T.conInteger p69v30 p 10000))
          ,T.con1 p69v37 p LowerLim aLowerLim
            (T.ap1 p69v46 p (TPreludeBasic.gfromInteger p69v46 p)
              (T.conInteger p69v46 p 0))
          ,T.con1 p69v49 p UpperLim aUpperLim
            (T.ap1 p69v58 p (TPreludeBasic.gfromInteger p69v58 p)
              (T.conInteger p69v58 p 1000000))
          ,T.con1 p69v67 p ScaleUp aScaleUp
            (T.ap1 p69v75 p (TPreludeBasic.gfromInteger p69v75 p)
              (T.conInteger p69v75 p 20))])

gbdDryRunSettings pbdDryRunSettings p =
  T.constUse pbdDryRunSettings p sbdDryRunSettings

sbdDryRunSettings =
  T.constDef T.mkRoot abdDryRunSettings
    (\ p ->
      T.fromExpList p72v6 p
        [T.con0 p72v7 p NoBaraki aNoBaraki
          ,T.con1 p72v17 p LowerLim aLowerLim
            (T.ap1 p72v26 p (TPreludeBasic.gfromInteger p72v26 p)
              (T.conInteger p72v26 p 0))
          ,T.con1 p72v29 p UpperLim aUpperLim
            (T.ap1 p72v38 p (TPreludeBasic.gfromInteger p72v38 p)
              (T.conInteger p72v38 p 0))
          ,T.con1 p72v41 p PolyLim aPolyLim
            (T.ap1 p72v49 p (TPreludeBasic.gfromInteger p72v49 p)
              (T.conInteger p72v49 p 1))
          ,T.con1 p72v52 p MonoLim aMonoLim
            (T.ap1 p72v60 p (TPreludeBasic.gfromInteger p72v60 p)
              (T.conInteger p72v60 p 1))
          ,T.con1 p72v63 p ScaleUp aScaleUp
            (T.ap1 p72v71 p (TPreludeBasic.gfromInteger p72v71 p)
              (T.conInteger p72v71 p 20))])

data SAInfo =
  SAResult (T.R String) (T.R Domain) (T.R Route)
  | SASearch (T.R ACMode) (T.R String) (T.R Int) (T.R Int)
  | SASizes (T.R String) (T.R (T.List OneFuncSize)) (T.R (T.List OneFuncSize))
  | SAHExpr (T.R String) (T.R (HExpr Naam))
  | SASL (T.R (T.List Route)) (T.R (T.List Route))
  | SAGiveUp (T.R (T.List String))

instance T.WrapVal SAInfo
  where
  
  T.wrapVal pwrapVal
    (kwrapVal@(SAResult (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal)))
    p =
    T.R kwrapVal
      (T.mkValueApp3 p pwrapVal aSAResult z1wrapVal z2wrapVal z3wrapVal)
  T.wrapVal pwrapVal
    (kwrapVal@(SASearch (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal)
        (T.R _ z4wrapVal))) p =
    T.R kwrapVal
      (T.mkValueApp4 p pwrapVal aSASearch z1wrapVal z2wrapVal z3wrapVal
        z4wrapVal)
  T.wrapVal pwrapVal
    (kwrapVal@(SASizes (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal)))
    p =
    T.R kwrapVal
      (T.mkValueApp3 p pwrapVal aSASizes z1wrapVal z2wrapVal z3wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(SAHExpr (T.R _ z1wrapVal) (T.R _ z2wrapVal)))
    p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aSAHExpr z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(SASL (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aSASL z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(SAGiveUp (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aSAGiveUp z1wrapVal)
  

data ExceptionInt a = MkExInt (T.R Int) (T.R (T.List a))

instance T.WrapVal (ExceptionInt a)
  where
  
  T.wrapVal pwrapVal (kwrapVal@(MkExInt (T.R _ z1wrapVal) (T.R _ z2wrapVal)))
    p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aMkExInt z1wrapVal z2wrapVal)
  

instance Eq a => Eq (ExceptionInt a)
  where
  
  (!==) (%==) p =
    T.ufun2 (+>&=%%==) (%==) p (*==)
    where
    
    (*==) (T.R (MkExInt fy1 fy2) _) (T.R (MkExInt fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

instance Ord a => Ord (ExceptionInt a)
  where
  
  gcompare pcompare p =
    T.ufun2 a94v37compare pcompare p hcompare
    where
    
    hcompare (T.R (MkExInt fy3 fy4) _) (T.R (MkExInt fy5 fy6) _) p =
      T.uccase T.mkNoSrcPos p
        (let
          v94v37v1 (T.R TPrelude.EQ _) p =
            T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy4 fy6
          v94v37v1 fy1 p = T.projection T.mkNoSrcPos p fy1 in (v94v37v1))
        (T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy5)
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy1
          :: T.R TPrelude.Int)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy2)
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a94v37localFromEnum plocalFromEnum p hlocalFromEnum
        where
        
        hlocalFromEnum (T.R (MkExInt _ _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0)
        hlocalFromEnum _ p = T.fatal p
        
      
    
  

instance Show a => Show (ExceptionInt a)
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a94v42showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (MkExInt fy2 fy3) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "MkExInt "))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
              (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)) fy2)
              (T.uap1 T.mkNoSrcPos p (TPrelude.gshowChar T.mkNoSrcPos p)
                (T.conChar T.mkNoSrcPos p ' ')))
            (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy3)))
    hshowsPrec _ _ p = T.fatal p
    
  

instance (Show a,Ord a) => Num (ExceptionInt a)
  where
  
  (!+) (%+) p =
    T.fun2 (+>>=$#+) (%+) p (*+)
    where
    
    (*+) (T.R (MkExInt fi1 fxs1) _) (T.R (MkExInt fi2 fxs2) _) p =
      T.con2 p100v9 p MkExInt aMkExInt (T.ap2 p100v21 p (p100v21 !+ p) fi1 fi2)
        (T.ap2 p100v32 p (p100v32 !++ p) fxs1 fxs2)
    (*+) _ _ p = T.fatal p
    
  
  (!*) (%*) p =
    T.fun2 (+#!$=$#*) (%*) p (**)
    where
    
    (**) (T.R (MkExInt fi1 fxs1) _) (T.R (MkExInt fi2 fxs2) _) p =
      T.con2 p103v9 p MkExInt aMkExInt (T.ap2 p103v21 p (p103v21 !* p) fi1 fi2)
        (T.ap2 p103v32 p (p103v32 !++ p) fxs1 fxs2)
    (**) _ _ p = T.fatal p
    
  

type DomainInt = ExceptionInt Domain

type DInt = T.Tuple2 Domain Int

type OneFuncSize = T.Tuple2 Int (T.List Domain)

type Sequence =
  T.Tuple2 (T.List (T.List OneFuncSize)) (T.List (T.List OneFuncSize))

type Naam = T.List Char

type Alter = AlterP Naam

type AlterP a = T.Tuple2 (T.List a) (CExprP a)

type ScValue = ScValueP Naam

type ScValueP a = T.Tuple2 (T.List a) (CExprP a)

type CoreProgram = CoreProgramP Naam

type CoreProgramP a =
  T.Tuple2 (T.List TypeDef) (T.List (T.Tuple2 Naam (ScValueP a)))

type AtomicProgram = T.Tuple2 (T.List TypeDef) CExpr

type TypeDef = T.Tuple3 Naam (T.List Naam) (T.List ConstrAlt)

type ConstrAlt = T.Tuple2 Naam (T.List TDefExpr)

data TDefExpr = TDefVar (T.R Naam) | TDefCons (T.R Naam) (T.R (T.List TDefExpr))

instance T.WrapVal TDefExpr
  where
  
  T.wrapVal pwrapVal (kwrapVal@(TDefVar (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aTDefVar z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(TDefCons (T.R _ z1wrapVal) (T.R _ z2wrapVal)))
    p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aTDefCons z1wrapVal z2wrapVal)
  

instance Eq TDefExpr
  where
  
  (!==) (%==) p =
    T.ufun2 (+#&@=$>==) (%==) p (*==)
    where
    
    (*==) (T.R (TDefVar fy1) _) (T.R (TDefVar fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (TDefCons fy1 fy2) _) (T.R (TDefCons fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

type CExpr = CExprP Naam

data CExprP a =
  EVar (T.R Naam) | ENum (T.R Int) | EConstr (T.R Naam)
  | EAp (T.R (CExprP a)) (T.R (CExprP a))
  | ELet (T.R Bool) (T.R (T.List (T.Tuple2 a (CExprP a)))) (T.R (CExprP a))
  | ECase (T.R (CExprP a)) (T.R (T.List (T.Tuple2 Naam (AlterP a))))
  | ELam (T.R (T.List a)) (T.R (CExprP a))

instance T.WrapVal (CExprP a)
  where
  
  T.wrapVal pwrapVal (kwrapVal@(EVar (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aEVar z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(ENum (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aENum z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(EConstr (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aEConstr z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(EAp (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aEAp z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal
    (kwrapVal@(ELet (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal))) p =
    T.R kwrapVal (T.mkValueApp3 p pwrapVal aELet z1wrapVal z2wrapVal z3wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(ECase (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aECase z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(ELam (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aELam z1wrapVal z2wrapVal)
  

instance Eq a => Eq (CExprP a)
  where
  
  (!==) (%==) p =
    T.ufun2 (+#^$=$+==) (%==) p (*==)
    where
    
    (*==) (T.R (EVar fy1) _) (T.R (EVar fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (ENum fy1) _) (T.R (ENum fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (EConstr fy1) _) (T.R (EConstr fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (EAp fy1 fy2) _) (T.R (EAp fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) (T.R (ELet fy1 fy2 fy3) _) (T.R (ELet fy4 fy5 fy6) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy4)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy5)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy3 fy6))
    (*==) (T.R (ECase fy1 fy2) _) (T.R (ECase fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) (T.R (ELam fy1 fy2) _) (T.R (ELam fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

type AnnExpr a b = T.Tuple2 b (AnnExpr' a b)

data AnnExpr' a b =
  AVar (T.R Naam) | ANum (T.R Int) | AConstr (T.R Naam)
  | AAp (T.R (AnnExpr a b)) (T.R (AnnExpr a b))
  | ALet (T.R Bool) (T.R (T.List (AnnDefn a b))) (T.R (AnnExpr a b))
  | ACase (T.R (AnnExpr a b)) (T.R (T.List (AnnAlt a b)))
  | ALam (T.R (T.List a)) (T.R (AnnExpr a b))

instance T.WrapVal (AnnExpr' a b)
  where
  
  T.wrapVal pwrapVal (kwrapVal@(AVar (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aAVar z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(ANum (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aANum z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(AConstr (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aAConstr z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(AAp (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aAAp z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal
    (kwrapVal@(ALet (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal))) p =
    T.R kwrapVal (T.mkValueApp3 p pwrapVal aALet z1wrapVal z2wrapVal z3wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(ACase (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aACase z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(ALam (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aALam z1wrapVal z2wrapVal)
  

instance (Eq a,Eq b) => Eq (AnnExpr' a b)
  where
  
  (!==) (%==) p =
    T.ufun2 (+#@>=$#==) (%==) p (*==)
    where
    
    (*==) (T.R (AVar fy1) _) (T.R (AVar fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (ANum fy1) _) (T.R (ANum fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (AConstr fy1) _) (T.R (AConstr fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (AAp fy1 fy2) _) (T.R (AAp fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) (T.R (ALet fy1 fy2 fy3) _) (T.R (ALet fy4 fy5 fy6) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy4)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy5)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy3 fy6))
    (*==) (T.R (ACase fy1 fy2) _) (T.R (ACase fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) (T.R (ALam fy1 fy2) _) (T.R (ALam fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

type AnnDefn a b = T.Tuple2 a (AnnExpr a b)

type AnnAlt a b = T.Tuple2 Naam (T.Tuple2 (T.List a) (AnnExpr a b))

type AnnProgram a b = T.List (T.Tuple3 Naam (T.List a) (AnnExpr a b))

data Eqn = EqnNVC (T.R Naam) (T.R (Set Naam)) (T.R (Set Naam))

instance T.WrapVal Eqn
  where
  
  T.wrapVal pwrapVal
    (kwrapVal@(EqnNVC (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal)))
    p =
    T.R kwrapVal
      (T.mkValueApp3 p pwrapVal aEqnNVC z1wrapVal z2wrapVal z3wrapVal)
  

instance Eq Eqn
  where
  
  (!==) (%==) p =
    T.ufun2 (+$!%=$$==) (%==) p (*==)
    where
    
    (*==) (T.R (EqnNVC fy1 fy2 fy3) _) (T.R (EqnNVC fy4 fy5 fy6) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy4)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy5)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy3 fy6))
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

type TVName = T.Tuple2 (T.List Int) (T.List Int)

type Message = T.List Char

data TExpr =
  TVar (T.R TVName) | TArr (T.R TExpr) (T.R TExpr)
  | TCons (T.R (T.List Char)) (T.R (T.List TExpr))

instance T.WrapVal TExpr
  where
  
  T.wrapVal pwrapVal (kwrapVal@(TVar (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aTVar z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(TArr (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aTArr z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(TCons (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aTCons z1wrapVal z2wrapVal)
  

instance Eq TExpr
  where
  
  (!==) (%==) p =
    T.ufun2 (+$#^=$&==) (%==) p (*==)
    where
    
    (*==) (T.R (TVar fy1) _) (T.R (TVar fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (TArr fy1 fy2) _) (T.R (TArr fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) (T.R (TCons fy1 fy2) _) (T.R (TCons fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

data TypeScheme = Scheme (T.R (T.List TVName)) (T.R TExpr)

instance T.WrapVal TypeScheme
  where
  
  T.wrapVal pwrapVal (kwrapVal@(Scheme (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aScheme z1wrapVal z2wrapVal)
  

instance Eq TypeScheme
  where
  
  (!==) (%==) p =
    T.ufun2 (+$$!=$>==) (%==) p (*==)
    where
    
    (*==) (T.R (Scheme fy1 fy2) _) (T.R (Scheme fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

type Subst = AList TVName TExpr

type TcTypeEnv = AList Naam TypeScheme

type TypeEnv = AList Naam TExpr

type TypeNameSupply = TVName

type TypeInfo = T.Tuple3 Subst TExpr (AnnExpr Naam TExpr)

type TypeDependancy = DefnGroup Naam

type Point = T.Tuple2 Domain Route

data FrontierElem = MkFrel (T.R (T.List Route))

instance T.WrapVal FrontierElem
  where
  
  T.wrapVal pwrapVal (kwrapVal@(MkFrel (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aMkFrel z1wrapVal)
  

instance Eq FrontierElem
  where
  
  (!==) (%==) p =
    T.ufun2 (+$&^=%#==) (%==) p (*==)
    where
    
    (*==) (T.R (MkFrel fy1) _) (T.R (MkFrel fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

instance Ord FrontierElem
  where
  
  gcompare pcompare p =
    T.ufun2 a247v35compare pcompare p hcompare
    where
    
    hcompare (T.R (MkFrel fy3) _) (T.R (MkFrel fy4) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy4
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy1
          :: T.R TPrelude.Int)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy2)
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a247v35localFromEnum plocalFromEnum p hlocalFromEnum
        where
        
        hlocalFromEnum (T.R (MkFrel _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0)
        hlocalFromEnum _ p = T.fatal p
        
      
    
  

instance Show FrontierElem
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a247v40showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (MkFrel fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "MkFrel "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec _ _ p = T.fatal p
    
  

data Frontier =
  Min1Max0 (T.R Int) (T.R (T.List FrontierElem)) (T.R (T.List FrontierElem))

instance T.WrapVal Frontier
  where
  
  T.wrapVal pwrapVal
    (kwrapVal@(Min1Max0 (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal)))
    p =
    T.R kwrapVal
      (T.mkValueApp3 p pwrapVal aMin1Max0 z1wrapVal z2wrapVal z3wrapVal)
  

instance Eq Frontier
  where
  
  (!==) (%==) p =
    T.ufun2 (+$*!=$^==) (%==) p (*==)
    where
    
    (*==) (T.R (Min1Max0 fy1 fy2 fy3) _) (T.R (Min1Max0 fy4 fy5 fy6) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy4)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy5)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy3 fy6))
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

instance Ord Frontier
  where
  
  gcompare pcompare p =
    T.ufun2 a250v31compare pcompare p hcompare
    where
    
    hcompare (T.R (Min1Max0 fy3 fy4 fy5) _) (T.R (Min1Max0 fy6 fy7 fy8) _) p =
      T.uccase T.mkNoSrcPos p
        (let
          v250v31v1 (T.R TPrelude.EQ _) p =
            T.uccase T.mkNoSrcPos p
              (let
                v250v31v1 (T.R TPrelude.EQ _) p =
                  T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy5
                    fy8
                v250v31v1 fy1 p = T.projection T.mkNoSrcPos p fy1 in
                (v250v31v1))
              (T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy4 fy7)
          v250v31v1 fy1 p = T.projection T.mkNoSrcPos p fy1 in (v250v31v1))
        (T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy6)
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy1
          :: T.R TPrelude.Int)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy2)
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a250v31localFromEnum plocalFromEnum p hlocalFromEnum
        where
        
        hlocalFromEnum (T.R (Min1Max0 _ _ _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0)
        hlocalFromEnum _ p = T.fatal p
        
      
    
  

instance Show Frontier
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a250v36showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (Min1Max0 fy2 fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "Min1Max0 "))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
              (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)) fy2)
              (T.uap1 T.mkNoSrcPos p (TPrelude.gshowChar T.mkNoSrcPos p)
                (T.conChar T.mkNoSrcPos p ' ')))
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
                (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 10)) fy3)
                (T.uap1 T.mkNoSrcPos p (TPrelude.gshowChar T.mkNoSrcPos p)
                  (T.conChar T.mkNoSrcPos p ' ')))
              (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)) fy4))))
    hshowsPrec _ _ p = T.fatal p
    
  

data Domain =
  Two  | Lift1 (T.R (T.List Domain)) | Lift2 (T.R (T.List Domain))
  | Func (T.R (T.List Domain)) (T.R Domain)

instance T.WrapVal Domain
  where
  
  T.wrapVal pwrapVal (kwrapVal@Two) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aTwo)
  T.wrapVal pwrapVal (kwrapVal@(Lift1 (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aLift1 z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(Lift2 (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aLift2 z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(Func (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aFunc z1wrapVal z2wrapVal)
  

instance Eq Domain
  where
  
  (!==) (%==) p =
    T.ufun2 (+$*+=$*==) (%==) p (*==)
    where
    
    (*==) (T.R Two _) (T.R Two _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R (Lift1 fy1) _) (T.R (Lift1 fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (Lift2 fy1) _) (T.R (Lift2 fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (Func fy1 fy2) _) (T.R (Func fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

instance Ord Domain
  where
  
  gcompare pcompare p =
    T.ufun2 a256v29compare pcompare p hcompare
    where
    
    hcompare (T.R (Lift1 fy3) _) (T.R (Lift1 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy4
    hcompare (T.R (Lift2 fy3) _) (T.R (Lift2 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy4
    hcompare (T.R (Func fy3 fy4) _) (T.R (Func fy5 fy6) _) p =
      T.uccase T.mkNoSrcPos p
        (let
          v256v29v1 (T.R TPrelude.EQ _) p =
            T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy4 fy6
          v256v29v1 fy1 p = T.projection T.mkNoSrcPos p fy1 in (v256v29v1))
        (T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy5)
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy1
          :: T.R TPrelude.Int)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy2)
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a256v29localFromEnum plocalFromEnum p hlocalFromEnum
        where
        
        hlocalFromEnum (T.R (Two) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0)
        hlocalFromEnum (T.R (Lift1 _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 1)
        hlocalFromEnum (T.R (Lift2 _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 2)
        hlocalFromEnum (T.R (Func _ _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 3)
        hlocalFromEnum _ p = T.fatal p
        
      
    
  

instance Show Domain
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a256v34showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (Two) _) p =
      T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "Two")
    hshowsPrec fy1 (T.R (Lift1 fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "Lift1 "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec fy1 (T.R (Lift2 fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "Lift2 "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec fy1 (T.R (Func fy2 fy3) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "Func "))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
              (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)) fy2)
              (T.uap1 T.mkNoSrcPos p (TPrelude.gshowChar T.mkNoSrcPos p)
                (T.conChar T.mkNoSrcPos p ' ')))
            (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy3)))
    hshowsPrec _ _ p = T.fatal p
    
  

instance Read Domain
  where
  
  greadsPrec preadsPrec p =
    T.ufun1 a256v40readsPrec preadsPrec p hreadsPrec
    where
    
    hreadsPrec fy1 p =
      T.uap2 T.mkNoSrcPos p (TPreludeBasic.galt T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (TPrelude.greadParen T.mkNoSrcPos p)
          (T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse)
          (T.uap2 T.mkNoSrcPos p (TPreludeBasic.gthenLex T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gyield T.mkNoSrcPos p)
              (T.con0 T.mkNoSrcPos p Two aTwo))
            (T.fromLitString T.mkNoSrcPos p "Two")))
        (T.uap2 T.mkNoSrcPos p (TPreludeBasic.galt T.mkNoSrcPos p)
          (T.uap2 T.mkNoSrcPos p (TPrelude.greadParen T.mkNoSrcPos p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
              (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 9)))
            (T.uap2 T.mkNoSrcPos p (TPreludeBasic.gthenAp T.mkNoSrcPos p)
              (T.uap2 T.mkNoSrcPos p (TPreludeBasic.gthenLex T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gyield T.mkNoSrcPos p)
                  (T.pa0 Lift1 T.cn1 T.mkNoSrcPos p aLift1))
                (T.fromLitString T.mkNoSrcPos p "Lift1"))
              (T.uap1 T.mkNoSrcPos p (TPrelude.greadsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)))))
          (T.uap2 T.mkNoSrcPos p (TPreludeBasic.galt T.mkNoSrcPos p)
            (T.uap2 T.mkNoSrcPos p (TPrelude.greadParen T.mkNoSrcPos p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 9)))
              (T.uap2 T.mkNoSrcPos p (TPreludeBasic.gthenAp T.mkNoSrcPos p)
                (T.uap2 T.mkNoSrcPos p (TPreludeBasic.gthenLex T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gyield T.mkNoSrcPos p)
                    (T.pa0 Lift2 T.cn1 T.mkNoSrcPos p aLift2))
                  (T.fromLitString T.mkNoSrcPos p "Lift2"))
                (T.uap1 T.mkNoSrcPos p (TPrelude.greadsPrec T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 10)))))
            (T.uap2 T.mkNoSrcPos p (TPrelude.greadParen T.mkNoSrcPos p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 9)))
              (T.uap2 T.mkNoSrcPos p (TPreludeBasic.gthenAp T.mkNoSrcPos p)
                (T.uap2 T.mkNoSrcPos p (TPreludeBasic.gthenAp T.mkNoSrcPos p)
                  (T.uap2 T.mkNoSrcPos p (TPreludeBasic.gthenLex T.mkNoSrcPos p)
                    (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gyield T.mkNoSrcPos p)
                      (T.pa0 Func T.cn2 T.mkNoSrcPos p aFunc))
                    (T.fromLitString T.mkNoSrcPos p "Func"))
                  (T.uap1 T.mkNoSrcPos p (TPrelude.greadsPrec T.mkNoSrcPos p)
                    (T.uap1 T.mkNoSrcPos p
                      (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                      (T.conInteger T.mkNoSrcPos p 10))))
                (T.uap1 T.mkNoSrcPos p (TPrelude.greadsPrec T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 10)))))))
    
  

data Route =
  Zero  | One  | Stop1  | Up1 (T.R (T.List Route)) | Stop2  | Up2 
  | UpUp2 (T.R (T.List Route)) | Rep (T.R Rep)

instance T.WrapVal Route
  where
  
  T.wrapVal pwrapVal (kwrapVal@Zero) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aZero)
  T.wrapVal pwrapVal (kwrapVal@One) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aOne)
  T.wrapVal pwrapVal (kwrapVal@Stop1) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aStop1)
  T.wrapVal pwrapVal (kwrapVal@(Up1 (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aUp1 z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@Stop2) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aStop2)
  T.wrapVal pwrapVal (kwrapVal@Up2) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aUp2)
  T.wrapVal pwrapVal (kwrapVal@(UpUp2 (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aUpUp2 z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(Rep (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aRep z1wrapVal)
  

instance Eq Route
  where
  
  (!==) (%==) p =
    T.ufun2 (+$++=$&==) (%==) p (*==)
    where
    
    (*==) (T.R Zero _) (T.R Zero _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R One _) (T.R One _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R Stop1 _) (T.R Stop1 _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R (Up1 fy1) _) (T.R (Up1 fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R Stop2 _) (T.R Stop2 _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R Up2 _) (T.R Up2 _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R (UpUp2 fy1) _) (T.R (UpUp2 fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (Rep fy1) _) (T.R (Rep fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

instance Ord Route
  where
  
  gcompare pcompare p =
    T.ufun2 a266v28compare pcompare p hcompare
    where
    
    hcompare (T.R (Up1 fy3) _) (T.R (Up1 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy4
    hcompare (T.R (UpUp2 fy3) _) (T.R (UpUp2 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy4
    hcompare (T.R (Rep fy3) _) (T.R (Rep fy4) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy4
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy1
          :: T.R TPrelude.Int)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy2)
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a266v28localFromEnum plocalFromEnum p hlocalFromEnum
        where
        
        hlocalFromEnum (T.R (Zero) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0)
        hlocalFromEnum (T.R (One) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 1)
        hlocalFromEnum (T.R (Stop1) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 2)
        hlocalFromEnum (T.R (Up1 _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 3)
        hlocalFromEnum (T.R (Stop2) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 4)
        hlocalFromEnum (T.R (Up2) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 5)
        hlocalFromEnum (T.R (UpUp2 _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 6)
        hlocalFromEnum (T.R (Rep _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 7)
        hlocalFromEnum _ p = T.fatal p
        
      
    
  

instance Show Route
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a266v33showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (Zero) _) p =
      T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "Zero")
    hshowsPrec fy1 (T.R (One) _) p =
      T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "One")
    hshowsPrec fy1 (T.R (Stop1) _) p =
      T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "Stop1")
    hshowsPrec fy1 (T.R (Up1 fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "Up1 "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec fy1 (T.R (Stop2) _) p =
      T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "Stop2")
    hshowsPrec fy1 (T.R (Up2) _) p =
      T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "Up2")
    hshowsPrec fy1 (T.R (UpUp2 fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "UpUp2 "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec fy1 (T.R (Rep fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "Rep "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec _ _ p = T.fatal p
    
  

data Rep =
  RepTwo (T.R Frontier) | Rep1 (T.R Frontier) (T.R (T.List Rep))
  | Rep2 (T.R Frontier) (T.R Frontier) (T.R (T.List Rep))

instance T.WrapVal Rep
  where
  
  T.wrapVal pwrapVal (kwrapVal@(RepTwo (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aRepTwo z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(Rep1 (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aRep1 z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal
    (kwrapVal@(Rep2 (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal))) p =
    T.R kwrapVal (T.mkValueApp3 p pwrapVal aRep2 z1wrapVal z2wrapVal z3wrapVal)
  

instance Eq Rep
  where
  
  (!==) (%==) p =
    T.ufun2 (+$^#=$$==) (%==) p (*==)
    where
    
    (*==) (T.R (RepTwo fy1) _) (T.R (RepTwo fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (Rep1 fy1 fy2) _) (T.R (Rep1 fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) (T.R (Rep2 fy1 fy2 fy3) _) (T.R (Rep2 fy4 fy5 fy6) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy4)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy5)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy3 fy6))
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

instance Ord Rep
  where
  
  gcompare pcompare p =
    T.ufun2 a271v26compare pcompare p hcompare
    where
    
    hcompare (T.R (RepTwo fy3) _) (T.R (RepTwo fy4) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy4
    hcompare (T.R (Rep1 fy3 fy4) _) (T.R (Rep1 fy5 fy6) _) p =
      T.uccase T.mkNoSrcPos p
        (let
          v271v26v1 (T.R TPrelude.EQ _) p =
            T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy4 fy6
          v271v26v1 fy1 p = T.projection T.mkNoSrcPos p fy1 in (v271v26v1))
        (T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy5)
    hcompare (T.R (Rep2 fy3 fy4 fy5) _) (T.R (Rep2 fy6 fy7 fy8) _) p =
      T.uccase T.mkNoSrcPos p
        (let
          v271v26v1 (T.R TPrelude.EQ _) p =
            T.uccase T.mkNoSrcPos p
              (let
                v271v26v1 (T.R TPrelude.EQ _) p =
                  T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy5
                    fy8
                v271v26v1 fy1 p = T.projection T.mkNoSrcPos p fy1 in
                (v271v26v1))
              (T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy4 fy7)
          v271v26v1 fy1 p = T.projection T.mkNoSrcPos p fy1 in (v271v26v1))
        (T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy6)
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy1
          :: T.R TPrelude.Int)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy2)
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a271v26localFromEnum plocalFromEnum p hlocalFromEnum
        where
        
        hlocalFromEnum (T.R (RepTwo _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0)
        hlocalFromEnum (T.R (Rep1 _ _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 1)
        hlocalFromEnum (T.R (Rep2 _ _ _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 2)
        hlocalFromEnum _ p = T.fatal p
        
      
    
  

instance Show Rep
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a271v31showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (RepTwo fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "RepTwo "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec fy1 (T.R (Rep1 fy2 fy3) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "Rep1 "))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
              (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)) fy2)
              (T.uap1 T.mkNoSrcPos p (TPrelude.gshowChar T.mkNoSrcPos p)
                (T.conChar T.mkNoSrcPos p ' ')))
            (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy3)))
    hshowsPrec fy1 (T.R (Rep2 fy2 fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "Rep2 "))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
              (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)) fy2)
              (T.uap1 T.mkNoSrcPos p (TPrelude.gshowChar T.mkNoSrcPos p)
                (T.conChar T.mkNoSrcPos p ' ')))
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
              (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
                (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                  (T.uap1 T.mkNoSrcPos p
                    (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                    (T.conInteger T.mkNoSrcPos p 10)) fy3)
                (T.uap1 T.mkNoSrcPos p (TPrelude.gshowChar T.mkNoSrcPos p)
                  (T.conChar T.mkNoSrcPos p ' ')))
              (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)) fy4))))
    hshowsPrec _ _ p = T.fatal p
    
  

data DExpr =
  DXTwo  | DXLift1 (T.R (T.List DExpr)) | DXLift2 (T.R (T.List DExpr))
  | DXFunc (T.R (T.List DExpr)) (T.R DExpr) | DXVar (T.R String)

instance T.WrapVal DExpr
  where
  
  T.wrapVal pwrapVal (kwrapVal@DXTwo) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aDXTwo)
  T.wrapVal pwrapVal (kwrapVal@(DXLift1 (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aDXLift1 z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(DXLift2 (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aDXLift2 z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(DXFunc (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aDXFunc z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(DXVar (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aDXVar z1wrapVal)
  

instance Eq DExpr
  where
  
  (!==) (%==) p =
    T.ufun2 (+$^@=$&==) (%==) p (*==)
    where
    
    (*==) (T.R DXTwo _) (T.R DXTwo _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R (DXLift1 fy1) _) (T.R (DXLift1 fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (DXLift2 fy1) _) (T.R (DXLift2 fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (DXFunc fy1 fy2) _) (T.R (DXFunc fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) (T.R (DXVar fy1) _) (T.R (DXVar fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

type RSubst = AList String Route

type DSubst = AList String Domain

type DRRSubst = AList String (T.Tuple3 Domain Route Route)

type DExprEnv = AList String DExpr

data ConstrElem = ConstrRec  | ConstrVar (T.R Int)

instance T.WrapVal ConstrElem
  where
  
  T.wrapVal pwrapVal (kwrapVal@ConstrRec) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aConstrRec)
  T.wrapVal pwrapVal (kwrapVal@(ConstrVar (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aConstrVar z1wrapVal)
  

instance Eq ConstrElem
  where
  
  (!==) (%==) p =
    T.ufun2 (+$>!=$>==) (%==) p (*==)
    where
    
    (*==) (T.R ConstrRec _) (T.R ConstrRec _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R (ConstrVar fy1) _) (T.R (ConstrVar fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

instance Ord ConstrElem
  where
  
  gcompare pcompare p =
    T.ufun2 a290v33compare pcompare p hcompare
    where
    
    hcompare (T.R (ConstrVar fy3) _) (T.R (ConstrVar fy4) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p) fy3 fy4
    hcompare fy1 fy2 p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gcompare T.mkNoSrcPos p)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy1
          :: T.R TPrelude.Int)
        (T.uap1 T.mkNoSrcPos p (glocalFromEnum T.mkNoSrcPos p) fy2)
      where
      
      glocalFromEnum plocalFromEnum p =
        T.ufun1 a290v33localFromEnum plocalFromEnum p hlocalFromEnum
        where
        
        hlocalFromEnum (T.R (ConstrRec) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 0)
        hlocalFromEnum (T.R (ConstrVar _) _) p =
          T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 1)
        hlocalFromEnum _ p = T.fatal p
        
      
    
  

instance Show ConstrElem
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a290v38showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (ConstrRec) _) p =
      T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
        (T.fromLitString T.mkNoSrcPos p "ConstrRec")
    hshowsPrec fy1 (T.R (ConstrVar fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "ConstrVar "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec _ _ p = T.fatal p
    
  

data ACMode = Safe  | Live 

instance T.WrapVal ACMode
  where
  
  T.wrapVal pwrapVal (kwrapVal@Safe) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aSafe)
  T.wrapVal pwrapVal (kwrapVal@Live) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aLive)
  

instance Eq ACMode
  where
  
  (!==) (%==) p =
    T.ufun2 (+$>>=$*==) (%==) p (*==)
    where
    
    (*==) (T.R Safe _) (T.R Safe _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R Live _) (T.R Live _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

type MemoList = AList (T.List Route) Route

data AppInfo =
  A2  | ALo1  | AHi1 (T.R Int) (T.R Int) (T.R Domain) | ALo2  | AMid2 
  | AHi2 (T.R Int) (T.R Int) (T.R Domain)

instance T.WrapVal AppInfo
  where
  
  T.wrapVal pwrapVal (kwrapVal@A2) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aA2)
  T.wrapVal pwrapVal (kwrapVal@ALo1) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aALo1)
  T.wrapVal pwrapVal
    (kwrapVal@(AHi1 (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal))) p =
    T.R kwrapVal (T.mkValueApp3 p pwrapVal aAHi1 z1wrapVal z2wrapVal z3wrapVal)
  T.wrapVal pwrapVal (kwrapVal@ALo2) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aALo2)
  T.wrapVal pwrapVal (kwrapVal@AMid2) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aAMid2)
  T.wrapVal pwrapVal
    (kwrapVal@(AHi2 (T.R _ z1wrapVal) (T.R _ z2wrapVal) (T.R _ z3wrapVal))) p =
    T.R kwrapVal (T.mkValueApp3 p pwrapVal aAHi2 z1wrapVal z2wrapVal z3wrapVal)
  

instance Eq AppInfo
  where
  
  (!==) (%==) p =
    T.ufun2 (+%$*=$+==) (%==) p (*==)
    where
    
    (*==) (T.R A2 _) (T.R A2 _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R ALo1 _) (T.R ALo1 _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R (AHi1 fy1 fy2 fy3) _) (T.R (AHi1 fy4 fy5 fy6) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy4)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy5)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy3 fy6))
    (*==) (T.R ALo2 _) (T.R ALo2 _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R AMid2 _) (T.R AMid2 _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R (AHi2 fy1 fy2 fy3) _) (T.R (AHi2 fy4 fy5 fy6) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy4)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy5)
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy3 fy6))
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

data HExpr a =
  HApp (T.R (HExpr a)) (T.R (HExpr a))
  | HVAp (T.R (HExpr a)) (T.R (T.List (HExpr a)))
  | HLam (T.R (T.List a)) (T.R (HExpr a)) | HVar (T.R a)
  | HMeet (T.R (T.List (HExpr a))) | HPoint (T.R Route)
  | HTable (T.R (AList Route (HExpr a)))

instance T.WrapVal (HExpr a)
  where
  
  T.wrapVal pwrapVal (kwrapVal@(HApp (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aHApp z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(HVAp (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aHVAp z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(HLam (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aHLam z1wrapVal z2wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(HVar (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aHVar z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(HMeet (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aHMeet z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(HPoint (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aHPoint z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(HTable (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aHTable z1wrapVal)
  

instance Eq a => Eq (HExpr a)
  where
  
  (!==) (%==) p =
    T.ufun2 (+%%>=$+==) (%==) p (*==)
    where
    
    (*==) (T.R (HApp fy1 fy2) _) (T.R (HApp fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) (T.R (HVAp fy1 fy2) _) (T.R (HVAp fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) (T.R (HLam fy1 fy2) _) (T.R (HLam fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) (T.R (HVar fy1) _) (T.R (HVar fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (HMeet fy1) _) (T.R (HMeet fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (HPoint fy1) _) (T.R (HPoint fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (HTable fy1) _) (T.R (HTable fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

instance Show a => Show (HExpr a)
  where
  
  gshowsPrec pshowsPrec p =
    T.ufun2 a339v30showsPrec pshowsPrec p hshowsPrec
    where
    
    hshowsPrec fy1 (T.R (HApp fy2 fy3) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "HApp "))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
              (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)) fy2)
              (T.uap1 T.mkNoSrcPos p (TPrelude.gshowChar T.mkNoSrcPos p)
                (T.conChar T.mkNoSrcPos p ' ')))
            (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy3)))
    hshowsPrec fy1 (T.R (HVAp fy2 fy3) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "HVAp "))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
              (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)) fy2)
              (T.uap1 T.mkNoSrcPos p (TPrelude.gshowChar T.mkNoSrcPos p)
                (T.conChar T.mkNoSrcPos p ' ')))
            (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy3)))
    hshowsPrec fy1 (T.R (HLam fy2 fy3) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "HLam "))
          (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
            (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
              (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
                (T.uap1 T.mkNoSrcPos p
                  (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                  (T.conInteger T.mkNoSrcPos p 10)) fy2)
              (T.uap1 T.mkNoSrcPos p (TPrelude.gshowChar T.mkNoSrcPos p)
                (T.conChar T.mkNoSrcPos p ' ')))
            (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
              (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
                (T.conInteger T.mkNoSrcPos p 10)) fy3)))
    hshowsPrec fy1 (T.R (HVar fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "HVar "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec fy1 (T.R (HMeet fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "HMeet "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec fy1 (T.R (HPoint fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "HPoint "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec fy1 (T.R (HTable fy2) _) p =
      T.uap2 T.mkNoSrcPos p (TPrelude.gshowParen T.mkNoSrcPos p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!> p) fy1
          (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
            (T.conInteger T.mkNoSrcPos p 9)))
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!. p)
          (T.uap1 T.mkNoSrcPos p (TPrelude.gshowString T.mkNoSrcPos p)
            (T.fromLitString T.mkNoSrcPos p "HTable "))
          (T.uap2 T.mkNoSrcPos p (TPrelude.gshowsPrec T.mkNoSrcPos p)
            (T.uap1 T.mkNoSrcPos p (TPreludeBasic.gfromInteger T.mkNoSrcPos p)
              (T.conInteger T.mkNoSrcPos p 10)) fy2))
    hshowsPrec _ _ p = T.fatal p
    
  

type PrPoint = T.List Int

type PrDomain = T.List PrPoint

type Token = T.Tuple2 Int (T.List Char)

data PResult a = PFail (T.R (T.List Token)) | POk (T.R a) (T.R (T.List Token))

instance T.WrapVal (PResult a)
  where
  
  T.wrapVal pwrapVal (kwrapVal@(PFail (T.R _ z1wrapVal))) p =
    T.R kwrapVal (T.mkValueApp1 p pwrapVal aPFail z1wrapVal)
  T.wrapVal pwrapVal (kwrapVal@(POk (T.R _ z1wrapVal) (T.R _ z2wrapVal))) p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aPOk z1wrapVal z2wrapVal)
  

instance Eq a => Eq (PResult a)
  where
  
  (!==) (%==) p =
    T.ufun2 (+%*>=$@==) (%==) p (*==)
    where
    
    (*==) (T.R (PFail fy1) _) (T.R (PFail fy2) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy2
    (*==) (T.R (POk fy1 fy2) _) (T.R (POk fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

type Parser a = T.Fun (T.List Token) (PResult a)

data PartialExpr = NoOp  | FoundOp (T.R Naam) (T.R CExpr)

instance T.WrapVal PartialExpr
  where
  
  T.wrapVal pwrapVal (kwrapVal@NoOp) p =
    T.R kwrapVal (T.mkValueUse p pwrapVal aNoOp)
  T.wrapVal pwrapVal (kwrapVal@(FoundOp (T.R _ z1wrapVal) (T.R _ z2wrapVal)))
    p =
    T.R kwrapVal (T.mkValueApp2 p pwrapVal aFoundOp z1wrapVal z2wrapVal)
  

instance Eq PartialExpr
  where
  
  (!==) (%==) p =
    T.ufun2 (+%+*=%!==) (%==) p (*==)
    where
    
    (*==) (T.R NoOp _) (T.R NoOp _) p =
      T.con0 T.mkNoSrcPos p TPrelude.True TPrelude.aTrue
    (*==) (T.R (FoundOp fy1 fy2) _) (T.R (FoundOp fy3 fy4) _) p =
      T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!&& p)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy1 fy3)
        (T.uap2 T.mkNoSrcPos p (T.mkNoSrcPos TPrelude.!== p) fy2 fy4)
    (*==) _ _ p = T.con0 T.mkNoSrcPos p TPrelude.False TPrelude.aFalse
    
  

type StaticComponent =
  T.Tuple7 DExprEnv DSubst (AList Naam (T.List ConstrElem))
    (AList Naam (T.List Naam)) (T.List Flag) (T.Tuple5 Int Int Int Int Int)
    (AList Domain Int)

tBaseDefs = T.mkModule "BaseDefs" "BaseDefs.hs" Prelude.True

aALeaf = T.mkConstructor tBaseDefs 180018 3 0 "ALeaf"

aABranch = T.mkConstructor tBaseDefs 190018 3 5 "ABranch"

aOk = T.mkConstructor tBaseDefs 260018 3 1 "Ok"

aFail = T.mkConstructor tBaseDefs 270018 3 1 "Fail"

aMkSet = T.mkConstructor tBaseDefs 410014 3 1 "MkSet"

aTypecheck = T.mkConstructor tBaseDefs 510013 3 0 "Typecheck"

aSimp = T.mkConstructor tBaseDefs 520013 3 0 "Simp"

aNoCaseOpt = T.mkConstructor tBaseDefs 530013 3 0 "NoCaseOpt"

aShowHExpr = T.mkConstructor tBaseDefs 540013 3 0 "ShowHExpr"

aNoPretty = T.mkConstructor tBaseDefs 550013 3 0 "NoPretty"

aNoFormat = T.mkConstructor tBaseDefs 560013 3 0 "NoFormat"

aNoBaraki = T.mkConstructor tBaseDefs 570013 3 0 "NoBaraki"

aSimpleInv = T.mkConstructor tBaseDefs 580013 3 0 "SimpleInv"

aPolyLim = T.mkConstructor tBaseDefs 590013 3 1 "PolyLim"

aMonoLim = T.mkConstructor tBaseDefs 600013 3 1 "MonoLim"

aForceAll = T.mkConstructor tBaseDefs 610013 3 0 "ForceAll"

aDryRun = T.mkConstructor tBaseDefs 620013 3 0 "DryRun"

aLowerLim = T.mkConstructor tBaseDefs 630013 3 1 "LowerLim"

aUpperLim = T.mkConstructor tBaseDefs 640013 3 1 "UpperLim"

aScaleUp = T.mkConstructor tBaseDefs 650013 3 1 "ScaleUp"

aSAResult = T.mkConstructor tBaseDefs 800015 3 3 "SAResult"

aSASearch = T.mkConstructor tBaseDefs 810015 3 4 "SASearch"

aSASizes = T.mkConstructor tBaseDefs 820015 3 3 "SASizes"

aSAHExpr = T.mkConstructor tBaseDefs 830015 3 2 "SAHExpr"

aSASL = T.mkConstructor tBaseDefs 840015 3 2 "SASL"

aSAGiveUp = T.mkConstructor tBaseDefs 850015 3 1 "SAGiveUp"

aMkExInt = T.mkConstructor tBaseDefs 930023 3 2 "MkExInt"

aTDefVar = T.mkConstructor tBaseDefs 1440019 3 1 "TDefVar"

aTDefCons = T.mkConstructor tBaseDefs 1450019 3 2 "TDefCons"

aEVar = T.mkConstructor tBaseDefs 1580016 3 1 "EVar"

aENum = T.mkConstructor tBaseDefs 1590016 3 1 "ENum"

aEConstr = T.mkConstructor tBaseDefs 1600016 3 1 "EConstr"

aEAp = T.mkConstructor tBaseDefs 1610016 3 2 "EAp"

aELet = T.mkConstructor tBaseDefs 1620016 3 3 "ELet"

aECase = T.mkConstructor tBaseDefs 1660016 3 2 "ECase"

aELam = T.mkConstructor tBaseDefs 1690016 3 2 "ELam"

aAVar = T.mkConstructor tBaseDefs 1820011 3 1 "AVar"

aANum = T.mkConstructor tBaseDefs 1830011 3 1 "ANum"

aAConstr = T.mkConstructor tBaseDefs 1840011 3 1 "AConstr"

aAAp = T.mkConstructor tBaseDefs 1850011 3 2 "AAp"

aALet = T.mkConstructor tBaseDefs 1860011 3 3 "ALet"

aACase = T.mkConstructor tBaseDefs 1870011 3 2 "ACase"

aALam = T.mkConstructor tBaseDefs 1880011 3 2 "ALam"

aEqnNVC = T.mkConstructor tBaseDefs 2020012 3 3 "EqnNVC"

aTVar = T.mkConstructor tBaseDefs 2140014 3 1 "TVar"

aTArr = T.mkConstructor tBaseDefs 2150014 3 2 "TArr"

aTCons = T.mkConstructor tBaseDefs 2160014 3 2 "TCons"

aScheme = T.mkConstructor tBaseDefs 2190019 3 2 "Scheme"

aMkFrel = T.mkConstructor tBaseDefs 2460021 3 1 "MkFrel"

aMin1Max0 = T.mkConstructor tBaseDefs 2490017 3 3 "Min1Max0"

aTwo = T.mkConstructor tBaseDefs 2520015 3 0 "Two"

aLift1 = T.mkConstructor tBaseDefs 2530015 3 1 "Lift1"

aLift2 = T.mkConstructor tBaseDefs 2540015 3 1 "Lift2"

aFunc = T.mkConstructor tBaseDefs 2550015 3 2 "Func"

aZero = T.mkConstructor tBaseDefs 2580014 3 0 "Zero"

aOne = T.mkConstructor tBaseDefs 2590014 3 0 "One"

aStop1 = T.mkConstructor tBaseDefs 2600014 3 0 "Stop1"

aUp1 = T.mkConstructor tBaseDefs 2610014 3 1 "Up1"

aStop2 = T.mkConstructor tBaseDefs 2620014 3 0 "Stop2"

aUp2 = T.mkConstructor tBaseDefs 2630014 3 0 "Up2"

aUpUp2 = T.mkConstructor tBaseDefs 2640014 3 1 "UpUp2"

aRep = T.mkConstructor tBaseDefs 2650014 3 1 "Rep"

aRepTwo = T.mkConstructor tBaseDefs 2680012 3 1 "RepTwo"

aRep1 = T.mkConstructor tBaseDefs 2690012 3 2 "Rep1"

aRep2 = T.mkConstructor tBaseDefs 2700012 3 3 "Rep2"

aDXTwo = T.mkConstructor tBaseDefs 2730014 3 0 "DXTwo"

aDXLift1 = T.mkConstructor tBaseDefs 2740014 3 1 "DXLift1"

aDXLift2 = T.mkConstructor tBaseDefs 2750014 3 1 "DXLift2"

aDXFunc = T.mkConstructor tBaseDefs 2760014 3 2 "DXFunc"

aDXVar = T.mkConstructor tBaseDefs 2770014 3 1 "DXVar"

aConstrRec = T.mkConstructor tBaseDefs 2880019 3 0 "ConstrRec"

aConstrVar = T.mkConstructor tBaseDefs 2890019 3 1 "ConstrVar"

aSafe = T.mkConstructor tBaseDefs 2970015 3 0 "Safe"

aLive = T.mkConstructor tBaseDefs 2980015 3 0 "Live"

aA2 = T.mkConstructor tBaseDefs 3070016 3 0 "A2"

aALo1 = T.mkConstructor tBaseDefs 3090016 3 0 "ALo1"

aAHi1 = T.mkConstructor tBaseDefs 3110016 3 3 "AHi1"

aALo2 = T.mkConstructor tBaseDefs 3160016 3 0 "ALo2"

aAMid2 = T.mkConstructor tBaseDefs 3180016 3 0 "AMid2"

aAHi2 = T.mkConstructor tBaseDefs 3200016 3 3 "AHi2"

aHApp = T.mkConstructor tBaseDefs 3320016 3 2 "HApp"

aHVAp = T.mkConstructor tBaseDefs 3330016 3 2 "HVAp"

aHLam = T.mkConstructor tBaseDefs 3340016 3 2 "HLam"

aHVar = T.mkConstructor tBaseDefs 3350016 3 1 "HVar"

aHMeet = T.mkConstructor tBaseDefs 3360016 3 1 "HMeet"

aHPoint = T.mkConstructor tBaseDefs 3370016 3 1 "HPoint"

aHTable = T.mkConstructor tBaseDefs 3380016 3 1 "HTable"

aPFail = T.mkConstructor tBaseDefs 3570018 3 1 "PFail"

aPOk = T.mkConstructor tBaseDefs 3580018 3 2 "POk"

aNoOp = T.mkConstructor tBaseDefs 3630020 3 0 "NoOp"

aFoundOp = T.mkConstructor tBaseDefs 3640020 3 2 "FoundOp"

(+$!=$@==) = T.mkVariable tBaseDefs 200028 3 2 "==" Prelude.False

(+$@=$@==) = T.mkVariable tBaseDefs 280028 3 2 "==" Prelude.False

(+&$=$&==) = T.mkVariable tBaseDefs 420024 3 2 "==" Prelude.False

(+++=$%==) = T.mkVariable tBaseDefs 660023 3 2 "==" Prelude.False

abdDefaultSettings =
  T.mkVariable tBaseDefs 680001 3 0 "bdDefaultSettings" Prelude.False

abdDryRunSettings =
  T.mkVariable tBaseDefs 710001 3 0 "bdDryRunSettings" Prelude.False

(+>&=%%==) = T.mkVariable tBaseDefs 940033 3 2 "==" Prelude.False

a94v37compare = T.mkVariable tBaseDefs 940037 3 2 "compare" Prelude.False

a94v42showsPrec = T.mkVariable tBaseDefs 940042 3 2 "showsPrec" Prelude.False

(+>>=$#+) = T.mkVariable tBaseDefs 990021 26 2 "+" Prelude.False

(+#!$=$#*) = T.mkVariable tBaseDefs 1020021 30 2 "*" Prelude.False

(+#&@=$>==) = T.mkVariable tBaseDefs 1480029 3 2 "==" Prelude.False

(+#^$=$+==) = T.mkVariable tBaseDefs 1720026 3 2 "==" Prelude.False

(+#@>=$#==) = T.mkVariable tBaseDefs 1890021 3 2 "==" Prelude.False

(+$!%=$$==) = T.mkVariable tBaseDefs 2030022 3 2 "==" Prelude.False

(+$#^=$&==) = T.mkVariable tBaseDefs 2170024 3 2 "==" Prelude.False

(+$$!=$>==) = T.mkVariable tBaseDefs 2200029 3 2 "==" Prelude.False

(+$&^=%#==) = T.mkVariable tBaseDefs 2470031 3 2 "==" Prelude.False

a247v35compare = T.mkVariable tBaseDefs 2470035 3 2 "compare" Prelude.False

a247v40showsPrec = T.mkVariable tBaseDefs 2470040 3 2 "showsPrec" Prelude.False

(+$*!=$^==) = T.mkVariable tBaseDefs 2500027 3 2 "==" Prelude.False

a250v31compare = T.mkVariable tBaseDefs 2500031 3 2 "compare" Prelude.False

a250v36showsPrec = T.mkVariable tBaseDefs 2500036 3 2 "showsPrec" Prelude.False

(+$*+=$*==) = T.mkVariable tBaseDefs 2560025 3 2 "==" Prelude.False

a256v29compare = T.mkVariable tBaseDefs 2560029 3 2 "compare" Prelude.False

a256v34showsPrec = T.mkVariable tBaseDefs 2560034 3 2 "showsPrec" Prelude.False

a256v40readsPrec = T.mkVariable tBaseDefs 2560040 3 1 "readsPrec" Prelude.False

(+$++=$&==) = T.mkVariable tBaseDefs 2660024 3 2 "==" Prelude.False

a266v28compare = T.mkVariable tBaseDefs 2660028 3 2 "compare" Prelude.False

a266v33showsPrec = T.mkVariable tBaseDefs 2660033 3 2 "showsPrec" Prelude.False

(+$^#=$$==) = T.mkVariable tBaseDefs 2710022 3 2 "==" Prelude.False

a271v26compare = T.mkVariable tBaseDefs 2710026 3 2 "compare" Prelude.False

a271v31showsPrec = T.mkVariable tBaseDefs 2710031 3 2 "showsPrec" Prelude.False

(+$^@=$&==) = T.mkVariable tBaseDefs 2780024 3 2 "==" Prelude.False

(+$>!=$>==) = T.mkVariable tBaseDefs 2900029 3 2 "==" Prelude.False

a290v33compare = T.mkVariable tBaseDefs 2900033 3 2 "compare" Prelude.False

a290v38showsPrec = T.mkVariable tBaseDefs 2900038 3 2 "showsPrec" Prelude.False

(+$>>=$*==) = T.mkVariable tBaseDefs 2990025 3 2 "==" Prelude.False

(+%$*=$+==) = T.mkVariable tBaseDefs 3250026 3 2 "==" Prelude.False

(+%%>=$+==) = T.mkVariable tBaseDefs 3390026 3 2 "==" Prelude.False

a339v30showsPrec = T.mkVariable tBaseDefs 3390030 3 2 "showsPrec" Prelude.False

(+%*>=$@==) = T.mkVariable tBaseDefs 3590028 3 2 "==" Prelude.False

(+%+*=%!==) = T.mkVariable tBaseDefs 3650030 3 2 "==" Prelude.False

a94v37localFromEnum =
  T.mkVariable tBaseDefs 940037 3 1 "localFromEnum" Prelude.True

a247v35localFromEnum =
  T.mkVariable tBaseDefs 2470035 3 1 "localFromEnum" Prelude.True

a250v31localFromEnum =
  T.mkVariable tBaseDefs 2500031 3 1 "localFromEnum" Prelude.True

a256v29localFromEnum =
  T.mkVariable tBaseDefs 2560029 3 1 "localFromEnum" Prelude.True

a266v28localFromEnum =
  T.mkVariable tBaseDefs 2660028 3 1 "localFromEnum" Prelude.True

a271v26localFromEnum =
  T.mkVariable tBaseDefs 2710026 3 1 "localFromEnum" Prelude.True

a290v33localFromEnum =
  T.mkVariable tBaseDefs 2900033 3 1 "localFromEnum" Prelude.True

p18v18 = T.mkSrcPos tBaseDefs 180018

p19v18 = T.mkSrcPos tBaseDefs 190018

p20v28 = T.mkSrcPos tBaseDefs 200028

p26v18 = T.mkSrcPos tBaseDefs 260018

p27v18 = T.mkSrcPos tBaseDefs 270018

p28v28 = T.mkSrcPos tBaseDefs 280028

p41v14 = T.mkSrcPos tBaseDefs 410014

p42v24 = T.mkSrcPos tBaseDefs 420024

p51v13 = T.mkSrcPos tBaseDefs 510013

p52v13 = T.mkSrcPos tBaseDefs 520013

p53v13 = T.mkSrcPos tBaseDefs 530013

p54v13 = T.mkSrcPos tBaseDefs 540013

p55v13 = T.mkSrcPos tBaseDefs 550013

p56v13 = T.mkSrcPos tBaseDefs 560013

p57v13 = T.mkSrcPos tBaseDefs 570013

p58v13 = T.mkSrcPos tBaseDefs 580013

p59v13 = T.mkSrcPos tBaseDefs 590013

p60v13 = T.mkSrcPos tBaseDefs 600013

p61v13 = T.mkSrcPos tBaseDefs 610013

p62v13 = T.mkSrcPos tBaseDefs 620013

p63v13 = T.mkSrcPos tBaseDefs 630013

p64v13 = T.mkSrcPos tBaseDefs 640013

p65v13 = T.mkSrcPos tBaseDefs 650013

p66v23 = T.mkSrcPos tBaseDefs 660023

p68v1 = T.mkSrcPos tBaseDefs 680001

p69v6 = T.mkSrcPos tBaseDefs 690006

p69v7 = T.mkSrcPos tBaseDefs 690007

p69v15 = T.mkSrcPos tBaseDefs 690015

p69v22 = T.mkSrcPos tBaseDefs 690022

p69v30 = T.mkSrcPos tBaseDefs 690030

p69v37 = T.mkSrcPos tBaseDefs 690037

p69v46 = T.mkSrcPos tBaseDefs 690046

p69v49 = T.mkSrcPos tBaseDefs 690049

p69v58 = T.mkSrcPos tBaseDefs 690058

p69v67 = T.mkSrcPos tBaseDefs 690067

p69v75 = T.mkSrcPos tBaseDefs 690075

p71v1 = T.mkSrcPos tBaseDefs 710001

p72v6 = T.mkSrcPos tBaseDefs 720006

p72v7 = T.mkSrcPos tBaseDefs 720007

p72v17 = T.mkSrcPos tBaseDefs 720017

p72v26 = T.mkSrcPos tBaseDefs 720026

p72v29 = T.mkSrcPos tBaseDefs 720029

p72v38 = T.mkSrcPos tBaseDefs 720038

p72v41 = T.mkSrcPos tBaseDefs 720041

p72v49 = T.mkSrcPos tBaseDefs 720049

p72v52 = T.mkSrcPos tBaseDefs 720052

p72v60 = T.mkSrcPos tBaseDefs 720060

p72v63 = T.mkSrcPos tBaseDefs 720063

p72v71 = T.mkSrcPos tBaseDefs 720071

p80v15 = T.mkSrcPos tBaseDefs 800015

p81v15 = T.mkSrcPos tBaseDefs 810015

p82v15 = T.mkSrcPos tBaseDefs 820015

p83v15 = T.mkSrcPos tBaseDefs 830015

p84v15 = T.mkSrcPos tBaseDefs 840015

p85v15 = T.mkSrcPos tBaseDefs 850015

p93v23 = T.mkSrcPos tBaseDefs 930023

p94v33 = T.mkSrcPos tBaseDefs 940033

p94v37 = T.mkSrcPos tBaseDefs 940037

p94v42 = T.mkSrcPos tBaseDefs 940042

p99v21 = T.mkSrcPos tBaseDefs 990021

p100v9 = T.mkSrcPos tBaseDefs 1000009

p100v21 = T.mkSrcPos tBaseDefs 1000021

p100v32 = T.mkSrcPos tBaseDefs 1000032

p102v21 = T.mkSrcPos tBaseDefs 1020021

p103v9 = T.mkSrcPos tBaseDefs 1030009

p103v21 = T.mkSrcPos tBaseDefs 1030021

p103v32 = T.mkSrcPos tBaseDefs 1030032

p144v19 = T.mkSrcPos tBaseDefs 1440019

p145v19 = T.mkSrcPos tBaseDefs 1450019

p148v29 = T.mkSrcPos tBaseDefs 1480029

p158v16 = T.mkSrcPos tBaseDefs 1580016

p159v16 = T.mkSrcPos tBaseDefs 1590016

p160v16 = T.mkSrcPos tBaseDefs 1600016

p161v16 = T.mkSrcPos tBaseDefs 1610016

p162v16 = T.mkSrcPos tBaseDefs 1620016

p166v16 = T.mkSrcPos tBaseDefs 1660016

p169v16 = T.mkSrcPos tBaseDefs 1690016

p172v26 = T.mkSrcPos tBaseDefs 1720026

p182v11 = T.mkSrcPos tBaseDefs 1820011

p183v11 = T.mkSrcPos tBaseDefs 1830011

p184v11 = T.mkSrcPos tBaseDefs 1840011

p185v11 = T.mkSrcPos tBaseDefs 1850011

p186v11 = T.mkSrcPos tBaseDefs 1860011

p187v11 = T.mkSrcPos tBaseDefs 1870011

p188v11 = T.mkSrcPos tBaseDefs 1880011

p189v21 = T.mkSrcPos tBaseDefs 1890021

p202v12 = T.mkSrcPos tBaseDefs 2020012

p203v22 = T.mkSrcPos tBaseDefs 2030022

p214v14 = T.mkSrcPos tBaseDefs 2140014

p215v14 = T.mkSrcPos tBaseDefs 2150014

p216v14 = T.mkSrcPos tBaseDefs 2160014

p217v24 = T.mkSrcPos tBaseDefs 2170024

p219v19 = T.mkSrcPos tBaseDefs 2190019

p220v29 = T.mkSrcPos tBaseDefs 2200029

p246v21 = T.mkSrcPos tBaseDefs 2460021

p247v31 = T.mkSrcPos tBaseDefs 2470031

p247v35 = T.mkSrcPos tBaseDefs 2470035

p247v40 = T.mkSrcPos tBaseDefs 2470040

p249v17 = T.mkSrcPos tBaseDefs 2490017

p250v27 = T.mkSrcPos tBaseDefs 2500027

p250v31 = T.mkSrcPos tBaseDefs 2500031

p250v36 = T.mkSrcPos tBaseDefs 2500036

p252v15 = T.mkSrcPos tBaseDefs 2520015

p253v15 = T.mkSrcPos tBaseDefs 2530015

p254v15 = T.mkSrcPos tBaseDefs 2540015

p255v15 = T.mkSrcPos tBaseDefs 2550015

p256v25 = T.mkSrcPos tBaseDefs 2560025

p256v29 = T.mkSrcPos tBaseDefs 2560029

p256v34 = T.mkSrcPos tBaseDefs 2560034

p256v40 = T.mkSrcPos tBaseDefs 2560040

p258v14 = T.mkSrcPos tBaseDefs 2580014

p259v14 = T.mkSrcPos tBaseDefs 2590014

p260v14 = T.mkSrcPos tBaseDefs 2600014

p261v14 = T.mkSrcPos tBaseDefs 2610014

p262v14 = T.mkSrcPos tBaseDefs 2620014

p263v14 = T.mkSrcPos tBaseDefs 2630014

p264v14 = T.mkSrcPos tBaseDefs 2640014

p265v14 = T.mkSrcPos tBaseDefs 2650014

p266v24 = T.mkSrcPos tBaseDefs 2660024

p266v28 = T.mkSrcPos tBaseDefs 2660028

p266v33 = T.mkSrcPos tBaseDefs 2660033

p268v12 = T.mkSrcPos tBaseDefs 2680012

p269v12 = T.mkSrcPos tBaseDefs 2690012

p270v12 = T.mkSrcPos tBaseDefs 2700012

p271v22 = T.mkSrcPos tBaseDefs 2710022

p271v26 = T.mkSrcPos tBaseDefs 2710026

p271v31 = T.mkSrcPos tBaseDefs 2710031

p273v14 = T.mkSrcPos tBaseDefs 2730014

p274v14 = T.mkSrcPos tBaseDefs 2740014

p275v14 = T.mkSrcPos tBaseDefs 2750014

p276v14 = T.mkSrcPos tBaseDefs 2760014

p277v14 = T.mkSrcPos tBaseDefs 2770014

p278v24 = T.mkSrcPos tBaseDefs 2780024

p288v19 = T.mkSrcPos tBaseDefs 2880019

p289v19 = T.mkSrcPos tBaseDefs 2890019

p290v29 = T.mkSrcPos tBaseDefs 2900029

p290v33 = T.mkSrcPos tBaseDefs 2900033

p290v38 = T.mkSrcPos tBaseDefs 2900038

p297v15 = T.mkSrcPos tBaseDefs 2970015

p298v15 = T.mkSrcPos tBaseDefs 2980015

p299v25 = T.mkSrcPos tBaseDefs 2990025

p307v16 = T.mkSrcPos tBaseDefs 3070016

p309v16 = T.mkSrcPos tBaseDefs 3090016

p311v16 = T.mkSrcPos tBaseDefs 3110016

p316v16 = T.mkSrcPos tBaseDefs 3160016

p318v16 = T.mkSrcPos tBaseDefs 3180016

p320v16 = T.mkSrcPos tBaseDefs 3200016

p325v26 = T.mkSrcPos tBaseDefs 3250026

p332v16 = T.mkSrcPos tBaseDefs 3320016

p333v16 = T.mkSrcPos tBaseDefs 3330016

p334v16 = T.mkSrcPos tBaseDefs 3340016

p335v16 = T.mkSrcPos tBaseDefs 3350016

p336v16 = T.mkSrcPos tBaseDefs 3360016

p337v16 = T.mkSrcPos tBaseDefs 3370016

p338v16 = T.mkSrcPos tBaseDefs 3380016

p339v26 = T.mkSrcPos tBaseDefs 3390026

p339v30 = T.mkSrcPos tBaseDefs 3390030

p357v18 = T.mkSrcPos tBaseDefs 3570018

p358v18 = T.mkSrcPos tBaseDefs 3580018

p359v28 = T.mkSrcPos tBaseDefs 3590028

p363v20 = T.mkSrcPos tBaseDefs 3630020

p364v20 = T.mkSrcPos tBaseDefs 3640020

p365v30 = T.mkSrcPos tBaseDefs 3650030
