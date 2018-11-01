-- | Functions to write bytecode sequences to file
module ByteCode.Write(bcWrite,withDirectory ) where

import ByteCode.Type
--import BCTags

import qualified Data.Map as Map
import StateMonad
import Control.Monad.State
import List(sortBy)
import Char(chr)
import IntState
import TokenId
import SysDeps(PackedString,unpackPS,packString,trace,openBinaryFileWrite)
import Id
import Flags
import Util.Extra
import IO
import System.FilePath
import System.Directory
import ForeignCode
import Syntax(CallConv(..))


type StringId = Int
type ModuleId = Int
type Module = [ StringId ]

data WState = WS { wsState :: IntState, wsStrings :: Map.Map String StringId, wsFreeS :: [StringId],
                   wsModules :: Map.Map Module ModuleId, wsFreeM :: [StringId] }

type Binary = [Char]
type Writer = State WState (Binary -> Binary)


bcVersion :: (Int,Int)
bcVersion = (1,10)

-- | Write a sequence of bytecode declarations into a file.
bcWrite :: IntState       -- ^ internal compiler state
        -> Flags          -- ^ compiler flags
        -> FileFlags      -- ^ info about the file to write
        -> [BCDecl]       -- ^ bytecode declarations to write to file
        -> IO ()

bcWrite state flags fflags prog =
    withDirectory (sObjectFile fflags) id wcode
    where
    wcode = wAll flags state prog


withDirectory :: String -> (String -> String) -> String -> IO ()
withDirectory dstPath xform dat =
    do let dstDir = takeDirectory dstPath
           dstFile = takeFileName dstPath
       createDirectoryIfMissing True dstDir
       handle <- openBinaryFileWrite (combine dstDir (xform dstFile))
       hPutStr handle dat
       hClose handle
-- FIXME: what about this?       tryWriteFile "interface" (sTypeFile fileflags) (buildInterface flags modid state)

wAll :: Flags -> IntState -> [BCDecl] -> Binary
wAll flags state ds = bs []
    where
    ws = WS state Map.empty [0..] Map.empty [0..]
    (bs,ws') = runState (wProgram flags ds) ws


wProgram :: Flags -> [BCDecl] -> Writer
wProgram flags ds =
    do header <- wChar 'H' >>> wChar 'S' >>> wChar 'B' >>> wChar 'C' >>>
                 wUShort (fst bcVersion) >>> wUShort (snd bcVersion) >>>
                 wUShort fl
       decls <-  wMap wDecl ds
       state <- readState wsState
       mref  <-  wModule (sepM $ strIS state $ miIS state)
       strings <- readState wsStrings
       extra  <-  wUShort (length ds) >>>
                  wStringTable strings
       return $ header . extra . mref . decls
  where
  fl = 0 + if sHat flags then 1 else 0

wDecl :: BCDecl -> Writer
wDecl (Fun name pos arity args code consts prim stack numDict fl) =
    do id <- wIdRef name
       rest <- wChar 'F' >>> wUByte arity >>> wUShort stack >>> wUByte (intFlags fl) >>> wConstTable consts >>> wCode code
       let rest' = rest  []
       len <- wUShort (length rest')
       return $ id . len . (rest'++)

wDecl (Con name pos size tag) =
    do id <- wIdRef name
       rest <- wChar 'C' >>> wUByte size >>> wUByte tag
       let rest' = rest []
       len <- wUShort (length rest')
       return $ id . len . (rest'++)

wDecl (Prim name pos) =
    do id <- wIdRef name
       rest <- wChar 'P' >>> wId name
       let rest' = rest []
       len <- wUShort (length rest')
       return $ id . len . (rest'++)

wDecl (External name pos arity cname cc nt) = wExternal name pos arity cname cc

wConstTable :: ConstTable -> Writer
wConstTable ct = wUShort (length ct') >>> wMap wConst ct'
    where
    ct' = map snd $ sortBy (\(x,_) (y,_) -> compare x y) $ Map.toList ct

wConst :: ConstItem -> Writer
wConst (CGlobal i y) = wGType y  >>> wId i
wConst (CInt i)      = wChar 'i' >>> wInt i
wConst (CInteger i)  = wChar 'l' >>> wInteger i
wConst (CFloat f)    = wChar 'f' >>> wFloat f
wConst (CDouble f)   = wChar 'd' >>> wDouble f
wConst (CString s)   = wChar 's' >>> wString s
wConst (CPos p)      =
   let (P s e)       = p
   in wChar 'p' >>> wInt s >>> wInt e
wConst (CVarDesc n p) =
   let (P s e)       = p
   in wChar 'v' >>> wString n >>> wInt s >>> wInt e

wGType :: GType -> Writer
wGType GCAF   = wChar 'A'
wGType GFUN   = wChar 'F'
wGType GFUN0  = wChar '0'
wGType GCON   = wChar 'C'
wGType GZCON  = wChar 'Z'
wGType GPRIM  = wChar 'P'
wGType GEXT   = wChar 'X'

wExternal :: Id -> Pos -> Int -> String -> CallConv -> Writer
wExternal name pos arity cName cc =
  do state <- getWIntState
     let syms    = getSymbolTable state
         memo    = foreignMemo syms
         forn    = toForeign syms memo cc Imported cName arity name
         cName'  = reverse $ takeWhile (/='&') $ reverse cName
     wForeign name cName' cc forn

wForeign :: Id -> String -> CallConv -> Foreign -> Writer
wForeign name cName cc (Foreign ie proto style mpath _ htok arity args res) =
    do id <- wIdRef name
       rest <- wChar 'X' >>> wString cName >>> wUShort arity >>> wCallConv cc style >>> wExternalArg res >>> wMap wExternalArg args
       let rest' = rest []
       len <- wUShort (length rest')
       return $ id . len . (rest'++)

wCallConv :: CallConv -> Style -> Writer
wCallConv _         Address     = wChar 'a'
wCallConv _         FunAddress  = wChar 'a'
wCallConv C         _           = wChar 'c'
wCallConv Cast      _           = wChar 'x'
wCallConv (Other s) _
  | s == "primitive" = wChar 'p'
  | s == "stdcall" = wChar 's'
  | s == "fastccall" = wChar 'C'
  | s == "faststdcall" = wChar 'S'
  | s == "builtin" = wChar 'b'
wCallConv e         _           = error $ "calling convention "++show e++" is not supported yet"

wExternalArg :: Arg -> Writer
wExternalArg a = wChar c
  where
  c  = case a of
        Int8 -> 'i'
        Int16 -> 'j'
        Int32 -> 'k'
        Int64 -> 'l'
        Word8 -> 'w'
        Word16 -> 'x'
        Word32 -> 'y'
        Word64 -> 'z'
        Int -> 'I'
        Float -> 'F'
        Double -> 'D'
        Char -> 'C'
        Bool -> 'B'
        Ptr -> 'P'
        (FunPtr _) -> 'P'
        StablePtr -> 'p'
        ForeignPtr -> 'f'
        Addr -> error "wExternalArg: Addr is no longer supported"
        ForeignObj -> error "wExternalArg: ForeignObj is no longer supported"
        PackedString -> 'u'
        Integer -> 'N'
        (HaskellFun _) -> 'H'
        (Unknown _) -> 'u'
        Unit -> 'U'

wReference :: BCDecl -> Writer
wReference (Fun name _ _ _ _ _ _ _ _ _) = wIdRef name
wReference (Con name _ _ _)             = wIdRef name

wStringTable :: Map.Map String StringId -> Writer
wStringTable st = wUShort (length st') >>> wMap wString st'
    where
    st' = sortListAT st

{-
wModuleTable :: Map.Map Module ModuleId -> Writer
wModuleTable mt = wMap wModule (sortListAT mt)
-}

wModule :: String -> Writer
wModule m = do is <- mapM addString ms
               wUByte (length ms) >>> wMap wUShort is
    where
    ms = splitModule ';' m []

splitModule :: Char -> String -> String -> [String]
splitModule sep []     []  = []
splitModule sep []     acc = [reverse acc]
splitModule sep (c:cs) acc
    | c == sep  = (reverse acc) : splitModule sep cs []
    | otherwise = splitModule sep cs (c:acc)

wCode :: Code -> Writer
wCode (CWrites ws) = do is <- wMap wWrite ws
                        let is' = is []
                        len <- wUShort (length is')
                        return $ len . (is'++)

wWrite :: Write -> Writer
wWrite (WUByte n)  = wUByte n
wWrite (WUShort n) = wUShort n
wWrite (WByte n)   = wByte n
wWrite (WShort n)  = wShort n

wIdRef :: Id -> Writer
wIdRef i = do state <- readState wsState
              let tid = tidIS state i
                  (md,id) = splitM tid
              wModule id

wStringRef :: String -> Writer
wStringRef s = do i <- addString s
                  wUShort i

wId :: Id -> Writer
wId i = do state <- readState wsState
           let tid = tidIS state i
               (md,id) = splitM tid
               md' = if md == "" then sepM $ strIS state $ miIS state else md
           wModule md' >>> wModule id

--------------------------------------------------------------------------------------------------------

addString :: String -> State WState Int
addString x = writeState (\s -> case Map.lookup x (wsStrings s) of
                                    Just i -> (s, i)
                                    _      -> let (i:is) = wsFreeS s
                                                  s' = s { wsFreeS = is, wsStrings = Map.insert x i (wsStrings s) }
                                              in (s', i)
                         )

addModule :: [Int] -> State WState Int
addModule x = writeState (\s -> case Map.lookup x (wsModules s) of
                                    Just i -> (s, i)
                                    _      -> let (i:is) = wsFreeM s
                                                  s' = s { wsFreeM = is, wsModules = Map.insert x i (wsModules s) }
                                              in (s', i)
                         )

sortListAT :: Ord v => Map.Map k v -> [k]
sortListAT st = map fst $ sortBy (\(_,x) (_,y) -> compare x y) $ Map.toList st

wMap :: (a -> Writer) -> [a] -> Writer
wMap f []     = wNil
wMap f (x:xs) = f x >>> wMap f xs

(>>>) :: Writer -> Writer -> Writer
x >>> y = do a <- x
             b <- y
             return $ a . b

wChar :: Char -> Writer
wChar c = return (c:)

wNil :: Writer
wNil = return id

wByte :: Int -> Writer
wByte i = if isByte i then let i' = (i + 256) `mod` 256 in wChar (chr i')
                      else error $ "wByte of "++show i

wUByte :: Int -> Writer
wUByte i = if isUByte i then wChar $ chr i
                        else error $ "wUByte of "++show i

wIntGeneric :: Integer -> Int -> Writer
wIntGeneric i 0
    | i == 0 || i == (-1) = wNil
    | otherwise           = error $ "wIntGeneric "++show i ++" 0"
wIntGeneric i n = wIntGeneric hi (n-1) >>> wUByte (fromInteger lo)
    where
    (hi,lo) = i `divMod` 256

wShort :: Int -> Writer
wShort i = if isShort i then wIntGeneric (toInteger i) 2
                        else error $ "wShort of "++show i

wUShort :: Int -> Writer
wUShort i = if isUShort i then wIntGeneric (toInteger i) 2
                          else error $ "wUShort of "++show i

wInt :: Int -> Writer
wInt i = if isInt i then wIntGeneric (toInteger i) 4
                    else error $ "wInt of "++show i

wInteger :: Integer -> Writer
wInteger i
 | i == 0 = wByte 0
 | i > 0  = do bs <- wInteger' i
               let len = length $ bs []
               wByte len >>> return bs
 | i < 0  = do bs <- wInteger' (-i)
               let len = length $ bs []
               wByte (-len) >>> return bs
 where
  wInteger' :: Integer -> Writer
  wInteger' 0 = wNil
  wInteger' i = wInteger' hi >>> wUByte (fromInteger lo)
    where
    (hi,lo) = i `divMod` 256

wString :: String -> Writer
wString s = wUShort (length s) >>> wMap wChar s

wFloat :: Float -> Writer
wFloat f = wInteger mant >>> wShort exp
    where
    (mant,exp) = decodeFloat f

wDouble :: Double -> Writer
wDouble f = wInteger mant >>> wShort exp
    where
    (mant,exp) = decodeFloat f


getWIntState :: State WState IntState
getWIntState = get >>= return . wsState
