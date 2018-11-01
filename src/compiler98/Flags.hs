{- ---------------------------------------------------------------------------
Flags are all the choices and information given to the compiler in the 
argument list. Here a data type Flags is defined for holding this information,
a function processArgs to obtain a value of type Flags from the argument list,
and a simple function pF for printing information demanded by a flag.
-}
module Flags(FileFlags(..), Flags(..), processArgs, processMoreArgs, splitArgs, pF, printUsage, getFileFlags, calcRootPath) where

import IO
import Util.OsOnly(fixRootDir,fixTypeFile,fixObjectFile)
import Char(isDigit)
import System.FilePath
import Util.Text

{- File flags are things which are specific to each compiled file -}
data FileFlags = FileFF { 
      sSourceFile :: String,   -- name of source file as given
      sModuleName :: String,   -- name of the module
      sTypeFile :: String,     -- full path to the .hi file
      sObjectFile :: String,   -- full path to the .hbc file
      sCoreFile :: String,     -- full path to the .ycr file
      sWrapFile :: String      -- full path to the .c wrapper
    }

instance Show FileFlags where
    show ff = unlines [ "sourceFile = "++sSourceFile ff,
                        "moduleName = "++sModuleName ff,
                        "typeFile = "++sTypeFile ff,
                        "objFile = "++sObjectFile ff,
                        "coreFile = "++sCoreFile ff,
                        "wrapFile = "++sWrapFile ff]

getFileFlags :: Flags -> FilePath -> String -> FileFlags
getFileFlags flags sourcefile modname = res
    where
    res = FileFF sourcefile modname typefile objfile corefile wrapfile
    
    typefile = fileForModule (sTypeDst flags) "hi"
    objfile = fileForModule (sObjectDst flags) "hbc"
    corefile = fileForModule (sObjectDst flags) "ycr"
    wrapfile = fileForModule (sWrapDst flags) "c"

    -- if modname == "Main" that means that the file may not be "Main", but could be something else
    name = if modname == "Main" then takeBaseName sourcefile else modname

    hidden = sHideObj flags
    rootPath = calcRootPath sourcefile modname
    
    fileForModule root ext =
            if hidden then
                addExtension (combine rootPath (joinPath [ext, name])) ext
            else
                replaceExtension (combine (rootPath </> root) (joinPath $ splitList "." name)) ext


-- | Figure out how far up the directory tree you have to go to get the root
calcRootPath :: FilePath -> String -> FilePath
calcRootPath filename modname = joinPath $ take tsize orig
    where
        orig = splitPath filename
        tsize = length orig - depthmod + depthfil - 1
        depthmod = length $ filter (== '.') modname
        depthfil = length $ filter (== '.') $ takeFileName $ dropExtension filename


{- Flags are flags that apply to every file -}
data Flags = FF 
  { -- sRealFile   :: String
    -- ,sSourceFile :: String
    -- ,sTypeFile   :: String
    -- ,sObjectFile :: String
  sRootFile :: String,     -- full path to root source file
  sIncludes     :: [String]
  ,sPreludes    :: [String]
  ,sBasePath    :: String

  ,sTypeDst     :: String --  where generated .hi should go
  ,sObjectDst   :: String --  where generated bcode should be 
  ,sWrapDst     :: String -- b-code    where generated wrappers should go
  ,sHideObj     :: Bool -- hide object code

--v Flags to control actions
  ,sViewCore    :: [String]

--v Flags to control compilation
  ,sRedefine   :: Bool   -- allow redefinitions of imported identifiers
  ,sPart       :: Bool   -- compiling part of a lib
  ,sCompileOne :: Bool   -- compile one file only
  ,sUnix       :: Bool   -- either unix or RiscOS
  ,sUnlit      :: Bool   -- unliterate the source code
  ,sCpp        :: Bool   -- preprocess the source code
  ,sHiSuffix   :: String -- set the interface file suffix (usually .hi)
  ,sHat        :: Bool

  ,sPrelude    :: Bool  -- keep prelude defns in interface file
  ,sLib        :: Bool  -- compiling a library
  ,sKeepCase   :: Bool  -- don't lift case, we fix those later

  ,sUnifyHack  :: Bool  -- enables type hackery that's required to make the prelude compile ...
  
  ,sDotNet     :: Bool  -- generate .NET IL

--v Flags for machine architecture / configuration
  ,sUnderscore :: Bool  -- force H'98 underscores
  ,sPuns       :: Bool  -- allow named-field puns
  ,s98         :: Bool  -- Haskell 98 mode

--v debugging flags - show program / import tables (after each compiler phase)
  ,sLex        :: Bool  -- input    after lexing
  ,sILex       :: Bool  -- input    after lexing imported interface files
  ,sParse      :: Bool  -- ast      after parsing
  ,sNeed       :: Bool  -- need table   before imports
  ,sINeed      :: Bool  -- need table   after all imports
  ,sIINeed     :: Bool  -- need table   after each import
  ,sIRename    :: Bool  -- rename table after all imports
  ,sIIRename   :: Bool  -- rename table after each import
  ,sImport     :: Bool  -- imported filenames
  ,sRImport    :: Bool  -- imports  actually used
  ,sDepend     :: Bool  -- imported ids (not currently used)
  ,sRename     :: Bool  -- ast      after rename
  ,sDerive     :: Bool  -- ast      after deriving
  ,sRemove     :: Bool  -- ast      after named-field removal
  ,sScc        :: Bool  -- ast      after strongly-connected-components
  ,sType       :: Bool  -- ast      after type check
  ,sFixSyntax  :: Bool  -- ast      after removing newtypes
  ,sLift       :: Bool  -- stg tree after lambda lifting
  ,sCase       :: Bool  -- stg tree after case pattern simplification
  ,sPrim       :: Bool  -- stg tree after inserting primitives
  ,sArity      :: Bool  -- stg tree after arity analysis
  ,sAtom       :: Bool  -- stg tree after only atoms in applications
  ,sFree       :: Bool  -- stg code with explicit free variables
  ,sBcodeCompile :: Bool -- b-code
  ,sBcodeMem   :: Bool  -- b-code       after NNEDHEAP analysis
  ,sBcodeFlatten :: Bool -- b-code      after flattening
  ,sBcodeWrapper :: Bool -- b-code      generate wrappers
  ,sBcodeRel :: Bool

  ,sFunNames   :: Bool  -- insert position and name of functions in the code
  

--v debugging flags - show symbol table (after each compiler phase)
  ,sIBound     :: Bool  -- after all imports
  ,sIIBound    :: Bool  -- after each import
  ,sRBound     :: Bool  -- after rename
  ,sDBound     :: Bool  -- after deriving
  ,sEBound     :: Bool  -- after extract
  ,sTBound     :: Bool  -- after type checking
  ,sFSBound    :: Bool  -- after fixsyntax
  ,sLBound     :: Bool  -- after lambda-lifting
  ,sCBound     :: Bool  -- after case
  ,sPBound     :: Bool  -- after inserting prims
  ,sABound     :: Bool  -- after only atoms in applications

--v miscellaneous flags
  ,sShowType   :: Bool  -- report type of "main" (for hmake interactive)
  ,sShowWidth  :: Int   -- width for showing intermediate program
  ,sShowIndent :: Int   -- indentation for nesting shown intermediate program
  ,sShowQualified :: Bool -- show qualified ids as far as possible
  
  ,sShowCore  :: Bool -- show Core
  ,sGenCore :: Bool -- generate a .ycr file
  ,sLinkCore :: Bool -- link all the core files together

--export control flags
  ,sExportAll :: Bool -- ignore what the module decl says, just export the lot
  
  ,sHelp :: Bool
  ,sVersion :: Bool
  }
  deriving (Show)


-- default implementation
class Default x where; def :: x
instance Default Bool where; def = False
instance Default [a] where; def = []
instance Default Int where; def = 0

instance Default Flags where
    def = FF
            def def def def def def def def def def def def def def def def def def def def
            def def def def def def def def def def def def def def def def def def def def
            def def def def def def def def def def def def def def def def def def def def
            def def def def def def def def def def def def def def def


data Opt = Opt {shortName :: String, longName :: String, value :: Value, description :: String}
         | OptGroup String Bool

isOpt :: Opt -> Bool
isOpt (Opt{}) = True
isOpt _ = False

data Value = BoolFlag Bool (Flags -> Bool -> Flags)
           | IntFlag Int (Flags -> Int -> Flags)
           | StrFlag String (Flags -> String -> Flags)

isBoolFlag :: Value -> Bool
isBoolFlag (BoolFlag _ _) = True
isBoolFlag _ = False



options :: [Opt]
options = filter isOpt allOpts

allOpts :: [Opt]
allOpts =
    [
      OptGroup "Path Options" True
    , Opt "I" "includes" (StrFlag "" $ \f x -> f{sIncludes=x `preCons` sIncludes f}) "Include directories"
    , Opt "P" "preludes" (StrFlag "" $ \f x -> f{sPreludes=x `preCons` sPreludes f}) "Prelude directories"
    , Opt "d" "dst" (StrFlag "." $ \f x -> f{sObjectDst=x}) "destination path for generated bytecode files"
    , Opt "i" "hidst" (StrFlag "." $ \ f x -> f{sTypeDst=x}) "destinated path for generated .hi file"
    , Opt "w" "wrapdst" (StrFlag "." $ \f x -> f{sWrapDst=x}) "destination path for generated FFI wrapper"
    , Opt ""  "hide" (bf $ \f x -> f{sHideObj=x}) "hide object files"

    , OptGroup "Generation Options" True
    , Opt ""  "hat" (bf $ \f x -> f{sHat=x}) "compile with Hat debugging support"
    , Opt ""  "dotnet" (bf $ \f x -> f{sDotNet=x}) "Generate .NET IL code"
    , Opt "W" "genwrapper"    (bf $ \f x -> f{sBcodeWrapper=x}) "generate FFI wrapper"
    , Opt ""  "hi-suffix" (StrFlag "hi" $ \f x -> f{sHiSuffix=x}) "change the default \".hi\" suffix"
    , Opt ""  "exportall" (bf $ \f x -> f{sExportAll=x}) "export all identifiers from a module, despite what the export list says"

    , OptGroup "Action Flags" True
    , Opt ""  "viewcore" (StrFlag "" $ \f x -> f{sViewCore=x `preCons` sViewCore f}) "View Core file (.ycr)"
    , Opt "c" "compile" (bf $ \f x -> f{sCompileOne=x}) "Compile one file only"

    , OptGroup "Compilation Options" True
    , Opt ""  "redefine" (bf $ \f x -> f{sRedefine=x}) "Don't complain if redefining an imported identifier"
    , Opt ""  "unix"     (bt $ \f x -> f{sUnix=x}) "Use unix file names"
    , Opt ""  "unlit"    (bf $ \f x -> f{sUnlit=x}) "Unliterate the source code"
    , Opt ""  "cpp"    (bf $ \f x -> f{sCpp=x}) "Pre-process the file first"
    , Opt ""  "prelude"    (bf $ \f x -> f{sPrelude=x}) "Keep prelude definitions in interface file"
    , Opt ""  "lib"    (bf $ \f x -> f{sLib=x}) "Compiling a lib, don't complain if importing modules with names that differs from their filename."
    , Opt ""  "unifyhack" (bf $ \f x -> f{sUnifyHack=x}) "Enable nasty type hack that's needed to make the prelude compile ..."
--    , Opt ""  "part"     (bf $ \f x -> f{sPart=x}) ("Compiling part of a lib, so don't complain if module name differs"++
--                                                    "from file name and don't create profiling information for this module")
    
    , OptGroup "Compliance Options" True
    , Opt ""  "underscore"    (bf $ \f x -> f{sUnderscore=x}) "Enable H'98 underscore-is-lower-case"
    , Opt ""  "puns"    (bt $ \f x -> f{sPuns=x}) "Enable pre-98 named-field puns"
    , Opt ""  "98"    (bf $ \f x -> f{s98=x}) "Haskell 98 compliance"

    , OptGroup "Help Options" True
    , Opt "v" "version" (bf $ \f x -> f{sVersion=x}) "Show version information"
    , Opt "?" "help"    (bf $ \f x -> f{sHelp=x}) "Show all options and useage information"

    , OptGroup "Core Options" True
    , Opt ""  "core" (bf $ \f x -> f{sGenCore=x}) "generate a .ycr binary file"
    , Opt ""  "showcore" (bf $ \f x -> f{sShowCore=x}) "show the Core language"
    , Opt ""  "linkcore" (bf $ \f x -> let f2 = f{sLinkCore=x} in if x then f2{sGenCore=True} else f2) "generate a linked .yca binary file"

    , OptGroup "Debug Options" False
    , Opt ""  "lex"    (bf $ \f x -> f{sLex=x}) "show lexical input"
    , Opt ""  "parse"    (bf $ \f x -> f{sParse=x}) "show syntax tree after parser"
    , Opt ""  "need"    (bf $ \f x -> f{sNeed=x}) "show need table before import"
    , Opt ""  "ineed"    (bf $ \f x -> f{sINeed=x}) "show need table after import"
    , Opt ""  "irename"    (bf $ \f x -> f{sIRename=x}) "show rename table after import"
    , Opt ""  "iineed"    (bf $ \f x -> f{sIINeed=x}) "show need table between all import files"
    , Opt ""  "iirename"    (bf $ \f x -> f{sIIRename=x}) "show rename table between all imports"
    , Opt ""  "rename"    (bf $ \f x -> f{sRename=x}) "show syntax tree after rename"
    , Opt ""  "derive"    (bf $ \f x -> f{sDerive=x}) "show syntax tree after derive"
    , Opt ""  "remove"    (bf $ \f x -> f{sRemove=x}) "show syntax tree after fields are removed (translated into selectors)"
    , Opt ""  "scc"    (bf $ \f x -> f{sScc=x}) "show syntax tree after splitting into strongly connected groups"
    , Opt ""  "type"    (bf $ \f x -> f{sType=x}) "show syntax tree after type check"
    , Opt ""  "fixsyntax"    (bf $ \f x -> f{sFixSyntax=x}) "show syntax tree after removing newtype constructors and fixing Class.Type.metod"
    , Opt ""  "lift"    (bf $ \f x -> f{sLift=x}) "show syntax tree after lambda lifting"
    , Opt ""  "case"    (bf $ \f x -> f{sCase=x}) "show stg tree after simplification of patterns"
    , Opt ""  "prim"    (bf $ \f x -> f{sPrim=x}) "show stg tree after inserting primitive functions"
    , Opt ""  "bcodecompile"    (bf $ \f x -> f{sBcodeCompile=x}) "show B code after compile"
    , Opt ""  "bcodemem"    (bf $ \f x -> f{sBcodeMem=x}) "show B code after heap usage analysis"
    , Opt ""  "bcodeflatten"    (bf $ \f x -> f{sBcodeFlatten=x}) "show B code after flattening"
    , Opt ""  "bcoderel"    (bf $ \f x -> f{sBcodeRel=x}) "show B code after converting to relative jumps"
    , Opt ""  "keepcase"    (bf $ \f x -> f{sKeepCase=x}) "Don't lift case, we fix those later"
    , Opt ""  "arity"    (bf $ \f x -> f{sArity=x}) "show stg tree after arity"
    , Opt ""  "ibound"    (bf $ \f x -> f{sIBound=x}) "show symbol table after import"
    , Opt ""  "iibound"    (bf $ \f x -> f{sIIBound=x}) "show symbol table between all import files"
    , Opt ""  "rbound"    (bf $ \f x -> f{sRBound=x}) "show symbol table after rename"
    , Opt ""  "dbound"    (bf $ \f x -> f{sDBound=x}) "show symbol table after derive"
    , Opt ""  "pbound"    (bf $ \f x -> f{sPBound=x}) "show symbol table after inserting primitive functions"
    , Opt ""  "ebound"    (bf $ \f x -> f{sEBound=x}) "show symbol table after extract"
    , Opt ""  "tbound"    (bf $ \f x -> f{sTBound=x}) "show symbol table after type check"
    , Opt ""  "fsbound"    (bf $ \f x -> f{sFSBound=x}) "show symbol table after adding Class.Type.method info"
    , Opt ""  "lbound"    (bf $ \f x -> f{sLBound=x}) "show symbol table after lambda lifting"
    , Opt ""  "cbound"    (bf $ \f x -> f{sCBound=x}) "show symbol table after simplification of pattern"
    , Opt ""  "abound"    (bf $ \f x -> f{sABound=x}) "show symbol table after only atoms in applications"
    , Opt ""  "import"    (bf $ \f x -> f{sImport=x}) "print name of imported files"
    , Opt ""  "depend"    (bf $ \f x -> f{sDepend=x}) "print imported identifiers that are used (not even alpha yet)"
    , Opt ""  "free"    (bf $ \f x -> f{sFree=x}) "show stg tree with explicitly free variables"
    , Opt ""  "atom"    (bf $ \f x -> f{sAtom=x}) "show stg tree after only atoms in applications"
    , Opt ""  "funnames"    (bf $ \f x -> f{sFunNames=x}) "insert position and name of functions in the code"
    , Opt ""  "ilex"    (bf $ \f x -> f{sILex=x}) "show lexical input"
    , Opt ""  "report-imports"    (bf $ \f x -> f{sRImport=x}) "show only imports actually used"  
    , Opt ""  "showtype" (bf $ \f x -> f{sShowType=x}) "report type of \"main\""
    , Opt ""  "showwidth" (IntFlag 80 $ \f x -> f{sShowWidth=x}) "set width for showing intermediate program"
    , Opt ""  "showindent" (IntFlag 2 $ \f x -> f{sShowIndent=x}) "set indentation for nesting"
    , Opt ""  "showqualified" (bt $ \f x -> f{sShowQualified=x}) "show qualified ids as far as possible"
    ]
    where
        bt = BoolFlag True
        bf = BoolFlag False
        
        preCons "" b = b
        preCons a  b = a:b


-- | Take a list of arguments, and turn them into a list of flag mutations
parseArgs :: [String] -> ([String], [Flags -> Flags])
parseArgs [] = ([], [])
parseArgs (x:xs) = case x of
                      ('-':'-':flag) -> f flag
                      ('-':flag) -> f flag
                      file -> ([file],[]) `join` parseArgs xs
    where
        f flag | arg && null xs = error $ "Expected flag after option: " ++ flag
               | arg = ([], [appArg x (head xs)]) `join` parseArgs (tail xs)
               | otherwise = ([], [appArg x ""]) `join` parseArgs xs
            where
                x = findFlag flag
                arg = not (isBoolFlag x)
        
        (a1,b1) `join` (a2,b2) = (a1++a2,b1++b2)
        
        
        appArg (BoolFlag b f) _ = flip f b
        appArg (IntFlag  i f) arg | all isDigit arg = flip f (read arg :: Int)
                                  | otherwise = error "Expected numeric argument"
        appArg (StrFlag  s f) arg = flip f arg




-- lookup a flag and find its value
-- report an error if invalid value
findFlag :: String -> Value
findFlag flag = case flag of
                ('n':'o':xs) -> invertBool $ f xs
                xs -> f xs
    where
        f xs = if null res then error ("Unknown flag: " ++ flag) else trueBool $ value (head res)
            where res = filter (\opt -> xs `elem` g opt) options
            
        g (Opt short long _ _) = long : map (:[]) short
            
        invertBool (BoolFlag b f) = BoolFlag (not b) f
        invertBool _ = error $ "Unknown flag: " ++ flag
        
        trueBool (BoolFlag b f) = BoolFlag True f
        trueBool x = x


-- should handle quotes etc, if required
splitArgs :: String -> [String]
splitArgs x = words x


processArgs :: [String] -> Flags
processArgs = processMoreArgs (applyDefaults (def :: Flags))
    where
        applyDefaults x = foldr g x (map value options)
        g (BoolFlag b f) flg = f flg b
        g (IntFlag  i f) flg = f flg i
        g (StrFlag  s f) flg = f flg s
        

processMoreArgs :: Flags -> [String] -> Flags
processMoreArgs flags args = hack98 $ applyFiles $ applyFlips flags 
    where
        (files, flips) = parseArgs args
        
        applyFlips x = foldr (\f flg -> f flg) x flips
        
        applyFiles x | length files == 0 = x
                     | length files >  1 = error "You can only compile on file at a time"
        applyFiles x = x{ sRootFile=sourcefile,  -- sSourceFile=sourcefile,
--                sTypeFile = fixTypeFile isUnix rootdir filename,
--                sObjectFile = fixObjectFile isUnix rootdir filename,
                sIncludes = (sTypeDst x) : sIncludes x}
            where
                isUnix = sUnix x
                [sourcefile] = files
                (rootdir,filename) = fixRootDir isUnix sourcefile
        
        -- apply some hacks to the flags
        hack98 x = if null (sWrapDst res)
                        then res{sWrapDst = sObjectDst x}
                        else res
            where 
                res = x{sUnderscore = sUnderscore x || h98,
                        sPuns = sPuns x && not h98}
                h98 = s98 x


printUsage :: Bool -> String
printUsage full = unlines $ 
        [ "yhc - York Haskell Compiler"
        , "A cross platform Haskell compiler"
        , ""
        , "  yhc [options] file"
        , ""
        , "file: Name of the source file to compile, i.e. main.hs"
        , "options: a list of options"
        , "  -flag           turn on an option"
        , "  -noflag         turn off an option"
        , "  -flag value     pass a value as the flag"
        ]
        ++ map showOpt opts
    where
        opts = takeWhile (\x -> full || isOpt x || usefulOpts x) allOpts
        usefulOpts (OptGroup name b) = b
        
        maxlen = maximum $ map (length . newLongName) (filter isOpt opts)
        width = 80
        prefix = 7 + maxlen
        remainder = width - prefix
        
        newLongName x = longName x ++ case value x of
                                        IntFlag _ _ -> " num"
                                        StrFlag _ _ -> " str"
                                        _ -> ""
    
        showOpt x@(Opt short _ val desc) =
                "  " ++
                (if null short then "   " else "-" ++ short ++ " ") ++
                "-" ++ long ++ (replicate (maxlen - length long + 1) ' ') ++
                a ++ concatMap (\x -> '\n' : replicate prefix ' ' ++ x) b
            where
                (a:b) = wrapOn remainder (desc ++ defaults val)
                long = newLongName x
        
        showOpt (OptGroup name _) = "\n" ++ name ++ ":"
            
        defaults (BoolFlag b _) = " (default=" ++ (if b then "on" else "off") ++ ")"
        defaults (IntFlag  i _) = " (default=" ++ show i ++ ")"
        defaults (StrFlag "" _) = ""
        defaults (StrFlag  s _) = " (default=" ++ s ++ ")"

        wrapOn i xs = a : if null b then [] else wrapOn i b
            where (a,b) = takeSpaces i xs
            
        takeSpaces i xs = if lena < i
                          then (if null b then (a,"") else (a++" "++a2,b2))
                          else ("",xs)
            where
                (a,b) = break (== ' ') xs
                (a2,b2) = takeSpaces (i-lena-1) (tail b)
                lena = length a


  

{- If first argument is True, then print second and third with formatting -}
pF :: Bool -> [Char] -> [Char] -> IO ()
pF flag title text =
  if flag 
    then hPutStr stderr ( "------\t"++title++":\n"++text++"\n") 
    else return ()
