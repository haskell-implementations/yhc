-- | Warning: many fields have comments attached, but haddock doesn't
-- seem to support docstrings on positional fields.

module Error
    (
        -- * Deprecated functions
        exit, can'tOpenStr, errorLC,
        
        -- * New error interface
        Error(..), -- standard error message
        ErrPos, noErrPos,
        raiseError,
        raiseErrors,
        
        errorRaw, -- low level
        tryReadFile, tryWriteFile
    )
    where

import List
import Char
import IO
import System
import Foreign
import Util.Extra


-- | The Error data type, most important.
--   
--   Lists all possible errors with as much information as possible.


type ErrPos = (FilePath, Pos)

noErrPos :: ErrPos
noErrPos = ("", noPos)


data Error
    -- | I was looking for a file, it isn't there
    = ErrorFileNone
        ErrPos
        String      -- reason
        FilePath    -- file you were looking for
        [FilePath]  -- where i searched
    
    -- | I was looking for one file, I found too many
    | ErrorFileMany
        (FilePath, Pos)
        String      -- reason
        FilePath    -- file you were looking for
        [FilePath]  -- what files i found
    
    | ErrorInternal
        String -- function that is giving this error
        String -- any helpful message

    -- | A newtype definition may be circular [If it may be circular, why is this an error? -SamB]
    | ErrorCircularNewtype
        String
        
    | ErrorCircularType
        [String]
    
    | ErrorConflictFixities
        String
        String
        String
    
    | ErrorUnboundTypeInstance
        String
        String
    
    | ErrorRaw String

        


showError :: Error -> [String]
showError (ErrorInternal func msg) = 
    ["INTERNAL ERROR: Please report this to <yhc -AT- haskell -DOT- org>"
    ,"Function: " ++ func
    ,"Details: " ++ msg]

showError (ErrorFileNone _ reason file paths) =
    ["Error: File not found, " ++ file, "Reason: " ++ reason, "Looked in:"] ++ map indent paths

showError (ErrorFileMany _ reason file paths) =
    ["Error: Found file multiple times, " ++ file, "Reason: " ++ reason, "Found in:"] ++ map indent paths

showError (ErrorCircularNewtype var) =
    ["Error: newtype may be circular, " ++ var]

showError (ErrorCircularType vars) =
    case vars of
        [x] -> ["Error: Circular type synonym, " ++ x]
        xs  -> ["Error: Circular dependancy between type synonyms", indent (commas xs)]
    where
        commas xs = concat $ intersperse ", " xs

showError (ErrorConflictFixities name f1 f2) =
    ["Error: Conflicting fixities, " ++ name,
     indent $ f1 ++ " and " ++ f2]

showError (ErrorUnboundTypeInstance pos name) =
        ["Error: Unbound type variable,  " ++ show name ++ " in instance at " ++ pos]

showError (ErrorRaw x) = ["Error: " ++ x]

showError x = ["no show defined for error"]



raiseError :: Error -> a
raiseError = errorRaw . showError


raiseErrors :: String -> [Error] -> a
raiseErrors stage xs = errorRaw (("-- during " ++ stage) : concatMap showError xs)



-- FIXME, remove - use proper error handling
exit :: IO a
exit = exitWith (ExitFailure 1)

-- FIXME, remove - use proper error handling
can'tOpenStr :: String -> [String] -> a -> String
can'tOpenStr name [filename] ioerror =
   "Can't open "++ filename  ++ " when trying to read "++name++".\n"
can'tOpenStr name filename ioerror =
   "Can't open any of:\n "++ concatMap (++"\n ") (nub filename)
   ++ "when trying to read "++name++".\n"

-- FIXME, remove - use proper error handling
errorLC :: Int -> Int -> String -> a
errorLC  l c msg =
  error ("Error at line "++show l ++", column " ++ show c ++ ": " ++ msg++"\n")


-- * Low level error message

errorRaw :: [String] -> a
errorRaw x = unsafePerformIO $ do
    putStr $ unlines x
    exitWith (ExitFailure 1)



-- * Higher level, call - if they fail they error

tryReadFile :: String -> FilePath -> IO String
tryReadFile reason file =
    catch (readFile file)
          (\ioerror -> errorRaw
                ["Error: Can't open" ++ r ++ " file, " ++ file
                ,"Reason: " ++ show ioerror])
    where
        r = if null reason then "" else ' ' : reason

tryWriteFile :: String -> FilePath -> String -> IO ()
tryWriteFile reason file contents =
    catch (writeFile file contents)
          (\ioerror -> errorRaw
                ["Error: Can't write" ++ r ++ " file, " ++ file
                ,"Reason: " ++ show ioerror])
    where
        r = if null reason then "" else ' ' : reason


indent :: String -> String
indent x = "  " ++ x


