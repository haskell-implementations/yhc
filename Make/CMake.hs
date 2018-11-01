
module Make.CMake(cmake) where

import Make.Useful
import qualified Data.Map as Map
import System.Time


type Files = Map.Map String (CalendarTime, [FilePath])
type Dates = Map.Map String CalendarTime


cmake :: String -> FilePath -> FilePath -> IO ()
cmake name dir output = do
    ghc <- ask "ghc"
    (_, allfiles) <- getContentsRec dir
    cfiles <- return $ filter (not . isPrefixOf "hat/") $ filter (".c" `isSuffixOf`) allfiles
    
    -- ensure that each file is in the list
    files <- loadFiles name
    files <- removeFiles dir files
    files <- addFiles dir files cfiles
    saveFiles name files
    let dates = dateMap files

    ensureDirectory $ "obj" </> name
    obj <- mapM (buildFile dir ("obj" </> name) dates) cfiles
    systemSuccess ("ERROR: " ++ output ++ " failed to link") $
        ghc ++ " " ++ unwords obj ++ " -o " ++ output
    return ()


buildFile :: String -> FilePath -> Dates -> FilePath -> IO FilePath
buildFile dir output dates file = do
    ghc <- ask "ghc"
    let out = output </> takeFileName file ++ ".o"
    done <- doesFileExist out
    done <- if not done then return False else do
        let lastTime = Map.lookup file dates
        time <- getModificationTime out
        time <- toCalendarTime time
        return $ isJust lastTime && time > fromJust (lastTime)

    when (not done) $ do
        putStrLn $ "Compiling " ++ file
        systemSuccess ("ERROR: " ++ file ++ " failed to compile") $
            ghc ++ " " ++ (dir </> file) ++ " -Isrc/runtime/BCKernel -Isrc/runtime/BCKernel/msvc/gmp" ++
                   " -Idepends/ctypes/libffi_msvc -c -o " ++ out
    return out



-- for each file give the newest file it depends on
-- be stupid, just find a fixed point
-- also handles circular dependancies correctly
dateMap :: Files -> Map.Map String CalendarTime
dateMap files = fix (Map.map fst files)
    where
        fix dates = if dates == dates2 then dates else fix dates2 
            where dates2 = Map.mapWithKey (calc dates) dates

        calc dates file time = maximum (time : map f deps)
            where
                f dep = Map.findWithDefault time dep dates
                deps = snd $ Map.findWithDefault (time,[]) file files 


removeFiles :: String -> Files -> IO Files
removeFiles dir files = filterM check (Map.toAscList files) >>= return . Map.fromAscList
    where
        check (file, (modify,_)) = do
            exists <- doesFileExist (dir </> file)
            if not exists then return False else do
                time <- getModificationTime (dir </> file)
                time <- toCalendarTime time
                return $ time == modify



addFiles :: String -> Files -> [FilePath] -> IO Files
addFiles dir files [] = return files
addFiles dir files (t:odo)
    | t `Map.member` files = addFiles dir files odo
    | otherwise = do
        time <- getModificationTime (dir </> t)
        time <- toCalendarTime time
        incs <- readIncludes dir t
        addFiles dir (Map.insert t (time,incs) files) (incs++odo)


-- from a file, what does it include
readIncludes :: FilePath -> FilePath -> IO [FilePath]
readIncludes dir file = readFile (dir </> file) >>= return . map relative . concatMap (f 0) . lines
    where
        f 0 x | "#" `isPrefixOf` x2 = f 1 $ trim $ tail x2
            where x2 = ltrim x

        f 1 x | "include " `isPrefixOf` x = f 2 $ drop 8 x

        f 2 x | n >= 3 && head x == '\"' && last x == '\"' = [take (n-2) $ tail x]
            where n = length x

        f _ _ = []
        
        relative inc = useSlash $ unwords $ collapse $ words $ useSpace $ dropFileName file </> inc
            where
                useSlash = replace ' ' '/'
                useSpace = replace '/' ' '
                
                collapse (x:"..":xs) = collapse xs
                collapse (x:xs) = x : collapse xs
                collapse [] = []
        


loadFiles :: String -> IO Files
loadFiles name = do
    let file = "obj/cmake_cache_" ++ name ++ ".txt"
    b <- doesFileExist file
    if b then readFile file >>= return . read else return Map.empty


saveFiles :: String -> Files -> IO ()
saveFiles name files = do
    let file = "obj/cmake_cache_" ++ name ++ ".txt"
    writeFile file (show files)
