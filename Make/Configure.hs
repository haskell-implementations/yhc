
module Make.Configure(configure, ensureConfigure) where

import Make.Profile
import Make.Useful
import Make.Depends
import System.Info


ensureConfigure :: IO ()
ensureConfigure = do
    valid <- ask "valid"
    when (null valid) configure


typeSizes = ["char","short","int","long","long long","float","double","void*"]


configure :: IO ()
configure = do
        conf "os" getOS
        conf "arch" getArch

        confProg "ghc"      $ version [6,6]
        confProg "ghc-pkg"  $ version [6,4,1]
        confProg "haddock"  $ version [0,8]
        confProg "darcs"    $ version [1,0,4]
        confProg "hscolour" $ version [1,0,4]

        conf "bigendian" getEndian
        mapM (\x -> conf (filter isAlpha x) (getTypeSize x)) typeSizes

        confHsPackage "filepath" [1,0]
        confHsPackage "cpphs"    [1,3]
        confHsPackage "mtl"      [1,0]
        confHsPackage "uniplate" [1,0]

        setProfile "valid" "1"
        saveProfile
    where
        conf name val = do
            ans <- ask ('!':name)
            res <- if null ans then val else return ans
            putStrLn $ "set " ++ name ++ "=" ++
                       (if null res then " (blank)" else res) ++
                       (if null ans then "" else " (user-supplied)")
            setProfile name res

        confProg name test = do
            ans <- ask ('!':name)
            let res = if null ans then name else ans
            b <- test name res
            conf name (return $ if b then res else "")
        
        confHsPackage name test = do
            b <- checkHsPackage name test
            b <- if b then return b else do
                    b2 <- dependsHsPackage name
                    if b2 then checkHsPackage name test else return False
            conf name (return $ if b then "1" else "")



getOS :: IO String
getOS = return $ if os == "mingw32" then "win" else os

getArch :: IO String
getArch = return arch



-- give an error message if the program given has a lower version
-- or doesn't run
version :: [Int] -> String -> String -> IO Bool
version ver name exe = do
    -- can't use System.Process because WinHugs 2006
    let output = "obj/configure_" ++ name ++ ".txt"
    systemPipeCont (exe ++ " --version") output
        (do putStrLn $ "WARNING: " ++ name ++ " failed to run: " ++ exe ++ " --version"
            return False)
        $ \s -> do
            let real = parseVersion s
                success = real >= ver
            when (not success) $ putStrLn $
                "WARNING: " ++ name ++ " too old, expected " ++ showVersion ver ++
                ", found " ++ showVersion real ++ ": " ++ exe
            return success


showVersion :: [Int] -> String
showVersion = concat . intersperse "." . map show

parseVersion :: String -> [Int]
parseVersion = map read . words . map (\x -> if x == '.' then ' ' else x) .
               takeWhile (\x -> isDigit x || x == '.') . dropWhile (not . isDigit)


getEndian :: IO String
getEndian = runC "endian" "Make/src/endian.c" []


getTypeSize :: String -> IO String
getTypeSize name = runC nam "Make/src/typesize.c" [("TYPE",name)]
    where nam = filter isAlpha name




-- run a C program, return the first line
runC :: String -> FilePath -> [(String,String)] -> IO String
runC name file defines = do
    let nam = "obj/configure_" ++ name
        defs = unwords ["-optc \"-D" ++ a ++ ['='|not (null b)] ++ b ++ "\"" | (a,b) <- defines]
    ghc <- ask "ghc"
    if null ghc
        then do putStrLn $ "WARNING: " ++ name ++ " skipping, no ghc found" 
                return []
        else systemSuccessCont
            (ghc ++ " " ++ defs ++ " " ++ file ++ " -o " ++ nam ++ " -odir obj")
            (do putStrLn $ "WARNING: " ++ name ++ " failed to compile test" ; return "") $ do
            systemPipeCont (slashes nam) (nam ++ ".txt")
                (do putStrLn $ "WARNING: " ++ name ++ " failed to run test" ; return "") return


loadPackageList :: String -> IO [(String,[Int])]
loadPackageList name = do
    ghc_pkg <- ask "ghc-pkg"
    systemPipeCont (ghc_pkg ++ " list --simple-output") ("obj/package_" ++ name ++ ".txt")
        (do putStrLn $ "WARNING: ghc-pkg could not be queried: " ++ ghc_pkg; return [])
        (return . map f . filter ('-' `elem`) . words)
    where
        f x = (reverse b, parseVersion $ reverse a)
            where (a,_:b) = break (== '-') $ reverse x


checkHsPackage :: String -> [Int] -> IO Bool
checkHsPackage name ver = do
    pkg <- loadPackageList name
    let real = maximum $ [] : [b | (a,b) <- pkg, a == name]
    if null real then do
        when (not $ null pkg) $ putStrLn $ "WARNING: package " ++ name ++ " not found"
        return False
     else if real < ver then do
        putStrLn $ "WARNING: package " ++ name ++ " is too old, expected " ++ showVersion ver ++ ", found " ++ showVersion real
        return False
     else
        return True
