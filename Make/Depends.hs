
module Make.Depends(dependsHsPackage) where

import Make.Useful


-- try and install a haskell package
-- return true if you think you managed it
dependsHsPackage :: String -> IO Bool
dependsHsPackage name = do
    depends@[darcs, ghc] <- mapM ask ["darcs","ghc"]
    if any null depends then return False else do
        putStrLn $ "NOTE: Attempting to install package " ++ name
        ensureDirectory "obj/haskell"
        removeDirectoryRecursive $ "obj/haskell/" ++ name
        curdir <- getCurrentDirectory
        prefix <- ask "prefix"
        b <- ioSequence
            [systemBool $ darcs ++ " get --partial --repo-name=obj/haskell/" ++ name ++
                          " http://www.cs.york.ac.uk/fp/yhc/dependencies/haskell/" ++ name
            ,systemBool $ ghc ++ " obj/haskell/" ++ name ++ "/Setup --make -o obj/haskell/" ++ name ++ "/setup" 
            ,setCurrentDirectory ("obj/haskell/" ++ name) >> return True
            ,systemBool $ "setup configure" ++ (if null prefix then "" else " --prefix=" ++ prefix)
            ,systemBool "setup build"
            ,systemBool "setup install"
            ]
        setCurrentDirectory curdir
        putStrLn $ if b then "NOTE: package " ++ name ++ " successfully installed"
                        else "WARNING: package " ++ name ++ " failed to install, please install manually"
        return b

