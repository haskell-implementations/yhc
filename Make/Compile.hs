
module Make.Compile(
    ensureYhc, ensureYhi, ensureLibraries,
    yhc, yhi, libraries
    ) where

import Make.Useful
import Make.CMake



ensureYhc = do
    b <- doesFileExist (exe_ "inst/bin/yhc")
    when (not b) yhc

ensureYhi = do
    b <- doesFileExist (exe_ "inst/bin/yhi")
    when (not b) yhi

ensureLibraries = do
    b <- doesFileExist "inst/lib/list.txt"
    when (not b) libraries


yhc = do
    rel <- liftM (== "release") $ ask "type"
    (ghc:_) <- demand ["ghc","filepath","mtl","cpphs","uniplate"]
    demandExtra "compiler" "http://darcs.haskell.org/york-compiler98/"

    let obj = "obj/yhc/" ++ if rel then "release" else "normal"
    ensureDirectory "obj/yhc" >> ensureDirectory obj

    systemSuccess "ERROR: yhc failed to compile" $
        "ghc extra/compiler/MainYhc.hs --make " ++
        "-iextra/compiler -isrc/libraries/core " ++
        "-odir " ++ obj ++ " -hidir " ++ obj ++ " -o " ++ exe_ "inst/bin/yhc" ++
        if rel then " -O" else ""


yhi = do
    ensureInst "bin"
    createConfigure
    cmake "yhi" "src/runtime/BCKernel" "inst/bin/yhi"


createConfigure = do
    let dest = "src/runtime/BCKernel/config.h"
        src = "Make/src/config.h"
    rebuild <- shouldBuild src dest
    same <- if rebuild then return False else do
        txt <- readFile dest
        let (line0:line1:_) = lines txt
        (line2,_) <- replaceVars line0
        return $ line1 == line2
    
    when (not same) $ do
        txt <- readFile src
        (txt2, used) <- replaceVars txt
        let used2 = snub used
            line0 = unwords $ "/*" : map (\(a,b) -> a ++ "=" ++ "%(" ++ a ++ ")") used2 ++ ["*/"]
            line1 = unwords $ "/*" : map (\(a,b) -> a ++ "=" ++ b) used2 ++ ["*/"]
        writeFile dest (line0 ++ "\n" ++ line1 ++ "\n" ++ txt2)


replaceVars :: String -> IO (String, [(String,String)])
replaceVars str = do
    let (skip,deal) = break (== '%') str
    if null deal then return (skip,[]) else do
        let (a,_:b) = break (== ')') $ drop 2 deal
        val <- ask a
        val <- return $ if null val then "0" else val
        (str,bind) <- replaceVars b
        return (skip++val++str, (a,val):bind)


libraries = ensureInst "lib"



ensureInst x = ensureDirectory "inst" >> ensureDirectory ("inst/" ++ x)


demandExtra folder repo = do
    let out = "extra" </> folder
    b <- doesDirectoryExist out
    when (not b) $ do
        ensureDirectory "extra"
        [darcs] <- demand ["darcs"]
        systemSuccess "failed to download extra" $
            darcs ++ " get --partial --repo-name=extra/" ++ folder ++ " " ++ repo
