
module Make.Docs(docs) where

import Make.Useful



docs :: IO ()
docs = do
    [haddock] <- demand ["haddock"]
    [ghc,hscolour] <- request ["ghc","hscolour"]
    
    ensureDirectory "inst"
    ensureDirectory "inst/docs"
    ensureDirectory "obj/docs"
    
    as <- sequence
            [mirrorDocs ghc hscolour "src/compiler98"
            ,mirrorDocs ghc hscolour "src/libraries/core"
            ,mirrorDocs ghc hscolour "src/libraries/general"]

    when (not $ null hscolour) $ do
        b <- shouldBuild  "misc/hscolour.css" "inst/docs/hscolour.css"
        when b $ copyFile "misc/hscolour.css" "inst/docs/hscolour.css"

    force <- ask "force"
    when (or as || not (null force)) $
        systemSuccess "ERROR: documentation could not be created" $ unwords $
            [haddock,"--html","--odir=inst/docs","--title=Yhc","--prologue=misc/haddock_prefix.txt"
            ,"--source=http://www.cs.york.ac.uk/fp/darcs/yhc-devel/src/compiler98/"
            ,"inst/docs/*.hs", if null ghc then "" else " obj/docs/*.hs"
            ,"--source-module=%{MODULE}.hs.html","--source-entity=%{MODULE}.hs.html#%{NAME}"
            ]


mirrorDocs :: String -> FilePath -> FilePath -> IO Bool
mirrorDocs ghc hscolour dir = do
    files <- liftM snd $ getContentsRec dir
    let hs  = filter (".hs"  `isSuffixOf`) files
        lhs = filter (".lhs" `isSuffixOf`) files

    a <- mapM move (lhs ++ hs)
    b <- if null hscolour then return [] else mapM color (lhs ++ hs)
    c <- if null ghc then return [] else mapM unlhs lhs
    return $ or $ a ++ b ++ c
    where
        flatten = replace '/' '.'

        move x = do
            let src = dir </> x
                dest = "inst/docs" </> flatten x
            b <- shouldBuild src dest
            when b $ putChar '.' >> copyFile src dest
            return b

        unlhs x = do
            let src = dir </> x
                dest = "obj/docs" </> dropExtension (flatten x) ++ ".hs"
            b <- shouldBuild src dest
            when b $ putChar '.' >> unlit ghc src dest
            return b

        color x = do
            let src = dir </> x
                dest = "inst/docs" </> dropExtension (flatten x) ++ ".hs.html"
            b <- shouldBuild src dest
            when b $ putChar '.' >> colourise hscolour src dest
            return b


colourise :: String -> FilePath -> FilePath -> IO ()
colourise hscolour src dest = do
    b <- systemBool $ hscolour ++ " -anchor -css " ++ src ++ " > " ++ dest
    when (not b) $ do
        removeFile dest
        abort $ "ERROR: " ++ src ++ " failed with HsColour preprocessing"


unlit :: String -> FilePath -> FilePath -> IO ()
unlit ghc src dest = systemSuccess ("ERROR: " ++ src ++ " failed with GHC preprocessing") $
    -- note, must pass -cpp or droppings will be left behind
    ghc ++ " -cpp -optP-P -E " ++ src ++ " -o " ++ dest
