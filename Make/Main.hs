
module Make.Main where

import Make.Useful
import Make.Profile
import Make.Configure
import Make.Compile
import Make.Docs


data Command = Help | Compile | Yhc | Yhi | Libraries
             | Install | Clean | Push | Configure | Docs
             | Tester | Test | TestQuick
               deriving (Enum,Eq,Bounded, Show)

aliases = [("library",Libraries)]


parseArgs :: IO Command
parseArgs = do
        args <- getArgs
        let (profiles,nonprofiles) = partition ("@" `isPrefixOf`) args
            (settings,commands) = partition ('=' `elem`) nonprofiles

        command <- liftM lookupCommand $ pickDef "command" "compile" commands
        profName <- pickDef "@profile" "" profiles
        
        loadProfile profName
        argsProfiles settings
        return command
    where
        pickDef msg def [x] = return x
        pickDef msg def []  = return def
        pickDef msg def xs  = abort ("Error, more than one " ++ msg ++ " given, " ++ show xs) >> return ""
        
        lookupCommand y = fromMaybe (error $ "Error, command not found: " ++ y) $ lookup y $ 
                          [(map toLower $ show x, x) | x <- [minBound..maxBound]] ++ aliases



main = do
    cmd <- parseArgs
    case cmd of
        Help      -> help
        Clean     -> clean
        Configure -> ensureObj >> configure
        _ -> do
            ensureObj
            ensureConfigure
            let ensureAll = ensureYhc >> ensureYhi >> ensureLibraries
            case cmd of
                Push -> push
                --Install   -> ensureAll >> install
                Compile   -> yhc >> yhi >> libraries
                Yhc       -> yhc
                Yhi       -> yhi
                Libraries -> ensureYhc >> libraries
                Docs      -> docs
                --Tester    -> tester
                --Test      -> ensureAll >> ensureTester >> test
                --TestQuick -> ensureAll >> ensureTester >> testQuick


ensureObj = ensureDirectory "obj"


help = putStrLn "DO NOT USE THIS YET, IT'S NOT READY!"

clean = removeDirectoryRecursive "obj"


push = do
    [darcs] <- demand ["darcs"]
    system $ darcs ++ " push --no-set-default neil@darcs.haskell.org:/home/darcs/yhc"
    return ()

