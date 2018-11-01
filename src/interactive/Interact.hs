-- This module contains routines to provide an interactive shell prompt and is
-- built on top of the readline library.

module Interact where

import Char
import Control.Monad.Identity
import List
import qualified Data.Map as Map
import System
import System.Directory
import Directory
import IO 

import GenUtil

#ifdef LIBREADLINE
import System.Console.Readline

readLine :: String -> (String -> IO [String]) -> IO String
readLine prompt tabExpand =  do
    setCompletionEntryFunction (Just (\s -> tabExpand s))
    s <- readline prompt
    case s of
        Nothing -> putStrLn "Bye!" >> exitSuccess
        Just cs | all isSpace cs -> return ""
        Just s -> addHistory s >> return s
#endif

flushConsole :: IO ()
flushConsole = hFlush stdout

--simpleCommand :: String -> IO (Maybe String)

commands = [
    (":quit","quit interactive session"),
    (":version","print out version number"),
    (":cd", "change directory to argument"),
    (":pwd", "show current directory"),
    (":set", "set options"),
    (":unset", "unset options"),
    (":execfile", "run sequence of commands from a file"),
    (":help", "print help table"),
    (":import", "import modules")
    ]

extra_help = [
    ("!command", "run shell command")
    ]



basicParse :: String ->  Either (String,String) String
basicParse s = f s' where
    s' = reverse $ dropWhile isSpace (reverse $ dropWhile isSpace s)
    f (':':rs) = Left (':':map toLower as,dropWhile isSpace rest) where
        (as,rest) = span isAlpha rs
    f _ = Right s'

data InteractCommand = InteractCommand {
    commandName :: String,
    commandHelp :: String,
    commandAction :: Interact -> String -> String -> IO Interact
    }

data Interact = Interact {
    interactPrompt :: String,               -- ^ the prompt to use
    interactCommands :: [InteractCommand],  -- ^ a list of commands
    interactSettables :: [String],          -- ^ possible things that may be set
    interactVersion :: String,              -- ^ version string to print
    interactSet :: Map.Map String String,   -- ^ vars that are actually set
    interactExpr :: (String -> IO ()) -> (String -> IO ()) -> String -> IO (), -- ^ what to run on a bare expression
    interactModules :: [String],
    interactWriteOut :: String -> IO (),
    interactWriteErr :: String -> IO (),
    quitNow :: Bool
    }

emptyInteract = Interact {
    interactPrompt = "> ",
    interactCommands = [],
    interactSettables = [],
    interactVersion = "(none)",
    interactSet = Map.empty,
    interactExpr = doNothing,
    interactModules = [],
    interactWriteOut = putStr,
    interactWriteErr = putStr,
    quitNow = False
    }

doNothing :: (String -> IO ()) -> (String -> IO ()) -> String -> IO ()
doNothing out err _ = do
  out "Unknown Command"


cleanupWhitespace s = reverse $ dropWhile isSpace (reverse $ dropWhile isSpace s)


doInteractions :: Interact -> [String] -> IO Interact
doInteractions act [] = return act
doInteractions act (x:xs) = do
    res <- doInteraction act x
    case (quitNow res) of 
      True -> return res
      False -> doInteractions res xs

-- | run a command as if typed at prompt

doInteraction :: Interact -> String -> IO Interact
doInteraction act s = do
    let writeOut = interactWriteOut act
        writeErr = interactWriteErr act
    let commands' = commands ++ [ (n,h) | InteractCommand { commandName = n, commandHelp = h } <- interactCommands act ]
        help_text = unlines $ buildTableLL (commands' ++ extra_help)
    let args s =  [ bb | bb@(n,_) <- commands', s `isPrefixOf` n ]
        expand s = fsts (args s) ++ filter (isPrefixOf s) (interactSettables act)

    let showSet
         | null $ interactSettables act = writeErr "Nothing may be set\n"
         | otherwise  = do
            let set = [ "  " ++ if null b then a else a ++ "=" ++ b | (a,b) <- Map.toList $ interactSet act]
                setable = [ "  " ++ a | a <- sort $ interactSettables act, not $ a `Map.member` interactSet act]
            when (not $ null set) $ writeOut "Set options:\n" >> writeOut (unlines set)
            when (not $ null setable) $ writeOut "Setable options:\n" >> writeOut (unlines setable)

    case basicParse s of
        Right "" -> return act
        Right ('!':rest) -> do System.system rest 
                               return act
        Right s -> do interactExpr act writeOut writeErr s
                      return act
        Left (cmd,arg) -> case fsts $ args cmd of
            [":quit"] -> return ( act { quitNow = True } )
            [":help"] -> do writeOut help_text
                            return act
            [":version"] -> do writeOut $ (interactVersion act) ++ "\n"
                               return act
            [":cd"] -> do catch (setCurrentDirectory arg) 
                                (\_ -> writeErr $ "Could not change to directory: " ++ arg ++ "\n") 
                          return act
            [":pwd"] -> do dir <- catch getCurrentDirectory (\_ -> writeErr "Could not get current directory.\n" >> return "")
                           writeErr $ dir ++ "\n"
                           return act
            [":import"] -> case simpleUnquote arg of 
                [] -> return act
                arg -> do let act' = act { interactModules = ((head arg):(interactModules act)) }
                          return act'
            [":set"] -> case simpleUnquote arg of
                [] -> showSet >> return act
                rs -> do
                    let ts = [ let (a,b) = span (/= '=') x in (cleanupWhitespace a,drop 1 b) | x <- rs ]
                    sequence_ [ writeErr $ "Unknown option: \n" ++ a | (a,_) <- ts, a `notElem` interactSettables act]
                    let act' = act { interactSet = Map.fromList [ x | x@(a,_) <- ts, a `elem` interactSettables act ] `Map.union` interactSet act }
                    return act'
            [":unset"] -> do let act' = act { interactSet = interactSet act Map.\\ Map.fromList [ (cleanupWhitespace rs,"") | rs <- simpleUnquote arg] }
                             return act'
            [":execfile"] -> do
                fc <- catch (readFile arg) (\_ -> writeErr "Could not read file.\n" >> return "")
                doInteractions act (lines fc)
            [m] -> let [a] =  [ a | InteractCommand { commandName = n, commandAction = a } <-  interactCommands act, n == m] in do
                act' <- a act m arg
                return act'
            (_:_:_) -> do writeOut "Ambiguous command, possibilites are:\n" 
                          writeOut  (unlines $ buildTableLL $ args cmd) >> return act
            [] -> do (writeErr $ "Unknown command (use :help for help): " ++ cmd ++ "\n")  
                     return act


-- | begin interactive interaction

userInteract :: Interact -> IO Interact
#ifdef LIBREADLINE
userInteract act = do
    let commands' = commands ++ [ (n,h) | InteractCommand { commandName = n, commandHelp = h } <- interactCommands act ]
        args s =  [ bb | bb@(n,_) <- commands', s `isPrefixOf` n ]
        expand s = fsts (args s) ++ filter (isPrefixOf s) (interactSettables act)
    s <- readLine (interactPrompt act) (return . expand)
    doInteraction act s
#endif 
#ifndef LIBREADLINE
userInteract act = do
  putStr $ interactPrompt act
  flushConsole
  input <- getLine
  doInteraction act input
#endif 
