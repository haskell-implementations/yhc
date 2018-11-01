module Main where

import Interact
import Evaluate

import List
import IO
import Char
import GenUtil

defaultState = emptyInteract { 
                  interactExpr = evalYhc,
                  interactWriteOut = putStr,
                  interactWriteErr = putStr 
                  }

banner :: String
banner = unlines
    ["  _  _ _   _  ___"
    ," ( \\| | |_| |/ __|  The York Haskell Evaluator"
    ,"  \\_  |  _  |\\ \\    (c) 2005 The Yhc Team"
    ,"   _| | | | |/ /_"
    ,"  (__/|_| |_|\\___|  http://www.haskell.org/haskellwiki/Yhc"
    ]

main = do putStrLn banner
          run defaultState

run :: Interact -> IO ()
run act = do
  res <- userInteract act
  case (quitNow res) of
    True -> do putStrLn "Bye!" 
               exitSuccess
    False -> run res

