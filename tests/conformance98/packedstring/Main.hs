module Main where


import Data.PackedString


main :: IO ()
main = do
  let a = packString "This is string A" 
      b = packString "This is string B"
      r = unpackPS (max a b)
  putStrLn r
  
