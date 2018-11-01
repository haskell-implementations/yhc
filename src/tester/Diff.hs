
module Diff where


isDiff :: String -> String -> Bool
isDiff a b = a /= b


showDiff :: String -> String -> String
showDiff a b = "EXPECTED:\n" ++ a ++ "\nACTUAL:\n" ++ b

