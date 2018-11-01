-- Simple test of arithmetic expressions

-- Commit this over firewall
--
-- Commit another one

module Test1 where

import UnsafeJS

pr :: Int -> Int

pr x = x - 1

factorial :: Int -> Int

factorial 0 = 0
factorial 1 = 1
factorial n | n > 0 = n * (factorial (pr n))
            | n <= 0 = -999

sumlst :: [Int] -> Int
sumlst [] = 0
sumlst (a:as) = a + sumlst as

tak :: Int -> [a] -> [a]

tak 0 _ = []
tak n (a:as) = a : tak (pr n) as

inf :: [Int]
inf = 1 : inf

str2 = 'a':'c':'d':"abyrvalg"

lgth :: [a] -> Int

lgth [] = 0
lgth (a:as) = 1 + lgth as

hd [] = error "head of empty list"
hd (a:as) = a;

em1 :: [Int]
em1 = []

em2 :: String
em2 = []

em3 :: [Char]
em3 = []

em4 :: String
em4 = ""

s5 :: String
s5 = 'g':[]

s6 :: String
s6 = 'd':""

s7 :: String
s7 = "ertyu"

data D = D {f::(Int -> Int -> Int)}

tup = (D (+),D (*))

res = f (fst tup) (2::Int) (3::Int)

intStatus :: Int -> Int

intStatus a = unsafeJS "window.status = exprEval(a); return a;"

anyStatus :: a -> a

anyStatus a = unsafeJS "window.status = exprEval(a).toSource(); return a;"

main = anyStatus (4 + 6)
-- main = anyStatus (["aaa","bbb",s7 ++ (hd s7):(hd s6):str2])

