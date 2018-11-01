import NHC.FFI

{- the original expected output for this program was

   281474976710655
   1
   1048575
   -4095
   281474976710655 
   1
   1048575
   -4095

   which is clearly silly given the program below.  
   -0xffffffff is not 1 by any method of thinking.

   Tom
-}

main = do
  print value1
  print value2
  print value3
  print value4
  print (fromIntegral value1)
  print (fromIntegral value2)
  print (fromIntegral value3)
  print (fromIntegral value4)

value1,value2,value3,value4 :: Int64
value1 =  0xffffffffffff
value2 = -0xffffffff
value3 =  0xfffff
value4 = -0xfff

