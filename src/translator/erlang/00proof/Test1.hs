module Test1 where

data Atom

newtype PID a = PID a

data ErlObj

newtype Process a = Process (a -> Process a)

foreign import erlang "hserl:identity" identity :: a -> ErlObj

foreign import erlang "hserl:hslist" hsList :: [a] -> ErlObj

foreign import erlang "hserl:atom_to_list" showAtom :: Atom -> String

foreign import erlang "erlang:list_to_atom" list_to_atom :: ErlObj -> Atom

foreign import erlang "erlang:exit" exit :: Atom -> z

foreign import erlang "hserl:spawn" spawn0 :: Boxed (Process a) -> PID a

foreign import erlang "hserl:send" send0 :: PID a -> a -> a

foreign import erlang "erlang:self" self0 :: PID a

foreign import erlang "hserl:force" force :: a -> a

mkAtom :: String -> Atom

mkAtom = list_to_atom . toErlang

class Erlang a where
  toErlang :: a -> ErlObj

instance (Erlang a) => Erlang [a] where
  toErlang = hsList . (map toErlang)

instance Erlang Int where
  toErlang = identity

instance Erlang (PID a) where
  toErlang = identity

instance Erlang Integer where
  toErlang = identity

instance Erlang Char where
  toErlang = identity

instance Erlang Atom where
  toErlang = identity

instance Show Atom where
  show = showAtom

data ErlApp = ErlApp

data Boxed a = Boxed a

lpar :: (Erlang a) => (String, String) -> a -> ErlApp

lpar (m, f) arg = mkEApp (mkAtom m) (mkAtom f) (toErlang arg)

foreign import erlang "hserl:mkeapp" mkEApp :: Atom -> Atom -> ErlObj -> ErlApp

comma :: (Erlang a) => ErlApp -> a -> ErlApp

comma app arg = addArg app (toErlang arg)

foreign import erlang "hserl:addarg" addArg :: ErlApp -> ErlObj -> ErlApp

type CPS c a = (a -> c) -> c

rpar :: ErlApp -> CPS z ErlObj

rpar a k = k (force (erlCall a))

foreign import erlang "hserl:erlcall" erlCall :: ErlApp -> ErlObj

infixl 0 `lpar`, `comma`, `rpar`

spawn f = \k -> k (force (spawn0 $ Boxed f))

self k = k (force self0)

p `send` m = \k -> k (send0 p m)

data PingPong = Finished | Ping (PID PingPong) | Pong

main :: Int -> Atom

main nn = ("io", "format") `lpar` "Main: Starting ping-pong " ++ 
                                  show nn ++ " times\n" `rpar` \_ ->

          spawn pong $ \p ->

          spawn (ping p nn) $ \_ ->

          ("io", "format") `lpar` "Main: Finishing\n" `rpar` \_ -> 

          mkAtom "ok"

pong :: Process PingPong

pong = ("io", "format") `lpar` "Pong started\n" `rpar` \_ -> 
  Process pong' where
    pong' = \msg ->
      case msg of
        Finished -> ("io", "format") `lpar` "PONG: Finished\n" `rpar` \_ ->
                    exit (mkAtom "ok")
        Ping p -> ("io", "format") `lpar` "PONG: Ping!!!\n" `rpar` \_ ->
                  p `send` Pong $ \_ ->
                  Process pong'
        _ -> error "Pong"
        


ping :: PID PingPong -> Int -> Process PingPong

ping _ n | n <= 0 = exit (mkAtom "ok")

ping p n = ("io", "format") `lpar` "Ping started\n" `rpar` \_ -> 
  self $ \ps ->
  p `send` (Ping ps) $ \_ ->
  Process (ping' (n - 1)) where
    ping' n = \msg -> case msg of
      Pong  ->
        ("io", "format") `lpar` "PING: Pong!!!\n" `rpar` \_ -> 
        if (n <= 0)
          then p `send` Finished $ \_ ->
               ("io", "format") `lpar` "PING: Finished\n" `rpar` \_ ->
               exit (mkAtom "ok")
          else self $ \ps ->
               p `send` (Ping ps) $ \_ ->
               ("io", "format") `lpar` "PING: " ++ show n ++ " to go\n" `rpar` \_ ->
               Process $ ping' (n - 1)
      _ ->
        ("io", "format") `lpar` "PING: Wrong!!!\n" `rpar` \_ ->
        ("erlang", "exit") `lpar` p `comma` mkAtom "kill" `rpar` \_ ->
        exit (mkAtom "ok")

fuse :: [a] -> [a] -> [a]

fuse [] z = z
fuse z [] = z
fuse (x:xs) (y:ys) = x:y:(fuse xs ys)

fac :: Int -> Int

fac 0 = 0
fac 1 = 1
fac n = n * fac (n - 1)



