{-# LANGUAGE NoMonomorphismRestriction #-}

import Type.DefArgs

test = defargs2 (\x y -> x ++ ", " ++ y ++ "!") "hello" "world"
test2 = defargs2 (+) (10 :: Int) 100 -- the type signature is necessary

main = do
  putStrLn $ test Def Def
  putStrLn $ test "good morning" Def
  putStrLn $ test Def "kitty"
  putStrLn $ test "oh" "yeah"
  print $ test2 (90 :: Int) Def -- the type signature is necessary
