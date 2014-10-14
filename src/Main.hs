{-# LANGUAGE TypeOperators, FlexibleContexts #-}

import Type.DefArgs

test :: (s =? String, s' =? String) => s -> s' -> String
test = defargs2 (\x y -> x ++ ", " ++ y ++ "!") "hello" "world"

test2 :: (i =? Int, i' =? Int) => i -> i' -> Int
test2 = defargs2 (+) 10 100

main = do
  putStrLn $ test Def Def
  putStrLn $ test "good morning" Def
  putStrLn $ test Def "kitty"
  putStrLn $ test "oh" "yeah"
  print $ test2 (90 :: Int) Def
