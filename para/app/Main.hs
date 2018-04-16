module Main where
import qualified Data.Foldable as List
import qualified Control.Parallel.Strategies as PS

coeffA :: (Floating a) => a
coeffA = 3.97

initX :: (Floating a) => a
initX = 0.1

stepDev :: (Floating a) => a -> a
stepDev n = coeffA * n * (1.0 - n)

stepDev' :: (Floating a) => a -> a
stepDev' n = coeffA * (n * (1.0 - n))


main :: IO ()
main = do
  -- 函数適用
  let x = PA.rpar List.foldr ($) initX (replicate 10000 stepDev)
  print $ List.foldr ($) initX (replicate 10000 stepDev')
  
  -- 函数合成
  print $ (List.foldr (.) id (replicate 10000 stepDev)) initX
  print $ (List.foldl (.) id (replicate 10000 stepDev)) initX
  print $ (List.foldl' (.) id (replicate 10000 stepDev)) initX
  print $ (List.foldr (.) id (replicate 10000 stepDev')) initX
  print $ (List.foldl (.) id (replicate 10000 stepDev')) initX
  print $ (List.foldl' (.) id (replicate 10000 stepDev')) initX

