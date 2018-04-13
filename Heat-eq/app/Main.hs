-- Solve the Heat Equation @mino2357

module Main where
import Lib
import qualified Data.List as List
import qualified Data.Foldable as List

import qualified Control.Parallel.Strategies as PS

x :: Double
x = 1.0

y :: Double
y = 2.0

para :: Double
para = PS.runEval $ do
  px1 <- PS.rpar $ List.foldr' ($) initX (replicate 1000000 stepDev)
  px2 <- PS.rpar $ List.foldr' ($) initX (replicate 2000000 stepDev)
  px3 <- PS.rpar $ List.foldr' ($) initX (replicate 3000000 stepDev)
  px4 <- PS.rpar $ List.foldr' ($) initX (replicate 4000000 stepDev)
  px5 <- PS.rpar $ List.foldr' ($) initX (replicate 5000000 stepDev)
  px6 <- PS.rpar $ List.foldr' ($) initX (replicate 6000000 stepDev)
  px7 <- PS.rpar $ List.foldr' ($) initX (replicate 7000000 stepDev)
  px8 <- PS.rpar $ List.foldr' ($) initX (replicate 8000000 stepDev)
  PS.rseq px1
  PS.rseq px2
  PS.rseq px3
  PS.rseq px4
  PS.rseq px5
  PS.rseq px6
  PS.rseq px7
  PS.rseq px8
  return (px1+px2+px3+px4+px5+px6+px7+px8)

main :: IO ()
main = do
  -- 函数適用
  print $ para
  --putStrLn $ PS.runEval $ PS.rpar $ List.foldr ($) initX (replicate 10000 stepDev)
  --print $ PS.rpar x
  --let y = List.foldr ($) initX (replicate 10000 stepDev)
  
  --print $ List.foldr ($) initX (replicate 10000 stepDev')
  --mapM_ print $ R.toList $ List.foldr' ($) u (replicate 100 timeDev)
  
  -- 函数合成 実行注意
  --mapM_ print $ R.toList $ (List.foldr' (.) id (replicate 1000 timeDev)) u
  --print $ (List.foldr (.) id (replicate 10000 stepDev)) initX
  --print $ (List.foldl (.) id (replicate 10000 stepDev)) initX
  --print $ (List.foldl' (.) id (replicate 10000 stepDev)) initX
  --print $ (List.foldr (.) id (replicate 10000 stepDev')) initX
  --print $ (List.foldl (.) id (replicate 10000 stepDev')) initX
  --print $ (List.foldl' (.) id (replicate 10000 stepDev')) initX
