-- Solve the Heat Equation @mino2357
{-# LANGUAGE TypeOperators #-}
module Main where

import Lib
--import qualified Data.Array.Repa as R
import Data.Array.Repa as R

type Vector1d = R.Array R.U R.DIM1 Double

------------ Parameter ------------
nDiv :: (Integral a) => a
nDiv = 4

xBegin :: (Floating a) => a
xBegin = 0.0

xEnd :: (Floating a) => a
xEnd = pi
-----------------------------------

-- 区間[st, ed]に対して函数fを適用した函数を初期条件とする．分割数はnDiv．
makeInitCondition :: (Double -> Double) -> Double -> Double -> Int -> Vector1d
makeInitCondition f st ed num = R.fromListUnboxed (Z:.(nDiv+1)) $ makeList f st ed num
  where
    makeList :: (Floating a, Integral b) => (a -> a) -> a -> a -> b -> [a]
    makeList f st ed num = fmap f $ makeInterval st ed num
    makeInterval :: (Floating a, Integral b) => a -> a -> b -> [a]
    makeInterval st ed num = [st + dx * fromIntegral i | i<-[0..num]]
      where dx = (ed - st) / fromIntegral num

main :: IO ()
main = do
  print $ makeInitCondition sin xBegin xEnd nDiv
