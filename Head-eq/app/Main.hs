-- Solve the Heat Equation @mino2357
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where

import Lib
import Data.Array.Repa (Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import Data.Array.Repa.Stencil as R
import Data.Array.Repa.Stencil.Dim2 as R

type Vector1d = R.Array R.U R.DIM1 Double

------------ Parameter ------------
-- 分割数
nDiv :: (Integral a) => a
nDiv = 4
-- 左端
xMin :: (Floating a) => a
xMin = 0.0
-- 右端
xMax :: (Floating a) => a
xMax = pi
-- 時間刻み
dt :: (Floating a) => a
dt = 0.001
-- 拡散係数
d :: (Floating a) => a
d = 1.0
-- よく使う定数
r :: (Floating a) => a
r = d * dt / (dx * dx)
  where
    dx :: (Floating a) => a
    dx = (xMax - xMin) / fromIntegral nDiv
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

zero :: Vector1d
zero = R.fromListUnboxed (Z:.1) [0.0]

-- stencilとかいうやつ
sten2 :: R.Stencil R.DIM2 Double
sten2 = R.makeStencil (Z :. 3 :. 0)
  (\ix -> case ix of
    Z :. -1 :. _ -> Just r
    Z :.  0 :. _ -> Just (1.0 - 2.0*r)
    Z :.  1 :. _ -> Just r
    _            -> Nothing)

-- stencilとかいうやつ
sten :: R.Stencil R.DIM1 Double
sten = R.makeStencil (Z :. 3)
  (\ix -> case ix of
    Z :. -1 -> Just r
    Z :.  0 -> Just (1.0 - 2.0*r)
    Z :.  1 -> Just r
    _       -> Nothing)

-----------------

-- timeDev :: Vector1d -> Vector1d
-- timeDev =





u :: Vector1d
u = makeInitCondition sin xMin xMax nDiv

--u1 = R.mapStencil2 (R.BoundConst 0) sten u

--x = (R.computeUnboxedS $ R.append u u) `asTypeOf` u
x1 = R.computeUnboxedS $ R.append zero $ R.append zero u
x2 = R.computeUnboxedS $ R.append (R.append u zero) zero
x3 = R.computeUnboxedS $ R.append zero $ R.append u zero

u2 = R.computeUnboxedS $ R.zipWith (\x y -> r*x + (1-2*r)*y) (x1 R.+^ x2) x3

main :: IO ()
main = do
  print $ u2



