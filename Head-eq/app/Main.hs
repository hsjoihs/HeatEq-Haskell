-- Solve the Heat Equation @mino2357
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where

import Data.Array.Repa (Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import Data.Array.Repa.Stencil as R
import Data.Array.Repa.Stencil.Dim2 as R
import Data.Array.Repa.Slice as R

-- Unboxed Array Type
type Vector1dU = R.Array R.U R.DIM1 Double
-- Delay Array Type
type Vector1dD = R.Array R.D R.DIM1 Double

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
dt = 0.01
-- 拡散係数
d :: (Floating a) => a
d = 1.0
-- よく使う定数
dr :: (Floating a) => a
dr = d * dt / (dx * dx)
  where
    dx :: (Floating a) => a
    dx = (xMax - xMin) / fromIntegral nDiv
-----------------------------------

-- 区間[st, ed]に対して函数fを適用した函数を初期条件とする．分割数はnDiv．
makeInitCondition :: (Double -> Double) -> Double -> Double -> Int -> Vector1dU
makeInitCondition f st ed num = R.fromListUnboxed (Z:.(nDiv+1)) $ makeList f st ed num
  where
    makeList :: (Floating a, Integral b) => (a -> a) -> a -> a -> b -> [a]
    makeList f st ed num = fmap f $ makeInterval st ed num
    makeInterval :: (Floating a, Integral b) => a -> a -> b -> [a]
    makeInterval st ed num = [st + dx * fromIntegral i | i<-[0..num]]
      where dx = (ed - st) / fromIntegral num

-- stencilとかいうやつ
sten2 :: R.Stencil R.DIM2 Double
sten2 = R.makeStencil (Z :. 3 :. 0)
  (\ ix -> case ix of
    Z :. -1 :. _ -> Just dr
    Z :.  0 :. _ -> Just (1.0 - 2.0 * dr)
    Z :.  1 :. _ -> Just dr
    _            -> Nothing)

-- stencilとかいうやつ
sten :: R.Stencil R.DIM1 Double
sten = R.makeStencil (Z :. 3)
  (\ ix -> case ix of
    Z :. -1 -> Just dr
    Z :.  0 -> Just (1.0 - 2.0 * dr)
    Z :.  1 -> Just dr
    _       -> Nothing)


u :: Vector1dU
u = makeInitCondition sin xMin xMax nDiv


--timeDev :: Vector1dU -> Vector1dD
--timeDev u =
timeDevSub :: Vector1dU -> Vector1dD
timeDevSub u =
  dr >< u1 R.+^ (1.0 - 2.0 * dr) >< u2 R.+^ dr >< u3
  where
    zero = R.fromListUnboxed (Z :. 1) [0.0]
    u1 = u    R.++ zero R.++ zero
    u2 = zero R.++ u    R.++ zero
    u3 = zero R.++ zero R.++ u
    -- Scalar multiplication in Repa
    infixl 7 ><
    (><) x v = R.map (* x) v

timeDev :: Vector1dU -> Vector1dD
timeDev u = zero R.++ (R.extract (Z :. 2) (Z :.(nDiv-1)) $ timeDevSub u) R.++ zero
  where
    zero = R.fromListUnboxed (Z :. 1) [0.0]

--debug :: Vector1dU -> Vector1dD
--debug u = 
--  dr >< u1 R.+^ (1.0 - 2.0 * dr) >< u2 R.+^ dr >< u3
--  where
--    zero = R.fromListUnboxed (Z :. 1) [0.0]
--    u1 = u    R.++ zero R.++ zero
--    u2 = zero R.++ u    R.++ zero
--    u3 = zero R.++ zero R.++ u
--    -- Scalar multiplication in Repa
--    infixl 7 ><
--    (><) x v = R.map (* x) v

main :: IO ()
main = do
  print $ makeInitCondition sin xMin xMax nDiv
  print $ R.computeUnboxedS $ timeDevSub u
  print $ R.computeUnboxedS $ timeDev u












