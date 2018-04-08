-- Solve the Heat Equation @mino2357
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where

import Data.Array.Repa (Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import Data.Array.Repa.Stencil as R
import Data.Array.Repa.Stencil.Dim2 as R
import Data.Array.Repa.Slice as R
import qualified Data.List as List
import qualified Data.Foldable as List
import Data.Functor.Identity

-- Unboxed Array Type
type Vector1dU = R.Array R.U R.DIM1 Double
-- Delay Array Type
type Vector1dD = R.Array R.D R.DIM1 Double

------------ Parameter ------------
-- 分割数
nDiv :: (Integral a) => a
nDiv = 128
-- 左端
xMin :: (Floating a) => a
xMin = 0.0
-- 右端
xMax :: (Floating a) => a
xMax = pi
-- 時間刻み
dt :: (Floating a) => a
dt = (1.0 / 6.0) * dx * dx
  where
    dx :: (Floating a) => a
    dx = (xMax - xMin) / fromIntegral nDiv
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
    makeList f st ed num = f <$> makeInterval st ed num
    makeInterval :: (Floating a, Integral b) => a -> a -> b -> [a]
    makeInterval st ed num = [st + dx * fromIntegral i | i<-[0..num]]
      where dx = (ed - st) / fromIntegral num

-- 時間をdt進める函数
timeDev :: Vector1dU -> Vector1dU
timeDev u = R.computeUnboxedS $ zero R.++ R.extract (Z :. 2) (Z :.(nDiv-1)) ((runIdentity . timeDevSub) u) R.++ zero
  where
    zero = R.fromListUnboxed (Z :. 1) [0.0]
    --補助関数（というよりメイン処理）
    timeDevSub :: (Monad m) => Vector1dU -> m Vector1dU
    timeDevSub u = R.computeUnboxedP $ dr >< u1 R.+^ (1.0 - 2.0 * dr) >< u2 R.+^ dr >< u3
      where
        zero = R.fromListUnboxed (Z :. 1) [0.0]
        u1 = u    R.++ zero R.++ zero
        u2 = zero R.++ u    R.++ zero
        u3 = zero R.++ zero R.++ u
        -- Scalar multiplication in Repa
        infixl 7 ><
        (><) x = R.map (* x)

u :: Vector1dU
u = makeInitCondition sin xMin xMax nDiv

main :: IO ()
main = do
  --mapM_ print $ R.toList $ (runIdentity . timeDev) u
  mapM_ print $ R.toList $ List.foldr' ($) u (replicate 100000 timeDev)
  --mapM_ print $ R.toList $ (List.foldr' (.) id (replicate 1000000 timeDev)) u
  --putStrLn "\n"
  --mapM_ print $ R.toList $ R.computeUnboxedS $ (List.foldl' (.) id (replicate 4 timeDev')) u'
