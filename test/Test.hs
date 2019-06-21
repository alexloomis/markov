{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TypeOperators     #-}

import Data.Ratio
import Test.Framework

import Markov
import Markov.Example

main = htfMain htf_thisModulesTests

-- Examples in the documentation.
test_fromMatrix =
    assertEqual
    (chain [pure (FromMatrix 't') :: (Product Double, FromMatrix)] !! 100)
    [ (0.5060975609756099, FromMatrix 'a')
    , (0.201219512195122, FromMatrix 't')
    , (0.29268292682926833, FromMatrix 'l') ]

test_m0Simple =
    assertEqual
    (take 3 $ chain0 [Simple 0])
    [ [0]
    , [-1,1]
    , [-2,0,2] ]

test_pdSimple =
    assertEqual
    (take 3 $ chain [pure 0 :: (Product Double, Simple)])
    [ [(1.0,0)]
    , [(0.5,-1),(0.5,1)]
    , [(0.25,-2),(0.5,0),(0.25,2)] ]

test_piSimple =
    assertEqual
    (take 3 $ chain [pure 0 :: (Product Int, Simple)])
    [ [(1,0)]
    , [(1,-1),(1,1)]
    , [(1,-2),(2,0),(1,2)] ]

test_siSimple =
    assertEqual
    (chain [pure 0 :: (Sum Int, Simple)] !! 2)
    [ (2,-2), (1,-1), (1,0), (0,0), (0,1), (0,2) ]

test_HMM =
    assertEqual
    (filter (\((_,Merge xs),_) -> xs == "aaa") $ chain
    [1 >*< Merge "" >*< 1 :: Product Rational :* Merge String :* Room] !! 3)
    [ ((Product $ 3243 % 200000, Merge "aaa"),Room 1)
    , ((Product $ 9729 % 500000, Merge "aaa"),Room 2)
    , ((Product $ 4501 % 250000, Merge "aaa"),Room 3) ]

test_expLoss =
    assertEqual
    (expectedLoss [pure $ initial [1,0,3] :: (Product Double, FillBin)])
    2
