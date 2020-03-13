{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.Random (mkStdGen)
import Data.Ratio
import Test.Framework

import Markov
import Markov.Example
import Markov.Extra

main = htfMain htf_thisModulesTests

test_fromLists =
    assertEqual
    [ (0.5060975609756099, FromLists 'a')
    , (0.29268292682926833, FromLists 'l')
    , (0.201219512195122, FromLists 't') ]
    (chain [pure (FromLists 't') :: (Product Double, FromLists)] !! 100)

test_m0Simple =
    assertEqual
    [ [0]
    , [-1,1]
    , [-2,0,2] ]
    (take 3 $ chain0 [Simple 0])

test_pdSimple =
    assertEqual
    [ [(1.0,0)]
    , [(0.5,-1),(0.5,1)]
    , [(0.25,-2),(0.5,0),(0.25,2)] ]
    (take 3 $ chain [pure 0 :: (Product Double, Simple)])

test_piSimple =
    assertEqual
    [ [(1,0)]
    , [(1,-1),(1,1)]
    , [(1,-2),(2,0),(1,2)] ]
    (take 3 $ chain [pure 0 :: (Product Int, Simple)])

test_siSimple =
    assertEqual
    [ (0,0), (0,1), (0,2), (1,-1), (1,0), (2,-2) ]
    (chain [pure 0 :: (Sum Int, Simple)] !! 2)

test_HMM =
    assertEqual
    [ ((Product $ 3243 % 200000, Merge "aaa"),Room 1)
    , ((Product $ 9729 % 500000, Merge "aaa"),Room 2)
    , ((Product $ 4501 % 250000, Merge "aaa"),Room 3) ]
    (filter (\((_,Merge xs),_) -> xs == "aaa") $ chain
    [1 >*< Merge "" >*< 1 :: Product Rational :* Merge String :* Room] !! 3)

test_expLoss =
    assertEqual
    (expectedLoss [pure $ initial [1,0,3] :: (Product Double, FillBin)])
    2

test_expLossMore =
    assertEqual
    9.764425505050506
    (expectedLoss [pure $ initial [1,0,3,2,2,5] :: (Product Double, FillBin)])

test_randomPath =
    assertEqual
    (0.1648351648351649, Urn (2,13))
    (randomPath (mkStdGen 70) (Urn (2,5)) !! 8 :: (Product Double, Urn))

