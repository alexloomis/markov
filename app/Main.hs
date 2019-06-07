{-# LANGUAGE FlexibleContexts #-}
module Main where

import Markov
import Examples
import Math.Combinatorics.Exact.Binomial (choose)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.List (intercalate)

data Input = Input
    { items :: Int
    , bins  :: Int }

options :: Parser Input
options = Input
    <$> option auto
        (  long "items"
        <> short 'i'
        <> metavar "INT"
        <> help "Length of sequence to be binned" )
    <*> option auto
        (  long "bins"
        <> short 'b'
        <> metavar "INT"
        <> help "Number of bins to be sorted into"
        )

run :: Input -> IO ()
run (Input n b) = print $ (runLoss n b :: Double)
--run _ = putStrLn ""

main :: IO ()
main = do
    cmdOpts <- execParser opts
    run cmdOpts
    where opts = info (options <**> helper)
              (  fullDesc
              <> progDesc "Gives the exact expected loss" )

readable :: Show a => [a] -> IO ()
readable = putStrLn . intercalate "\n" . fmap show

initials :: (Integral a, Fractional b) => a -> a -> [[Int]] -> [MProd b FillBin]
initials n b = map (\x -> MProd (1/(fromIntegral $ n `choose` b)) (initial x))

-- FillBins corresponding to partitions created by picking b items from n items.
partitions :: Int -> Int -> [[Int]]
partitions n b = filter (\x -> sum x == n-b) $ iterate grow [[]] !! (b+1)
    where grow z = [ x:y | x <- [0..n-b], y <- z ]

-- The expected loss given there are n items and b bins.
-- runLoss :: Markov a FillBin => Int -> Int -> a
runLoss n b = expectedLoss . initials n b $ partitions n b
