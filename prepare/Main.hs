module Main where

import Control.Monad (guard)
import Data.Char (isAlpha, isAscii, toLower)
import Data.List.Split (splitOn)
import Data.Map qualified as M

parseLine :: String -> (String, Int)
parseLine line = (key, read (drop 1 rest))
  where
    (key, rest) = break (== ' ') line

parseFile :: String -> [(String, Int)]
parseFile = map parseLine . lines

prepList :: [(String, Int)] -> [(String, Int)]
prepList pairs = do
  (k, v) <- pairs
  guard (all isAscii k)
  w <- splitOn "-" k
  let w' = filter isAlpha w
  guard (not (null w'))
  pure (map toLower w', v)

makeMap :: [(String, Int)] -> M.Map String Int
makeMap = M.filter (>= 50) . M.fromListWith (+) . prepList

calcEntropy :: Int -> Int -> Double
calcEntropy total count = logBase 2 (fromIntegral total) - logBase 2 (fromIntegral count)

buildEntropy :: M.Map String Int -> M.Map String Double
buildEntropy m = M.map (calcEntropy total) m
  where
    total = sum (M.elems m)

inputFile :: FilePath
inputFile = "enwiki-2023-04-13.txt"

outputFile :: FilePath
outputFile = "entropy.txt"

main :: IO ()
main = do
  contents <- readFile inputFile
  let countMap = makeMap (parseFile contents)
  let entropyMap = buildEntropy countMap
  let output = unlines [w ++ " " ++ show e | (w, e) <- M.toAscList entropyMap]
  writeFile outputFile output
