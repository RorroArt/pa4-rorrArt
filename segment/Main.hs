module Main where

import Data.Array (Array, array, (!))
import Data.List (minimumBy)
import Data.Map qualified as M
import Data.Ord (comparing)

parseLine :: String -> (String, Double)
parseLine line = (key, read (drop 1 rest))
  where
    (key, rest) = break (== ' ') line

parseMap :: String -> M.Map String Double
parseMap = M.fromAscList . map parseLine . lines

prefixes :: M.Map String Double -> String -> [(String, Double)]
prefixes m s = do
  k <- [1 .. length s]
  let w = take k s
  Just e <- [M.lookup w m]
  pure (w, e)

segment :: M.Map String Double -> String -> [String]
segment entropyMap msg = recover 0
  where
    n :: Int
    n = length msg

    assocs :: [(Int, (Double, String))]
    assocs = map bestAt [0 .. n - 1]
      where
        bestAt :: Int -> (Int, (Double, String))
        bestAt k = (k, minimumBy (comparing fst) candidates)
          where
            candidates :: [(Double, String)]
            candidates = do
              (w, e) <- prefixes entropyMap (drop k msg)
              pure (e + entropyArr ! (k + length w), w)

    entropyArr :: Array Int Double
    entropyArr = array (0, n) $ (n, 0) : [(k, fst v) | (k, v) <- assocs]

    wordArr :: Array Int String
    wordArr = array (0, n - 1) [(k, snd v) | (k, v) <- assocs]

    recover :: Int -> [String]
    recover k
      | k >= n = []
      | otherwise = wordArr ! k : recover (k + length (wordArr ! k))

fill :: Int -> [String] -> String
fill w = unlines . map unwords . foldr f []
  where
    f word [] = [[word]]
    f word (l : ls)
      | length word + 1 + length (unwords l) <= w = (word : l) : ls
      | otherwise = [word] : l : ls

entropyFile :: FilePath
entropyFile = "entropy.txt"

inputFile :: FilePath
inputFile = "ishmael.dec"

outputFile :: FilePath
outputFile = "ishmael.txt"

main :: IO ()
main = do
  entropyMap <- parseMap <$> readFile entropyFile
  msg <- filter (/= '\n') <$> readFile inputFile
  writeFile outputFile (fill 60 (segment entropyMap msg))
