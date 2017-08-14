module Main where

import System.IO
import Data.Ord
import Data.List
import Data.Tuple
import Data.Maybe
import Control.Exception.Base

letters :: [Char]
letters = "abcdefghijklmnopqrstuvwxyz"

countAppearance :: [Char] -> Char -> (Int, Char)
countAppearance t s = (length $ elemIndices s t, s)

findE :: String -> Char
findE t =  snd . maximum $ fmap (countAppearance t) letters

transChar :: Int -> Char -> Char
transChar d c
  | elem c letters = letters !! (((+) (0-d) $ fromJust $ elemIndex c letters) `mod` 26)
  | otherwise      = id c

decryptWithGuess :: String -> String
decryptWithGuess t =  fmap (transChar $ -4 +( fromJust (elemIndex (findE t) letters))) t

encryptWith ::Int -> String -> String
encryptWith d text =  fmap (transChar (-d)) text

decryptWith ::Int -> String -> String
decryptWith d text =  fmap (transChar d) text

main :: IO ()
main = do
  putStrLn "File?"
  filename <- getLine
  file <- (readFile filename) `catch` (\e -> do let err = show (e :: IOException)
                                                hPutStr stderr ("Warning: " ++ filename ++ ": " ++ err)
                                                return "")
  let t = decryptWithGuess file
  putStrLn t
  putStrLn . encryptWith 5 $ t
