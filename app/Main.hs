module Main where

import System.IO
import Data.Ord
import Data.List
import Data.Tuple
import Data.Maybe
import Control.Exception.Base

letters :: [Char]
letters = cycle ['a'..'z']

countAppearance :: [Char] -> Char -> (Int, Char)
countAppearance t s = (length $ elemIndices s t, s)

findShiftToE :: String -> Char
findShiftToE t = snd . maximum $ fmap (countAppearance t) $ take 26 letters

transChar :: Int -> Char -> Char
transChar shift c
  | elem c $ take 26 letters = head $ drop (shift `mod` 26) $ dropWhile (/=c) letters
  | otherwise      = c

decryptWithGuess :: String -> String
decryptWithGuess t =  fmap (transChar $ -4 +( fromJust (elemIndex (findShiftToE t) letters))) t

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
  writeFile "encSecret.txt" $ encryptWith 5 t
