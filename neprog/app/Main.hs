{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Lib (Tarif(..), parseTarif, parseUserQuery, searchProducts, showTarif, getPriceByIndex)

import System.IO ()
import Data.List.Split (splitOn)  
import Data.Maybe (catMaybes)
import System.Directory (doesFileExist)
import Data.Char (toLower)

-- checking if user response have right `format`
checkResponse :: String -> Either String Bool
checkResponse response
  | map toLower response == "yes" = Right True
  | map toLower response == "no" = Right False
  | otherwise = Left "Invalid response. Please enter 'yes' or 'no'."

-- reading from files + checking if all of them exist
readAndConcatFileLines :: FilePath -> IO (Either String [Tarif])
readAndConcatFileLines path = do
  fileExists <- doesFileExist path
  if fileExists
    then do
      contents <- readFile path
      let linesOfFile = lines contents
      return $ Right $ catMaybes (map parseTarif linesOfFile)
    else return $ Left $ "File '" ++ path ++ "' doesn't exist"

-- suggestion to show price taking bonuses into account or not
showPrice :: [Tarif] -> Int -> Double -> IO()
showPrice searchResult index bonus = do
  putStrLn "Do you want to use bonuses? (yes/no)"
  bonus_ans <- getLine
  case (checkResponse bonus_ans) of
    Right True -> do
      let bonusApplied = bonus
      putStrLn $ show $ getPriceByIndex searchResult index bonusApplied
      continueWithPrices searchResult bonus
    Right False -> do
      let bonusApplied = 0.0
      putStrLn $ show $ getPriceByIndex searchResult index bonusApplied
      continueWithPrices searchResult bonus
    Left err -> do
      putStrLn err
      showPrice searchResult index bonus

-- sugesstion to have a look at another price in list
continueWithPrices :: [Tarif] -> Double -> IO ()
continueWithPrices searchResult bonus = do
  putStrLn "Continue with prices? (yes/no)"
  continue <- getLine
  case checkResponse continue of
    Right True -> repeatIndices searchResult bonus
    Right False -> return ()
    Left err -> do
      putStrLn err
      continueWithPrices searchResult bonus

-- suggestion to enter another index
repeatIndices :: [Tarif] -> Double -> IO()
repeatIndices searchResult bonus = do
  putStrLn "Show the price of tarif at index:.. If you don't need price, enter 0"
  num <- getLine
  let index = read num
  putStrLn ""
  if index == 0 then putStr ""
  else showPrice searchResult index bonus

-- suggestion to enter another query
repeatQueries :: [[Tarif]] -> Double -> IO ()
repeatQueries fileContents bonus = do
  putStrLn "Enter the query:"
  query <- getLine
  putStrLn $ show $ splitOn "/" query
  let searchResult = concatMap (searchProducts (parseUserQuery query)) (concat fileContents)
  putStrLn ""
  putStrLn $ showTarif searchResult
  
  repeatIndices searchResult bonus
  
  putStrLn "Do you want to enter another query? (yes/no)"
  continue <- getLine
  if map toLower continue == "yes"
    then repeatQueries fileContents bonus
    else putStrLn ""

main :: IO ()
main = do 
  putStrLn "Enter the file paths separated by commas and spaces:"
  input <- getLine
  bonus <- readFile ((++) "C:/uni2023-24/haskell_VI_Potapova/neprog/" "bonus.txt")
  let paths = splitOn ", " input
  fileContents <- mapM readAndConcatFileLines (map ((++) "C:/uni2023-24/haskell_VI_Potapova/neprog/") paths)
  case sequence fileContents of
    Right tarifs -> do
      repeatQueries tarifs (read bonus)
    Left errorMessage -> putStrLn $ "Access error: " ++ errorMessage
