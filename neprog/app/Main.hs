{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Lib (Tarif(..), parseTarif, parseUserQuery, searchProducts, showTarif, getPriceByIndex)

import System.IO ()
import Data.List.Split (splitOn)  
import Data.Maybe (catMaybes)

readAndConcatFileLines :: FilePath -> IO [Tarif]
readAndConcatFileLines path = do
  contents <- readFile path
  let linesOfFile = lines contents
  return (catMaybes (map parseTarif linesOfFile))

repeatIndices :: [Tarif] -> Double -> IO()
repeatIndices searchResult bonus = do
  putStrLn "Show the price of tarif at index: "
  num <- getLine
  let index = read num
  putStrLn ""
  
  putStrLn "Do you want to use bonuses? (yes/no)"
  bonus_ans <- getLine
  let bonusApplied = if bonus_ans == "yes" then bonus else 0.0
  putStrLn $ show $ getPriceByIndex searchResult index bonusApplied
  putStrLn "Continue with prices? (yes/no)"
  continue <- getLine
  if continue == "yes"
    then repeatIndices searchResult bonus
    else putStrLn ""

repeatQueries :: [[Tarif]] -> Double -> IO ()
repeatQueries fileContents bonus = do
  putStrLn "Enter the query:"
  query <- getLine
  putStrLn $ show $ splitOn "/" query
  let searchResult = concatMap (searchProducts (parseUserQuery query)) (concat fileContents)
  putStrLn ""
  putStrLn $ showTarif searchResult bonus 0
  
  repeatIndices searchResult bonus
  
  putStrLn "Do you want to enter another query? (yes/no)"
  continue <- getLine
  if continue == "yes"
    then repeatQueries fileContents bonus
    else putStrLn ""

main :: IO ()
main = do 
  putStrLn "Enter the file paths separated by commas and spaces:"
  input <- getLine
  bonus <- readFile ((++) "C:/uni2023-24/haskell_VI_Potapova/neprog/" "bonus.txt")
  let paths = splitOn ", " input
  fileContents <- mapM readAndConcatFileLines (map ((++) "C:/uni2023-24/haskell_VI_Potapova/neprog/") paths)
  repeatQueries fileContents (read bonus)
