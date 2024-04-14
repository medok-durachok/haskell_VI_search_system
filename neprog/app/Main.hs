{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Lib (Tarif(..), parseTarif, parseUserQuery, searchProducts, showTarif, getPriceByIndex)

import System.IO ()
import Data.List.Split (splitOn)  
import Data.Maybe (catMaybes)
import System.Directory (doesFileExist)
import Control.Monad (when)


readFilesIfExists :: [FilePath] -> IO (Either String [String])
readFilesIfExists filePaths = do
  let checkFile filePath = do
        fileExists <- doesFileExist filePath
        if fileExists
          then Right <$> readFile filePath
          else return $ Left $ "Файл '" ++ filePath ++ "' не существует!"
  results <- mapM checkFile filePaths
  return $ sequence results

readAndConcatFileLines :: FilePath -> IO (Either String [Tarif])
readAndConcatFileLines path = do
  fileExists <- doesFileExist path
  if fileExists
    then do
      contents <- readFile path
      let linesOfFile = lines contents
      return $ Right $ catMaybes (map parseTarif linesOfFile)
    else return $ Left $ "File '" ++ path ++ "' doesn't exist"

showPrice :: [Tarif] -> Int -> Double -> IO()
showPrice searchResult index bonus = do
  putStrLn "Do you want to use bonuses? (yes/no)"
  bonus_ans <- getLine
  let bonusApplied = if bonus_ans == "yes" then bonus else 0.0
  putStrLn $ show $ getPriceByIndex searchResult index bonusApplied
  putStrLn "Continue with prices? (yes/no)"
  continue <- getLine
  if continue == "yes"
    then repeatIndices searchResult bonus
    else putStr ""

repeatIndices :: [Tarif] -> Double -> IO()
repeatIndices searchResult bonus = do
  putStrLn "Show the price of tarif at index:.. If you don't need price, enter 0"
  num <- getLine
  let index = read num
  putStrLn ""
  if index == 0 then putStr ""
  else showPrice searchResult index bonus

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
  case sequence fileContents of
    Right tarifs -> do
      repeatQueries tarifs (read bonus)
    Left errorMessage -> putStrLn $ "Access error: " ++ errorMessage
