{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import InOut (  readAndConcatFileLines
              , instruct
              , askQuery
              , repeatQueries)

import System.IO ()
import Data.List.Split (splitOn)  

main :: IO ()
main = do 
  putStrLn "Enter the file paths separated by commas and spaces:"
  input <- getLine
  bonus <- readFile ((++) "C:/uni2023-24/haskell_VI_Potapova/neprog/" "bonus.txt")
  let paths = splitOn ", " input
  fileContents <- mapM readAndConcatFileLines 
                  (map ((++) "C:/uni2023-24/haskell_VI_Potapova/neprog/") paths)
  case sequence fileContents of
    Right tarifs -> do
      instruct
      inpQuery <- askQuery
      repeatQueries tarifs inpQuery (read bonus)
    Left errorMessage -> do
      putStrLn $ "Access error: " ++ errorMessage
      main

-- brand1.txt, brand2.txt, brand3.txt
-- /From 200/To 5000//FromTo 10 20000/yes//yes
