{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Lib (Tarif(..), parseTarif, parseUserQuery, searchProducts, showTarif)

import System.IO ()
import Data.List.Split (splitOn)  

readAndConcatFileLines :: FilePath -> IO [Tarif]
readAndConcatFileLines path = do
  contents <- readFile path
  let linesOfFile = lines contents
  return (map parseTarif linesOfFile)

--loadBonusPoints :: FilePath -> Int
--loadBonusPoints path = do 


main :: IO ()
main = do 
  --contents <- readFile "C:/uni2023-24/haskell_VI_Potapova/neprog/brand1.txt"
  --putStrLn contents
  putStrLn "Введите пути файлов, разделенные запятыми и пробелами:"
  input <- getLine
  bonus <- readFile ((++) "C:/uni2023-24/haskell_VI_Potapova/neprog/" "bonus.txt")
  let paths = splitOn ", " input
  putStrLn "" 
  --mapM_ putStrLn (map ((++) "C:/uni2023-24/haskell_VI_Potapova/neprog/") paths)
-- запрашиваем пути файлов через запятую, делаем сплит по ', '
  fileContents <- mapM readAndConcatFileLines (map ((++) "C:/uni2023-24/haskell_VI_Potapova/neprog/") paths)
  print (concat fileContents)
  putStrLn "Введите запрос:"
  query <- getLine
  putStrLn "Ваш запрос:"
  putStrLn $ show (parseUserQuery query)
  putStrLn "Учесть бонусы?"
  bonus_ans <- getLine
  putStrLn $ (concat (map (searchProducts (parseUserQuery query)) (concat fileContents))) bonus bonus_ans
  --print (if bonus_ans == 0 then concat (map (searchProducts (parseUserQuery query)) (concat fileContents)))
-- предлагаем пользователю инструкцию к запросу
-- предлагаем пользователю посмотреть цену через порядковый номер
-- загружаем файл с бонусными баллами, выводим цену с учетом бонусных баллов или без него
-- предлагаем пользователю продолжить поиск или закончить
