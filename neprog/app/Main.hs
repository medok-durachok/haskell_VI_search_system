{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (Tarif(..), parseTarif)

import System.IO
import Data.List.Split (splitOn)

{-parseFilePaths :: String -> [FilePath]
-- полученную строку с названиями файлов разбиваем

getUserQuery :: IO String
-- получаем запрос-}

readAndConcatFileLines :: FilePath -> IO [Tarif]
readAndConcatFileLines path = do
  contents <- readFile path
  let linesOfFile = lines contents
  return (map parseTarif linesOfFile)

main :: IO ()
main = do 
  --contents <- readFile "C:/uni2023-24/haskell_VI_Potapova/neprog/brand1.txt"
  --putStrLn contents
  putStrLn "Введите пути файлов, разделенные запятыми и пробелами:"
  input <- getLine
  let paths = splitOn ", " input
  putStrLn "" 
  --mapM_ putStrLn (map ((++) "C:/uni2023-24/haskell_VI_Potapova/neprog/") paths)
-- запрашиваем пути файлов через запятую, делаем сплит по ', '
  fileContents <- mapM readAndConcatFileLines (map ((++) "C:/uni2023-24/haskell_VI_Potapova/neprog/") paths)
  putStrLn $ show (concat fileContents)
  putStrLn "Введите запрос:"
  --query <- getLine

-- получаем записи, проверяем, что они соответствуют типу ??
-- записи: построчно -> список строк; каждую строку превращаем в список строк (через сплит??);
							-- внутри каждого списка делаем проверку на нужные типы и преобразуем в нужные
							-- потом делаем добавление
-- предлагаем пользователю инструкцию к запросу
-- пользователь вводит запрос (можно задавать строкой по формату)
-- разбираем запрос в список кортежей вида (характеристика, значение)
-- если характеристика отсутствует, заменяем пустым (, ), при сравнении пропускаем
-- делаем поиск по собранным записям: если есть записи через - как интервал, то определяем как интервал (как тогда лучше 
																					--определить тип) ??
-- возвращаем список подходящих, выводим на экран
-- предлагаем пользователю посмотреть цену через порядковый номер
-- загружаем файл с бонусными баллами, выводим цену с учетом бонусных баллов или без него
-- предлагаем пользователю продолжить поиск или закончить
