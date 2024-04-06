module Lib
    ( Tarif (..),
        parseTarif
    ) where

data Tarif = Tarif {
    brandName :: String,
    tarifPrice :: Int,
    minutesNumber :: Int,
    gigabyteNumber :: Int,
    smsNumber :: Int,
    balanceTransfer :: Bool,
    familyTarif :: Bool,
    isUnlimitedSocials :: Bool
} deriving (Show, Read)

data Price = From Double | To Double | FromTo (Double, Double) | Double deriving (Show, Read) -- FROMTO КОРТЕЖ

data Query = Query {
    query_brandName :: (String, String),
    query_tarifPrice :: (String, Price),
    query_minutesNumber :: (String, Either String Int), -- maybe 
    query_gigabyteNumber :: (String, Either String Int),
    query_smsNumber :: (String, Either String Int),
    query_balanceTransfer :: (String, Either String Bool),
    query_familyTarif :: (String, Either String Bool),
    query_isUnlimitedSocials :: (String, Either String Bool)
} deriving (Show, Read)


parseTarif :: String -> Tarif
parseTarif str =
  let [brand, price, minutes, internet, sms, transfer, family, socials] = words str
  in Tarif { brandName = brand, tarifPrice = read price , minutesNumber = read minutes, gigabyteNumber = read internet, smsNumber = read sms, 
            balanceTransfer = read transfer, familyTarif = read family, isUnlimitedSocials = read socials}
-- приведение строки к data Tarif

{-readFrom::FilePath -> [Tarif]
-- считывание из одного

readAndValidateData :: [FilePath] -> Either String [Tarif]
-- считываем информацию из бд let filepaths = ["company1.txt", "company2.txt", "company3.txt"]\

printQueryInstructions :: IO ()
-- инструкция к запросу

validateQuery :: String -> Either String Bool
-- проверка формата запроса

parseUserQuery :: String -> Query
-- получаем информацию из запроса

searchProducts :: Query -> [Tarif] -> [Tarif]
-- поиск по БД

calculatePriceWithBonus :: Double -> Tarif -> Double
-- цена с учетом баллов

printMatchingProducts :: [Tarif] -> Tarif -> IO ()
-- выводим подходящие на экран

askForPriceByIndex :: [Tarif] -> Int -> IO ()
-- вывести цену интересующего товара

loadBonusPoints :: FilePath -> IO (Either String Double)
-- загрузить файл с бонусами

askToContinue :: IO Bool-}
