module Lib
    ( Tarif (..),
        parseTarif
    ) where     

import Text.Read (readMaybe)  

data Tarif = Tarif {
    brandName :: String,
    tarifPrice :: Maybe Int,
    minutesNumber :: Maybe Int,
    gigabyteNumber :: Maybe Int,
    smsNumber :: Maybe Int,
    balanceTransfer :: Maybe Bool,
    familyTarif :: Maybe Bool,
    isUnlimitedSocials :: Maybe Bool
} deriving (Show, Read)

data Price = From Double | To Double | FromTo (Double, Double) | Double deriving (Show, Read) -- FROMTO КОРТЕЖ

data Query = Query {
    query_brandName :: (String, String),
    query_tarifPrice :: (String, Maybe Int),
    query_minutesNumber :: (String, Maybe Int), -- maybe 
    query_gigabyteNumber :: (String, Maybe Int),
    query_smsNumber :: (String, Maybe Int),
    query_balanceTransfer :: (String, Maybe Bool),
    query_familyTarif :: (String, Maybe Bool),
    query_isUnlimitedSocials :: (String, Maybe Bool)
} deriving (Show, Read)


parseTarif :: String -> Tarif
parseTarif str =
  let [brand, price, minutes, internet, sms, transfer, family, socials] = words str
  in Tarif { brandName = brand, tarifPrice = readMaybe price , minutesNumber = readMaybe minutes, gigabyteNumber = readMaybe internet, smsNumber = readMaybe sms, 
            balanceTransfer = readMaybe transfer, familyTarif = readMaybe family, isUnlimitedSocials = readMaybe socials}
-- приведение строки к data Tarif


parseUserQuery :: String -> Query
parseUserQuery str =
  let [brand, price, minutes, internet, sms, transfer, family, socials] = words str
  in Query { query_brandName = ("brand", brand), query_tarifPrice = ("price", readMaybe price), query_minutesNumber = ("minutesNuber", readMaybe minutes), query_gigabyteNumber = ("Gigabytes", readMaybe internet), query_smsNumber = ("SMS", readMaybe sms), 
            query_balanceTransfer = ("transfer", readMaybe transfer), query_familyTarif = ("family", readMaybe family), query_isUnlimitedSocials = ("socials", readMaybe socials)}
-- получаем информацию из запроса

{-
printQueryInstructions :: IO ()
-- инструкция к запросу

validateQuery :: String -> Either String Bool
-- проверка формата запроса

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
