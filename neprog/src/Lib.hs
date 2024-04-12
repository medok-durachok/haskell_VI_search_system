module Lib
    ( Tarif (..),
        parseTarif,
        parseUserQuery,
        compareQueries
    ) where     

import Text.Read (readMaybe) 
import Data.List.Split (splitOn) 
import Data.List (any)
import Data.Maybe (isNothing)

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
  let [brand, price, minutes, internet, sms, transfer, family, socials] = splitOn "/" str
  in Query { query_brandName = ("brand", brand), query_tarifPrice = ("price", readMaybe price), query_minutesNumber = ("minutesNuber", readMaybe minutes), query_gigabyteNumber = ("Gigabytes", readMaybe internet), query_smsNumber = ("SMS", readMaybe sms), 
            query_balanceTransfer = ("transfer", readMaybe transfer), query_familyTarif = ("family", readMaybe family), query_isUnlimitedSocials = ("socials", readMaybe socials)}
-- получаем информацию из запроса

compareField :: Eq a => Maybe a -> Maybe a -> Bool
compareField (Just x) (Just y) = x == y
compareField _ _ = True

searchProducts :: Query -> Tarif -> [Tarif]
searchProducts query tarif =
  if all id [compareField (tarifPrice tarif) (snd $ query_tarifPrice query)
          , compareField (minutesNumber tarif) (snd $ query_minutesNumber query)
          , compareField (gigabyteNumber tarif) (snd $ query_gigabyteNumber query)
          , compareField (smsNumber tarif) (snd $ query_smsNumber query)
          , compareField (balanceTransfer tarif) (snd $ query_balanceTransfer query)
          , compareField (familyTarif tarif) (snd $ query_familyTarif query)
          , compareField (isUnlimitedSocials tarif) (snd $ query_isUnlimitedSocials query)] == True then [tarif]
          else []

{-
printQueryInstructions :: IO ()
-- инструкция к запросу

calculatePriceWithBonus :: Double -> Tarif -> Double
-- цена с учетом баллов

printMatchingProducts :: [Tarif] -> Tarif -> IO ()
-- выводим подходящие на экран

askForPriceByIndex :: [Tarif] -> Int -> IO ()
-- вывести цену интересующего товара

loadBonusPoints :: FilePath -> IO (Either String Double)
-- загрузить файл с бонусами

askToContinue :: IO Bool-}
