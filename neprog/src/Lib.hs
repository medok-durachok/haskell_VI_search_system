module Lib
    ( Tarif (..)
        , parseTarif
        , parseUserQuery
        , searchProducts
    ) where     

import Text.Read (readMaybe) 
import Data.List.Split (splitOn) 
import Data.List (any)
import Data.Maybe (isNothing)

data Tarif = Tarif {
    brandName :: String
    , tarifPrice :: Maybe Intervals
    , minutesNumber :: Maybe Intervals
    , gigabyteNumber :: Maybe Intervals
    , smsNumber :: Maybe Intervals
    , balanceTransfer :: Maybe Bool
    , familyTarif :: Maybe Bool
    , isUnlimitedSocials :: Maybe Bool
} deriving (Show, Read)

data Intervals = From Double | To Double | FromTo (Double, Double) | Single Double
     deriving (Show, Read, Eq)

data Query = Query {
    queryBrandName :: (String, String)
    , queryTarifPrice :: (String, Maybe Intervals)
    , queryMinutesNumber :: (String, Maybe Intervals) 
    , queryGigabyteNumber :: (String, Maybe Intervals)
    , querySmsNumber :: (String, Maybe Intervals)
    , queryBalanceTransfer :: (String, Maybe Bool)
    , queryFamilyTarif :: (String, Maybe Bool)
    , queryIsUnlimitedSocials :: (String, Maybe Bool)
} deriving (Show, Read)


parseTarif :: String -> Tarif
parseTarif str =
  let [brand, price, minutes, internet, sms, transfer, family, socials] = splitOn ", " str
  in Tarif { brandName = brand, tarifPrice = parseIntervals price , minutesNumber = parseIntervals minutes, gigabyteNumber = parseIntervals internet, smsNumber = parseIntervals sms, 
            balanceTransfer = readMaybe transfer, familyTarif = readMaybe family, isUnlimitedSocials = readMaybe socials}
-- приведение строки к data Tarif


parseUserQuery :: String -> Query
parseUserQuery str =
  let [brand, price, minutes, internet, sms, transfer, family, socials] = splitOn "/" str
  in Query { queryBrandName = ("brand", brand), queryTarifPrice = ("price", parseIntervals price), queryMinutesNumber = ("minutesNuber", parseIntervals minutes), queryGigabyteNumber = ("Gigabytes", parseIntervals internet), querySmsNumber = ("SMS", parseIntervals sms), 
            queryBalanceTransfer = ("transfer", readMaybe transfer), queryFamilyTarif = ("family", readMaybe family), queryIsUnlimitedSocials = ("socials", readMaybe socials)}
-- получаем информацию из запроса

compareBoolField :: Eq a => Maybe a -> Maybe a -> Bool
compareBoolField (Just x) (Just y) = x == y
compareBoolField (Just _) _ = True

compareIntervalsField :: Maybe Intervals -> Maybe Intervals -> Bool
compareIntervalsField (Just (Single x)) (Just (Single y)) = x == y
compareIntervalsField (Just (Single x)) (Just (From y)) = x >= y
compareIntervalsField (Just (Single x)) (Just (To y)) = x <= y
compareIntervalsField (Just (Single x)) (Just (FromTo (y1, y2))) = x <= y2 && x >= y1
compareIntervalsField (Just (Single _)) _ = True

searchProducts :: Query -> Tarif -> [Tarif]
searchProducts query tarif =
  if all id [compareIntervalsField (tarifPrice tarif) (snd $ queryTarifPrice query)
          , compareIntervalsField (minutesNumber tarif) (snd $ queryMinutesNumber query)
          , compareIntervalsField (gigabyteNumber tarif) (snd $ queryGigabyteNumber query)
          , compareIntervalsField (smsNumber tarif) (snd $ querySmsNumber query)
          , compareBoolField (balanceTransfer tarif) (snd $ queryBalanceTransfer query)
          , compareBoolField (familyTarif tarif) (snd $ queryFamilyTarif query)
          , compareBoolField (isUnlimitedSocials tarif) (snd $ queryIsUnlimitedSocials query)] == True then [tarif]
          else []

parseIntervals :: String -> Maybe Intervals
parseIntervals str = case words str of
  ["From", x] -> From <$> readMaybe x
  ["To", x] -> To <$> readMaybe x
  ["FromTo", x, y] -> FromTo <$> ((,) <$> readMaybe x <*> readMaybe y)
  [x] -> Single <$> readMaybe x
  _ -> Nothing

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
