module Lib
    ( Tarif (..)
        , parseTarif
        , parseUserQuery
        , searchProducts
        , showTarif
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
} deriving (Read)

instance Show Tarif where
  show (Tarif brand price minutes internet sms transfer family socials) =
    "brand: " ++ show brand ++
    ", price: " ++ showInterval price ++
    ", minutes: " ++ showInterval minutes ++
    ", gigabyte: " ++ showInterval internet ++
    ", sms: " ++ showInterval sms ++
    ", transfer: " ++ showMaybe transfer ++
    ", family: " ++ showMaybe family ++
    ", unlimitedSocials: " ++ showMaybe socials ++ "\n"
    where
      showInterval (Just (Single x)) = show x
      showMaybe (Just True) = "yes"
      showMaybe (Just False) = "no"


showTarifWithBonus :: Tarif -> Double -> String
showTarifWithBonus tarif bonus = 
  "brand: " ++ brandName tarif ++
  ", price: " ++ showPrice (tarifPrice tarif) bonus ++
  ", minutes: " ++ showInterval (minutesNumber tarif) ++
  ", gigabyte: " ++ showInterval (gigabyteNumber tarif) ++
  ", sms: " ++ showInterval (smsNumber tarif) ++
  ", transfer: " ++ showMaybe (balanceTransfer tarif) ++
  ", family: " ++ showMaybe (familyTarif tarif) ++
  ", unlimitedSocials: " ++ showMaybe (isUnlimitedSocials tarif) ++ "\n"
  where
    showPrice (Just (Single x)) bonus = show (x - bonus)
    showInterval (Just (Single x)) = show x
    showMaybe (Just True) = "yes"
    showMaybe (Just False) = "no"


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
compareBoolField _ _ = True

compareIntervalsField :: Maybe Intervals -> Maybe Intervals -> Bool
compareIntervalsField (Just (Single x)) (Just (Single y)) = x == y
compareIntervalsField (Just (Single x)) (Just (From y)) = x >= y
compareIntervalsField (Just (Single x)) (Just (To y)) = x <= y
compareIntervalsField (Just (Single x)) (Just (FromTo (y1, y2))) = x <= y2 && x >= y1
compareIntervalsField _ _ = True

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


showTarif :: [Tarif] -> Double -> Int -> String
showTarif tarif _ 0 = show tarif
showTarif tarif bonus 1 = concat (map (\t -> showTarifWithBonus t bonus) tarif)

{-
printQueryInstructions :: IO ()
-- инструкция к запросу

askForPriceByIndex :: [Tarif] -> Int -> IO ()
-- вывести цену интересующего товара

askToContinue :: IO Bool-}
