module Lib
    ( Tarif (..)
        , parseTarif
        , parseUserQuery
        , searchProducts
        , showTarif
        , getPriceByIndex
    ) where     

import Text.Read (readMaybe) 
import Data.List.Split (splitOn) 

data Tarif = Tarif {
    brandName :: String
    , tarifName :: String
    , tarifPrice :: Maybe Intervals
    , minutesNumber :: Maybe Intervals
    , gigabyteNumber :: Maybe Intervals
    , smsNumber :: Maybe Intervals
    , balanceTransfer :: Maybe Bool
    , familyTarif :: Maybe Bool
    , isUnlimitedSocials :: Maybe Bool
} deriving (Read)

instance Show Tarif where
  show (Tarif brand name _ minutes internet sms transfer family socials) =
    "brand: " ++ show brand ++
    ", name: " ++ show name ++
    ", minutes: " ++ showInterval minutes ++
    ", gigabyte: " ++ showInterval internet ++
    ", sms: " ++ showInterval sms ++
    ", transfer: " ++ showMaybe transfer ++
    ", family: " ++ showMaybe family ++
    ", unlimitedSocials: " ++ showMaybe socials ++ "\n"
    where
      showInterval (Just (Single x)) = show x
      showInterval _ = ""
      showMaybe (Just True) = "yes"
      showMaybe (Just False) = "no"
      showMaybe _ = ""


showTarifWithBonus :: Tarif -> Double -> String
showTarifWithBonus tarif _ = 
  "brand: " ++ brandName tarif ++
  {-", price: " ++ showPrice (tarifPrice tarif) bonus ++ -}
  ", minutes: " ++ showInterval (minutesNumber tarif) ++
  ", gigabyte: " ++ showInterval (gigabyteNumber tarif) ++
  ", sms: " ++ showInterval (smsNumber tarif) ++
  ", transfer: " ++ showMaybe (balanceTransfer tarif) ++
  ", family: " ++ showMaybe (familyTarif tarif) ++
  ", unlimitedSocials: " ++ showMaybe (isUnlimitedSocials tarif) ++ "\n"
  where
    {-showPrice (Just (Single x)) bonus = show (x - bonus)-}
    showInterval (Just (Single x)) = show x
    showInterval _ = ""
    showMaybe (Just True) = "yes"
    showMaybe (Just False) = "no"
    showMaybe _ = ""


data Intervals = From Double | To Double | FromTo (Double, Double) | Single Double
     deriving (Show, Read, Eq)

data Query = Query {
    queryBrandName :: (String, Maybe String)
    , queryTarifPrice :: (String, Maybe Intervals)
    , queryMinutesNumber :: (String, Maybe Intervals) 
    , queryGigabyteNumber :: (String, Maybe Intervals)
    , querySmsNumber :: (String, Maybe Intervals)
    , queryBalanceTransfer :: (String, Maybe Bool)
    , queryFamilyTarif :: (String, Maybe Bool)
    , queryIsUnlimitedSocials :: (String, Maybe Bool)
} deriving (Show, Read)


parseTarif :: String -> Maybe Tarif
parseTarif str =
  let [brand, name, price, minutes, internet, sms, transfer, family, socials] = splitOn ", " str
  in Just Tarif { brandName = brand, tarifName = name, tarifPrice = parseIntervals price , minutesNumber = parseIntervals minutes, gigabyteNumber = parseIntervals internet, smsNumber = parseIntervals sms, 
            balanceTransfer = readMaybe transfer, familyTarif = readMaybe family, isUnlimitedSocials = readMaybe socials}
-- приведение строки к data Tarif


parseUserQuery :: String -> Query
parseUserQuery str =
  let [brand, price, minutes, internet, sms, transfer, family, socials] = splitOn "/" str
  in Query {queryBrandName = ("brand", readMaybe brand), 
            queryTarifPrice = ("price", parseIntervals price), 
            queryMinutesNumber = ("minutesNuber", parseIntervals minutes), 
            queryGigabyteNumber = ("Gigabytes", parseIntervals internet), 
            querySmsNumber = ("SMS", parseIntervals sms), 
            queryBalanceTransfer = ("transfer", if transfer == "yes" then Just True 
                                                else if transfer == "no" then Just False 
                                                else Nothing), 
            queryFamilyTarif = ("family", if family == "yes" then Just True 
                                                else if family == "no" then Just False 
                                                else Nothing), 
            queryIsUnlimitedSocials = ("socials", if socials == "yes" then Just True 
                                                else if socials == "no" then Just False 
                                                else Nothing)}
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
  if all id [compareBoolField (Just (brandName tarif)) (snd $ queryBrandName query)
          , compareIntervalsField (tarifPrice tarif) (snd $ queryTarifPrice query)
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


printListWithNumbers :: [String] -> [String]
printListWithNumbers xs = zipWith (\n x -> show n ++ ". " ++ x) [1..] xs


showTarif :: [Tarif] -> Double -> Int -> String
showTarif tarif _ 0 = concat $ printListWithNumbers (map (\t -> show t) tarif)
showTarif tarif bonus 1 = concat $ printListWithNumbers (map (\t -> showTarifWithBonus t bonus) tarif)
showTarif _ _ _ = ""

priceOnly :: Maybe Intervals -> Double -> Double
priceOnly (Just (Single x)) bonus = x - bonus
priceOnly (Just _) _ = 0
priceOnly Nothing _ = 0


getPriceByIndex :: [Tarif] -> Int -> Double -> Double
getPriceByIndex lst n bonus = priceOnly (tarifPrice (last (take n lst))) bonus
