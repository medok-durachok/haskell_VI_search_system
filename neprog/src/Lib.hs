module Lib (parseTarif
            , searchProducts
            , showTarif
            , getPriceByIndex
            , checkResponse
            , parseIntervals) where

import Types ( Tarif(..)
              , Query(..)
              , Intervals(..))

import Text.Read (readMaybe) 
import Data.List.Split (splitOn) 
import Data.Char (toLower)
import System.FilePath

-- checking if user response have right format
checkResponse :: String -> Either String Bool
checkResponse response
  | map toLower response == "yes" = Right True
  | map toLower response == "no" = Right False
  | otherwise = Left "Invalid response. Please enter 'yes' or 'no'."

-- parsing tarifs from files
parseTarif :: String -> FilePath -> Either String Tarif
parseTarif str file =
  case splitOn ", " str of
    [brand, name, price, minutes, internet, sms, transfer, family, socials] ->
      case (parseIntervals price, parseIntervals minutes, 
            parseIntervals internet, parseIntervals sms, 
            readMaybe transfer, readMaybe family, readMaybe socials) of
        (Just p, Just m, Just i, Just s, Just t, Just f, Just so) ->
          Right $ Tarif brand name (Just p) (Just m) (Just i) (Just s) (Just t) (Just f) (Just so)
        _ -> Left ("Wrong format in " ++ last (splitOn "/" (takeFileName file)) ++ ": " ++ str)
    _ -> Left ("Wrong format in" ++ last (splitOn "/" (takeFileName file)) ++ ": " ++ str)

-- comparison of Maybe Bool fields in Query and Tarif
compareMaybeBoolField :: Eq a => Maybe a -> Maybe a -> Bool
compareMaybeBoolField (Just x) (Just y) = x == y
compareMaybeBoolField _ _ = True

-- comparison of Maybe String fields in Query and Tarif
compareMaybeStringField :: Maybe String -> Maybe String -> Bool
compareMaybeStringField (Just x) (Just y) = map toLower x == map toLower y
compareMaybeStringField _ _ = True

-- comparison of Intevals type fields
compareIntervalsField :: Maybe Intervals -> Maybe Intervals -> Bool
compareIntervalsField (Just (Single x)) (Just (Single y)) = x == y
compareIntervalsField (Just (Single x)) (Just (From y)) = x >= y
compareIntervalsField (Just (Single x)) (Just (To y)) = x <= y
compareIntervalsField (Just (Single x)) (Just (FromTo (y1, y2))) = x <= y2 && x >= y1
compareIntervalsField _ _ = True

-- searching relevant tarif plans ; can be simplified by change of data Query 
searchProducts :: Query -> Tarif -> [Tarif]
searchProducts query tarif =
  if all id [compareMaybeStringField (Just (brandName tarif)) (snd $ queryBrandName query)
          , compareMaybeStringField (Just (tarifName tarif)) (snd $ queryTarifName query)
          , compareIntervalsField (tarifPrice tarif) (snd $ queryTarifPrice query)
          , compareIntervalsField (minutesNumber tarif) (snd $ queryMinutesNumber query)
          , compareIntervalsField (gigabyteNumber tarif) (snd $ queryGigabyteNumber query)
          , compareIntervalsField (smsNumber tarif) (snd $ querySmsNumber query)
          , compareMaybeBoolField (balanceTransfer tarif) (snd $ queryBalanceTransfer query)
          , compareMaybeBoolField (familyTarif tarif) (snd $ queryFamilyTarif query)
          , compareMaybeBoolField (isUnlimitedSocials tarif) 
                              (snd $ queryIsUnlimitedSocials query)] == True 
          then [tarif]
          else []

isValidNumber :: String -> Bool
isValidNumber x = case (readMaybe x :: Maybe Double) of
  Just num -> num >= 0
  Nothing -> False

isValidFromTo :: String -> String -> Bool
isValidFromTo x y = case (readMaybe x :: Maybe Double) of
  Just num_x -> case (readMaybe y :: Maybe Double) of
    Just num_y -> num_y >= num_x
    Nothing -> False
  Nothing -> False

-- helps to read Interval fields in user query
parseIntervals :: String -> Maybe Intervals
parseIntervals str = case words (map toLower str) of
  [x] 
    | isValidNumber x -> Single <$> readMaybe x
    | otherwise -> Nothing
  ["from", x]
    | isValidNumber x -> From <$> readMaybe x
    | otherwise -> Nothing
  ["to", x]
    | isValidNumber x -> To <$> readMaybe x
    | otherwise -> Nothing
  ["fromto", x, y] 
    | isValidNumber x && isValidFromTo x y -> 
      FromTo <$> ((,) <$> readMaybe x <*> readMaybe y)
    | otherwise -> Nothing
  _ -> Nothing

-- zipping for better look as the output
printListWithNumbers :: [String] -> [String]
printListWithNumbers xs = zipWith addNumber [1..] xs
  where
    addNumber :: Int -> String -> String
    addNumber n x = show n ++ ". " ++ x

-- show with numbers
showTarif :: [Tarif] -> String
showTarif tarif = concat $ printListWithNumbers (map (\t -> show t) tarif)

-- getting only the number for output without any constructors
priceOnly :: Maybe Intervals -> Double -> Double
priceOnly (Just (Single x)) bonus = x - bonus
priceOnly (Just _) _ = 0
priceOnly Nothing _ = 0

-- return tarif on the particular index in list
getPriceByIndex :: [Tarif] -> Int -> Double -> Double
getPriceByIndex lst n bonus = priceOnly (tarifPrice (last (take n lst))) bonus
