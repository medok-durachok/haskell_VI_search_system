module Lib (Tarif (..)
            , Query(..)
            , parseTarif
            , searchProducts
            , showTarif
            , getPriceByIndex
            , askQuery
            , checkResponse
            , changeField) where     

import Text.Read (readMaybe) 
import Data.List.Split (splitOn) 
import Data.Char (toLower)

-- checking if user response have right format
checkResponse :: String -> Either String Bool
checkResponse response
  | map toLower response == "yes" = Right True
  | map toLower response == "no" = Right False
  | otherwise = Left "Invalid response. Please enter 'yes' or 'no'."

-- collecting info to make full query
askQuery :: IO Query
askQuery = do
  inputBrandName <- askInput "Brand name"
  inputTarifPrice <- askInputWithIntervals "Tarif price"
  inputMinutesNumber <- askInputWithIntervals "Minutes number"
  inputGigabyteNumber <- askInputWithIntervals "Gigabyte number"
  inputSmsNumber <- askInputWithIntervals "SMS number"
  inputBalanceTransfer <- askInputWithBool "Balance transfer"
  inputFamilyTarif <- askInputWithBool "Family tarif"
  inputIsUnlimitedSocials <- askInputWithBool "Is unlimited socials"
  return $ Query inputBrandName inputTarifPrice inputMinutesNumber 
                 inputGigabyteNumber inputSmsNumber inputBalanceTransfer 
                 inputFamilyTarif inputIsUnlimitedSocials

-- start func for query
askInput :: String -> IO (String, Maybe String)
askInput prompt = do
  putStrLn $ "Enter " ++ prompt ++ ":"
  input <- getLine
  if input == "" then return (prompt,Nothing)
  else return (prompt, Just input)

-- parsing user enter for interval fields
askInputWithIntervals :: String -> IO (String, Maybe Intervals)
askInputWithIntervals prompt = do
  putStrLn $ "Enter " ++ prompt ++ " (single value, from, to, or from-to):"
  input <- getLine
  if input == "" then return (prompt, Nothing)
  else case readMaybe input :: Maybe Double of
    Just value -> return (prompt, Just $ Single value)
    Nothing -> case words input of
      ["from", fromValue] -> case readMaybe fromValue :: Maybe Double of
        Just value -> return (prompt, Just $ From value)
        Nothing -> return (prompt, Nothing)
      ["to", toValue] -> case readMaybe toValue :: Maybe Double of
        Just value -> return (prompt, Just $ To value)
        Nothing -> return (prompt, Nothing)
      ["from-to", fromValue, toValue] -> 
        case (readMaybe fromValue :: Maybe Double, readMaybe toValue :: Maybe Double) of
        (Just from, Just to) -> return (prompt, Just $ FromTo (from, to))
        _ -> do 
          putStrLn "Wrong format. Try again"
          askInputWithIntervals prompt
      _ -> do 
          putStrLn "Wrong format. Try again"
          askInputWithIntervals prompt

-- parsing user enter for boolean fields
askInputWithBool :: String -> IO (String, Maybe Bool)
askInputWithBool prompt = do
  putStrLn $ "Enter " ++ prompt ++ " (yes/no):"
  input <- getLine
  if input == "" then return (prompt, Nothing)
  else case checkResponse input of 
            Right x -> return (prompt, Just x)
            Left err -> do
              putStrLn err
              askInputWithBool prompt 

-- data for tarifs read from file
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

-- data for those items which can be specified as beam, interval or just number
data Intervals = From Double | To Double | FromTo (Double, Double) | Single Double
     deriving (Show, Read, Eq)

-- data for user query
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

-- parsing tarifs from files
parseTarif :: String -> Maybe Tarif
parseTarif str =
  case splitOn ", " str of
    [brand, name, price, minutes, internet, sms, transfer, family, socials] ->
      case (parseIntervals price, parseIntervals minutes, 
            parseIntervals internet, parseIntervals sms, 
            readMaybe transfer, readMaybe family, readMaybe socials) of
        (Just p, Just m, Just i, Just s, Just t, Just f, Just so) ->
          Just $ Tarif brand name (Just p) (Just m) (Just i) (Just s) (Just t) (Just f) (Just so)
        _ -> Nothing
    _ -> Nothing

-- comparison of Maybe type fields in Query and Tarif
compareMaybeField :: Eq a => Maybe a -> Maybe a -> Bool
compareMaybeField (Just x) (Just y) = x == y
compareMaybeField _ _ = True

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
  if all id [compareMaybeField (Just (brandName tarif)) (snd $ queryBrandName query)
          , compareIntervalsField (tarifPrice tarif) (snd $ queryTarifPrice query)
          , compareIntervalsField (minutesNumber tarif) (snd $ queryMinutesNumber query)
          , compareIntervalsField (gigabyteNumber tarif) (snd $ queryGigabyteNumber query)
          , compareIntervalsField (smsNumber tarif) (snd $ querySmsNumber query)
          , compareMaybeField (balanceTransfer tarif) (snd $ queryBalanceTransfer query)
          , compareMaybeField (familyTarif tarif) (snd $ queryFamilyTarif query)
          , compareMaybeField (isUnlimitedSocials tarif) 
                              (snd $ queryIsUnlimitedSocials query)] == True 
          then [tarif]
          else []

-- helps to read Interval fields in user query
parseIntervals :: String -> Maybe Intervals
parseIntervals str = case words str of
  ["From", x] -> From <$> readMaybe x
  ["To", x] -> To <$> readMaybe x
  ["FromTo", x, y] -> FromTo <$> ((,) <$> readMaybe x <*> readMaybe y)
  [x] -> Single <$> readMaybe x
  _ -> Nothing

parseBool :: String -> Maybe Bool
parseBool "yes" = Just True
parseBool "no" = Just False
parseBool _ = Nothing

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

--
changeField :: Query -> String -> Maybe String -> Query
changeField query label (Just newValue) =
  case label of
    "brand" -> query { queryBrandName = ("brand", if newValue == "" then Nothing else Just newValue) }
    "price" -> query { queryTarifPrice = ("price", parseIntervals newValue) }
    "minutes" -> query { queryMinutesNumber = ("minutesNuber", parseIntervals newValue) }
    "internet" -> query { queryGigabyteNumber = ("Gigabytes", parseIntervals newValue) }
    "sms" -> query { querySmsNumber = ("SMS", parseIntervals newValue) }
    "transfer" -> query { queryBalanceTransfer = ("transfer", parseBool newValue) }
    "family" -> query { queryFamilyTarif = ("family", parseBool newValue) }
    "socials" -> query { queryIsUnlimitedSocials = ("socials", parseBool newValue) }
    _ -> query
changeField query _ Nothing = query
