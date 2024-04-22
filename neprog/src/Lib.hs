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
  putStrLn $ "Enter " ++ prompt ++ " (single value, From, To, or FromTo):"
  input <- getLine
  if input == "" then return (prompt, Nothing)
  else case readMaybe input :: Maybe Double of
    Just value -> return (prompt, Just $ Single value)
    Nothing -> case parseIntervals input of
        Nothing -> do 
          putStrLn "Wrong format. Try again"
          askInputWithIntervals prompt
        _ -> do return (prompt, parseIntervals input)

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
parseIntervals str = case words (map toLower str) of
  ["from", x] -> From <$> readMaybe x
  ["to", x] -> To <$> readMaybe x
  ["fromto", x, y] -> FromTo <$> ((,) <$> readMaybe x <*> readMaybe y)
  [x] -> Single <$> readMaybe x
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

-- change value by key in Query
changeField :: Query -> String -> IO (Query)
changeField query label =
  case label of
    "brand" -> do 
      (key, value) <- askInput "Brand name"
      return query { queryBrandName = (key, value)}
    "price" -> do 
      (key, value) <- askInputWithIntervals "Tarif price"
      return query { queryTarifPrice = (key, value)}
    "minutes" -> do 
      (key, value) <- askInputWithIntervals "Minutes number"
      return query { queryMinutesNumber = (key, value)}
    "internet" -> do 
      (key, value) <- askInputWithIntervals "Gigabyte number"
      return query { queryGigabyteNumber = (key, value)}
    "sms" -> do 
      (key, value) <- askInputWithIntervals "SMS number"
      return query { querySmsNumber = (key, value)}
    "transfer" -> do 
      (key, value) <- askInputWithBool "Balance transfer"
      return query { queryBalanceTransfer= (key, value)}
    "family" -> do 
      (key, value) <- askInputWithBool "Family tarif"
      return query { queryFamilyTarif = (key, value)}
    "socials" -> do 
      (key, value) <- askInputWithBool "Is unlimited socials"
      return query { queryIsUnlimitedSocials = (key, value)}
    _ -> return query
