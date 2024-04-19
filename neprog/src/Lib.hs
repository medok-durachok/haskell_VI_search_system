module Lib (Tarif (..)
            , parseTarif
            , searchProducts
            , showTarif
            , getPriceByIndex
            , askQuery) where     

import Text.Read (readMaybe) 
import Data.List.Split (splitOn) 
import Data.Char (toLower)

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
                 inputGigabyteNumber inputSmsNumber inputBalanceTransfer inputFamilyTarif inputIsUnlimitedSocials

askInput :: String -> IO (String, Maybe String)
askInput prompt = do
  putStrLn $ "Enter " ++ prompt ++ ":"
  input <- getLine
  if input == "" then return (prompt,Nothing)
  else return (prompt, Just input)

askInputWithIntervals :: String -> IO (String, Maybe Intervals)
askInputWithIntervals prompt = do
  putStrLn $ "Enter " ++ prompt ++ " (single value, from, to, or from-to):"
  input <- getLine
  case readMaybe input :: Maybe Double of
    Just value -> return (prompt, Just $ Single value)
    Nothing -> case words input of
      ["from", fromValue] -> case readMaybe fromValue :: Maybe Double of
        Just value -> return (prompt, Just $ From value)
        Nothing -> return (prompt, Nothing)
      ["to", toValue] -> case readMaybe toValue :: Maybe Double of
        Just value -> return (prompt, Just $ To value)
        Nothing -> return (prompt, Nothing)
      ["from-to", fromValue, toValue] -> case (readMaybe fromValue :: Maybe Double, readMaybe toValue :: Maybe Double) of
        (Just from, Just to) -> return (prompt, Just $ FromTo (from, to))
        _ -> return (prompt, Nothing)
      _ -> return (prompt, Nothing)

askInputWithBool :: String -> IO (String, Maybe Bool)
askInputWithBool prompt = do
  putStrLn $ "Enter " ++ prompt ++ " (true/false):"
  input <- getLine
  case map toLower input of
    "true" -> return (prompt, Just True)
    "false" -> return (prompt, Just False)
    _ -> return (prompt, Nothing)

-- data for tarifs read from file
data Tarif = Tarif {  -- either string tarif check
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


parseTarif :: String -> Maybe Tarif
parseTarif str =
  case splitOn ", " str of
    [brand, name, price, minutes, internet, sms, transfer, family, socials] ->
      case (parseIntervals price, parseIntervals minutes, parseIntervals internet, parseIntervals sms, 
                readMaybe transfer, readMaybe family, readMaybe socials) of
        (p, m, i, s, t, f, so) ->
          Just $ Tarif brand name p m i s t f so
    _ -> Nothing


-- setting string from file as tarif object 


-- setting entered user query as Query object
{-parseUserQuery :: String -> Either String Query
parseUserQuery str = if length (splitOn "/" str) < queryArity then Left "wrong query. Try again"
  else case splitOn "/" str of
    [brand, price, minutes, internet, sms, transfer, family, socials] ->
      case (readMaybe brand, parseIntervals price, parseIntervals minutes, parseIntervals internet
              , parseIntervals sms, if transfer == "yes" then Just True 
                                                else if transfer == "no" then Just False 
                                                else Nothing,
                            if family == "yes" then Just True 
                                                else if family == "no" then Just False 
                                                else Nothing, 
                            if socials == "yes" then Just True 
                                                else if socials == "no" then Just False 
                                                else Nothing) of
        (b, p, m, i, s, t, f, so) ->
          Right $ Query ("brand", b) ("price", p) ("minutesNuber", m) ("gigabytes", i) ("SMS", s) ("transfer", t) ("family", f) ("socials", so)
    _ -> Left "rtt"-}

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
          , compareMaybeField (isUnlimitedSocials tarif) (snd $ queryIsUnlimitedSocials query)] == True 
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
