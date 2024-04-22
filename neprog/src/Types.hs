module Types ( Tarif(..)
              , Query(..)
              , Intervals(..)) where

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
    , queryTarifName :: (String, Maybe String)
    , queryTarifPrice :: (String, Maybe Intervals)
    , queryMinutesNumber :: (String, Maybe Intervals) 
    , queryGigabyteNumber :: (String, Maybe Intervals)
    , querySmsNumber :: (String, Maybe Intervals)
    , queryBalanceTransfer :: (String, Maybe Bool)
    , queryFamilyTarif :: (String, Maybe Bool)
    , queryIsUnlimitedSocials :: (String, Maybe Bool)
} deriving (Show, Read)
