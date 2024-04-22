module InOut (  readAndConcatFileLines
              , instruct
              , askQuery
              , repeatQueries
              , changeField) where

import Lib (  parseTarif
            , searchProducts
            , showTarif
            , getPriceByIndex
            , checkResponse
            , parseIntervals)

import Types ( Tarif(..)
              , Query(..)
              , Intervals(..))
    
import System.IO () 
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import System.Directory (doesFileExist)

-- reading from files + checking if all of them exist
readAndConcatFileLines :: FilePath -> IO (Either String [Tarif])
readAndConcatFileLines path = do
  fileExists <- doesFileExist path
  if fileExists
    then do
      contents <- readFile path
      let linesOfFile = lines contents
      return $ Right $ catMaybes (map parseTarif linesOfFile)
    else return $ Left $ "File '" ++ path ++ "' doesn't exist.\n"

-- instruction for user about query inputing
instruct :: IO ()
instruct = do
  putStrLn "\nInstruction:"
  putStrLn "Welcome to Tariff Search!"
  putStrLn "Please provide the following information for your query:"
  putStrLn "Brand Name: Enter the brand name of the tariff."
  putStrLn "Tarif Price: Enter the price range or single value of the tariff."
  putStrLn "Minutes Number: Enter the range or single value of included minutes."
  putStrLn "Gigabyte Number: Enter the range or single value of included gigabytes."
  putStrLn "SMS Number: Enter the range or single value of included SMS."
  putStrLn "Balance Transfer: Enter 'yes' if the tariff includes balance transfer, 'no' if not."
  putStrLn "Family Tarif: Enter 'yes' if the tariff includes family options, 'no' if not."
  putStrLn "Is Unlimited Socials: Enter 'yes' if the tariff includes unlimited social media, 'no' if not."
  putStrLn "Leave fields blank if not applicable. \n"

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
  putStrLn $ "Enter " ++ prompt ++ " (single value, From, To, or FromTo)."
  putStrLn $ "Example: 12; From 12; To 12, FromTo 11 12"
  input <- getLine
  if input == "" then return (prompt, Nothing)
  else case readMaybe input :: Maybe Double of
    Just value -> case parseIntervals input of
        Nothing -> do 
          putStrLn "Wrong format. Try again"
          askInputWithIntervals prompt
        _ -> do return (prompt, Just $ Single value)
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

-- suggestion to enter another query or change current
repeatQueries :: [[Tarif]] -> Query -> Double -> IO ()
repeatQueries fileContents query bonus = do
  let searchResult = concatMap (searchProducts query) (concat fileContents)
  putStr "\nFind: "
  print $ length searchResult
  putStrLn $ showTarif searchResult
  repeatIndices searchResult bonus
  putStrLn "Do you want to change any fields?"
  changed <- askForChangingFields query
  case changed of 
    Just q -> repeatQueries fileContents q bonus
    _ -> askToRepeatQuery fileContents searchResult bonus

-- asking about particular field to change
changingFields :: Query -> IO (Maybe Query)
changingFields query = do
  putStrLn "Enter the field you want to change\
  \(brand, price, minutes, internet, sms, transfer, family, socials):"
  field <- getLine
  if elem field ["brand", "price", "minutes", "internet", "sms", "transfer", "family", "socials"]
    then do
      putStrLn "Enter the new value for the field:"
      updatedQuery <- changeField query field
      putStrLn "Do you want to change another field? (yes/no)"
      continue <- getLine
      if continue == "yes" then changingFields updatedQuery 
      else return (Just updatedQuery)
    else do
      putStrLn "Invalid field. Please enter a valid field."
      changingFields query
    
-- ask if user wants to change anything
askForChangingFields :: Query -> IO (Maybe Query)
askForChangingFields query = do
  answer <- getLine
  case checkResponse answer of 
    Right True -> changingFields query
    Right False -> return Nothing
    Left err -> do
      putStrLn err 
      askForChangingFields query

-- change value by key in Query
changeField :: Query -> String -> IO (Query)
changeField query label =
  case label of
    "brand" -> do 
      (key, value) <- askInput "Brand name"
      return query { queryBrandName = (key, value) }
    "price" -> do 
      (key, value) <- askInputWithIntervals "Tarif price"
      return query { queryTarifPrice = (key, value) }
    "minutes" -> do 
      (key, value) <- askInputWithIntervals "Minutes number"
      return query { queryMinutesNumber = (key, value) }
    "internet" -> do 
      (key, value) <- askInputWithIntervals "Gigabyte number"
      return query { queryGigabyteNumber = (key, value) }
    "sms" -> do 
      (key, value) <- askInputWithIntervals "SMS number"
      return query { querySmsNumber = (key, value) }
    "transfer" -> do 
      (key, value) <- askInputWithBool "Balance transfer"
      return query { queryBalanceTransfer= (key, value) }
    "family" -> do 
      (key, value) <- askInputWithBool "Family tarif"
      return query { queryFamilyTarif = (key, value) }
    "socials" -> do 
      (key, value) <- askInputWithBool "Is unlimited socials"
      return query { queryIsUnlimitedSocials = (key, value) }
    _ -> return query

-- helping function for repeating query
askToRepeatQuery:: [[Tarif]] -> [Tarif] -> Double -> IO ()
askToRepeatQuery fileContents searchResult bonus = do
  putStrLn "Do you want to enter another query? (yes/no)"
  continue <- getLine
  case checkResponse continue of
    Right True -> do
      newQuery <- askQuery
      repeatQueries fileContents newQuery bonus
    Right False -> return ()
    Left err -> do
      putStrLn err
      askToRepeatQuery fileContents searchResult bonus

-- suggestion to enter another index
repeatIndices :: [Tarif] -> Double -> IO ()
repeatIndices searchResult bonus = do
  if length searchResult > 0 then do
    putStrLn "Show the price of tarif at index:.. If you don't need price, enter 0"
    num <- getLine
    let ind = readMaybe num
    case ind of 
      Just index -> do
        putStrLn ""
        if index == 0 then putStr ""
        else 
          if index > 0 && index <= (length searchResult) then showPrice searchResult index bonus
          else do 
            putStrLn "Wrong number. Try again"
            repeatIndices searchResult bonus
      Nothing -> do 
        putStrLn "Wrong number. Try again"
        repeatIndices searchResult bonus
  else putStr ""

-- sugesstion to have a look at another price in list
continueWithPrices :: [Tarif] -> Double -> IO ()
continueWithPrices searchResult bonus = do
  putStrLn "Continue with prices? (yes/no)"
  continue <- getLine
  case checkResponse continue of
    Right True -> repeatIndices searchResult bonus
    Right False -> return ()
    Left err -> do
      putStrLn err
      continueWithPrices searchResult bonus

-- suggestion to show price taking bonuses into account or not
showPrice :: [Tarif] -> Int -> Double -> IO()
showPrice searchResult index bonus = do
  putStrLn "Do you want to use bonuses? (yes/no)"
  bonus_ans <- getLine
  case (checkResponse bonus_ans) of
    Right True -> do
      let bonusApplied = bonus
      putStrLn $ show $ getPriceByIndex searchResult index bonusApplied
      continueWithPrices searchResult bonus
    Right False -> do
      let bonusApplied = 0.0
      putStrLn $ show $ getPriceByIndex searchResult index bonusApplied
      continueWithPrices searchResult bonus
    Left err -> do
      putStrLn err
      showPrice searchResult index bonus   
      