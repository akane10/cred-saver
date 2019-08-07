{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           GHC.Generics
import           System.Directory

fileName :: FilePath
fileName = "./data.json"

data Cred = Cred {
  title    :: String,
  email    :: String,
  password :: String
} deriving (Generic,Show)

instance FromJSON Cred
instance ToJSON Cred

promptLine :: String -> IO String
promptLine prompt = do
  putStr prompt
  getLine

main :: IO()
main = putStrLn "wut"

add :: IO()
add = do
  [title, email, password] <- askCred
  let cred = createCred title email password
  concating cred

view :: IO()
view = do
  val <- decoding
  case val of
    Left err -> putStrLn err
    -- Right ps -> print ps
    Right ps -> putStrLn $ unlines $ fuck [0..] ps

fuck :: [Integer] -> [Cred] -> [String]
fuck = zipWith (\n x ->
  "No: " ++ show n ++ "\n" ++
  "Title: " ++ title x ++ "\n" ++
  "Email: " ++ email x ++ "\n" ++
  "Password: " ++ password x ++ "\n"
  )

concating :: Cred -> IO()
concating newVal = do
  val <- decoding
  case val of
    Left err -> putStrLn err
    Right ps -> writing $ newVal : ps

askCred :: IO [String]
askCred = do
  title <- promptLine "what is title? "
  email <- promptLine "what is email? "
  password <- promptLine "what is password? "
  return [title, email, password]

createCred :: String -> String -> String -> Cred
createCred t e p = Cred {
  title = t,
  email = e,
  password = p
}

writing :: ToJSON a => a -> IO ()
writing val = BS.writeFile fileName $ encode $ val

getJSON :: IO BS.ByteString
getJSON = BS.readFile fileName

decoding :: IO (Either String [Cred])
decoding = eitherDecode <$> getJSON
