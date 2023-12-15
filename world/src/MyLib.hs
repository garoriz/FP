{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector, toList)

data DataRow = DataRow
  { trackId :: String
  , trackName :: String
  , trackAlbumId :: String
  , trackAlbumName :: String
  , trackAlbumReleaseDate :: String
  , trackDuration :: Int
  } deriving Show

instance FromNamedRecord DataRow where
    parseNamedRecord m = DataRow
        <$> m .: "track_id"
        <*> m .: "track_name"
        <*> m .: "track_album_id"
        <*> m .: "track_album_name"
        <*> m .: "track_album_release_date"
        <*> m .: "duration_ms"

readCSVFile :: FilePath -> IO (Either String [DataRow])
readCSVFile filePath = do
    csvData <- BL.readFile filePath
    return $ case decodeByName csvData of
        Left err -> Left $ "Error decoding CSV: " ++ err
        Right (_, v) -> Right $ toList v

someFunc :: IO ()
someFunc = do
    let csvFile = "spotify_songs.csv"
    result <- readCSVFile csvFile
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right dataRows -> do
            putStrLn $ "First DataRow: " ++ show (dataRows)
