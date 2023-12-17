{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib (someFunc) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector, toList)
import Control.Lens
import Data.List (groupBy, sortBy, maximumBy)
import Data.Ord (comparing)

data DataRow = DataRow
  { _trackId :: String
  , _trackName :: String
  , _trackAlbumId :: String
  , _trackAlbumName :: String
  , _trackAlbumReleaseDate :: String
  , _trackDuration :: Int
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

data Track = Track
  { _ident :: String
  , _name :: String
  , _duration :: Int
  } deriving Show

data Album = Album
  { _albumId :: String
  , _albumName :: String
  , _albumReleaseDate :: String
  , _albumTracks :: [Track]
  } deriving Show

makeLenses ''DataRow
makeLenses ''Track
makeLenses ''Album

toTrack :: DataRow -> Track
toTrack row = Track
  { _ident = row ^. trackId
  , _name = row ^. trackName
  , _duration = row ^. trackDuration
  }

toAlbum :: [DataRow] -> Album
toAlbum row = Album
  { _albumId = (head row) ^. trackAlbumId
  , _albumName = (head row) ^. trackAlbumName
  , _albumReleaseDate = (head row) ^. trackAlbumReleaseDate
  , _albumTracks = map (\a -> toTrack a) row
  }

toAlbums :: [DataRow] -> [Album]
toAlbums row = map (\a -> toAlbum a) $ 
    groupBy (\a b -> a ^. trackAlbumId == b ^. trackAlbumId) $ 
    sortBy (\a b -> compare (a ^. trackAlbumId) (b ^. trackAlbumId)) row

longestAlbumByYear :: String -> [Album] -> Maybe Album
longestAlbumByYear date albums =
  case filter (\album -> album ^. albumReleaseDate == date) albums of
    [] -> Nothing
    albumsOfYear -> Just $ maximumBy (comparing totalDuration) albumsOfYear
  where
    totalDuration album = sum $ map (\a -> a ^. duration) (album ^. albumTracks)

someFunc :: IO ()
someFunc = do
    let csvFile = "spotify_songs.csv"
    result <- readCSVFile csvFile
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right dataRows -> do
            let albums = toAlbums dataRows
                date = "2019-06-14"
            case longestAlbumByYear date albums of
                Nothing -> putStrLn $ "No albums released in " ++ show date
                Just album -> putStrLn $ "Longest album in " ++ show date ++ ": " ++ show album
