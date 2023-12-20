{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Hedgehog
import Control.Exception

import MyLib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MyLib Tests"
  [ parseCsv
  , getAlbumsByFilter
  ]

parseCsv :: TestTree
parseCsv = testGroup "Parsing CSV"
  [ testCase "Parsing CSV" $ do
        let csvFile = "test_data.csv"
        result <- readCSVFile csvFile
        case result of
            Left err -> putStrLn $ "Error: " ++ err
            Right dataRows -> do
                let albums = toAlbums dataRows
                albums @?= [Album { _albumId = "1", 
                                    _albumName = "album 1", 
                                    _albumReleaseDate = "2002-09-18", 
                                    _albumTracks = [Track {_ident = "1", 
                                                           _name = "track 1", 
                                                           _duration = 3
                                                          },
                                                    Track {_ident = "1", 
                                                           _name = "track 2", 
                                                           _duration = 3
                                                          }
                                                   ]
                                  },
                            Album {_albumId = "2", 
                                   _albumName = "album 2", 
                                   _albumReleaseDate = "2002-09-18", 
                                   _albumTracks = [Track {_ident = "2", 
                                                          _name = "track 3", 
                                                          _duration = 1
                                                         }
                                                  ]
                                  }
                           ]
  ]

getAlbumsByFilter :: TestTree
getAlbumsByFilter = testGroup "Getting albums by filter"
  [ testCase "Gettung longest album" $ do
        let albums = [Album { _albumId = "1", 
                                    _albumName = "album 1", 
                                    _albumReleaseDate = "2002-09-18", 
                                    _albumTracks = [Track {_ident = "1", 
                                                           _name = "track 1", 
                                                           _duration = 3
                                                          },
                                                    Track {_ident = "1", 
                                                           _name = "track 2", 
                                                           _duration = 3
                                                          }
                                                   ]
                                  },
                            Album {_albumId = "2", 
                                   _albumName = "album 2", 
                                   _albumReleaseDate = "2002-09-18", 
                                   _albumTracks = [Track {_ident = "2", 
                                                          _name = "track 3", 
                                                          _duration = 1
                                                         }
                                                  ]
                                  }
                     ]
        let date = "2002-09-18"
        let longestAlbum = longestAlbumByYear date albums
        longestAlbum @?= (Just (head albums))
  , testCase "Not getting longest album" $ do
        let albums = []
        let date = "2002-09-18"
        let longestAlbum = longestAlbumByYear date albums
        longestAlbum @?= Nothing
  , testCase "Getting album with maximum average song length by year" $ do
        let albums = [Album { _albumId = "1", 
                                    _albumName = "album 1", 
                                    _albumReleaseDate = "2002-09-18", 
                                    _albumTracks = [Track {_ident = "1", 
                                                           _name = "track 1", 
                                                           _duration = 3
                                                          },
                                                    Track {_ident = "1", 
                                                           _name = "track 2", 
                                                           _duration = 3
                                                          }
                                                   ]
                                  },
                            Album {_albumId = "2", 
                                   _albumName = "album 2", 
                                   _albumReleaseDate = "2002-09-18", 
                                   _albumTracks = [Track {_ident = "2", 
                                                          _name = "track 3", 
                                                          _duration = 1
                                                         }
                                                  ]
                                  }
                     ]
        let date = "2002-09-18"
        let longestAlbum = albumWithMaxAverageSongLengthByYear date albums
        longestAlbum @?= (Just (head albums))
  , testCase "Not getting album with maximum average song length by year" $ do
        let albums = []
        let date = "2002-09-18"
        let longestAlbum = albumWithMaxAverageSongLengthByYear date albums
        longestAlbum @?= Nothing
  , testCase "Getting shortest album" $ do
        let albums = [Album { _albumId = "1", 
                                    _albumName = "album 1", 
                                    _albumReleaseDate = "2002-09-18", 
                                    _albumTracks = [Track {_ident = "1", 
                                                           _name = "track 1", 
                                                           _duration = 3
                                                          },
                                                    Track {_ident = "1", 
                                                           _name = "track 2", 
                                                           _duration = 3
                                                          }
                                                   ]
                                  },
                            Album {_albumId = "2", 
                                   _albumName = "album 2", 
                                   _albumReleaseDate = "2002-09-18", 
                                   _albumTracks = [Track {_ident = "2", 
                                                          _name = "track 3", 
                                                          _duration = 1
                                                         }
                                                  ]
                                  }
                     ]
        let date = "2002-09-18"
        let shortestAlbum = shortestAlbumByYear date albums
        shortestAlbum @?= (Just (head $ tail albums))
  , testCase "Not getting shortest album by year" $ do
        let albums = []
        let date = "2002-09-18"
        let shortestAlbum = shortestAlbumByYear date albums
        shortestAlbum @?= Nothing
  ]
