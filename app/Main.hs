{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import System.FilePath

data Format = MP3| M4A | FLAC deriving (Show)

data Info = Info
    { filePath  :: FilePath
    , artist    :: String
    , album     :: String
    , song      :: String
    , format    :: Format
    } deriving (Show)

main :: IO ()
main = do
    xs <- lines <$> readFile "/tmp/music.txt"
    mapM_ (print . parse) xs

parse :: FilePath -> Info
parse filePath = Info{..}
    where fn = takeFileName filePath
          song = dropExtension fn
          (artist, album) = case splitDirectories $ takeDirectory filePath of
            [ "." ] -> ("", "")
            [ ".", x, y ] -> (x, y)
            xs -> ("", "")
          format = case takeExtension fn of
            ".mp3" -> MP3
            ".m4a" -> M4A
            ".flac" -> FLAC
            x -> error $ "unknown format" <> x

