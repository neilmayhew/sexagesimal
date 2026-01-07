{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Foldable (for_)
import Numeric (readFloat, readSigned)
import Options.Applicative
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import qualified System.Console.Terminal.Size as TS

import Numeric.Sexagesimal (Sexagesimal)

data Direction = From | To
  deriving (Show)

data Options = Options
  { optNumbers :: [String]
  , optDirection :: Direction
  }
  deriving (Show)

main :: IO ()
main = do
  cols <- maybe 100 TS.width <$> TS.size

  Options {..} <-
    customExecParser
      (prefs $ columns cols)
      ( info
          ( helper <*> do
              optDirection <-
                flag To From $
                  help "Convert from sexagesimal instead of to it"
                    <> short 'f'
                    <> long "from"
              optNumbers <-
                some . strArgument $
                  help "Numbers to be converted"
                    <> metavar "NUMBER ..."
              pure Options {..}
          )
          ( fullDesc
              <> header "Convert numbers to or from sexagesimal (base 60) notation"
              <> footer
                ( unwords
                    [ "Times and angles are often expressed as sexagesimal numbers."
                    , "This program uses ':' to separate successive sexagesimal digits,"
                    , "and the digits are represented as zero-padded decimal numbers."
                    ]
                )
          )
      )

  for_ optNumbers $
    either (hPutStrLn stderr) putStrLn . convert optDirection

convert :: Direction -> String -> Either String String
convert To s = showSexagesimal . fromRational <$> readRational s
convert From s = showRational . toRational <$> readSexagesimal s

readRational :: String -> Either String Rational
readRational s = case readSigned readFloat s of
  [(r, "")] -> Right r
  _ -> Left $ "Bad rational: " <> s

showRational :: Rational -> String
showRational = show . fromRational @Double

readSexagesimal :: String -> Either String Sexagesimal
readSexagesimal s = case readMaybe s of
  Just x -> Right x
  Nothing -> Left $ "Bad sexagesimal: " <> s

showSexagesimal :: Sexagesimal -> String
showSexagesimal = show
