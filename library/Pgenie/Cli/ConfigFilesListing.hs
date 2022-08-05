module Pgenie.Cli.ConfigFilesListing
  ( chooseAndReadConfigHappily,
  )
where

import qualified Coalmine.EvenSimplerPaths as Path
import Coalmine.Prelude
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text.IO as TextIO
import qualified Safe
import qualified System.Directory as Directory

data ConfigFileName = ConfigFileName
  { configFileNameVersion :: Word,
    configFileNameSuffix :: ConfigFileNameSuffix
  }
  deriving (Eq, Ord)

instance CompactPrinting ConfigFileName where
  toCompactBuilder ConfigFileName {..} =
    ".pgenie" <> toCompactBuilder configFileNameVersion
      <> toCompactBuilder configFileNameSuffix

data ConfigFileNameSuffix
  = YamlConfigFileNameSuffix
  | YmlConfigFileNameSuffix
  | EmptyConfigFileNameSuffix
  deriving (Eq, Ord)

instance CompactPrinting ConfigFileNameSuffix where
  toCompactBuilder = \case
    YamlConfigFileNameSuffix -> ".yaml"
    YmlConfigFileNameSuffix -> ".yml"
    EmptyConfigFileNameSuffix -> ""

configFileNameParser :: Attoparsec.Parser ConfigFileName
configFileNameParser = do
  Attoparsec.char '.'
  Attoparsec.string "pgenie"
  version <- Attoparsec.decimal
  suffix <-
    asum
      [ YamlConfigFileNameSuffix <$ Attoparsec.string ".yaml",
        YmlConfigFileNameSuffix <$ Attoparsec.string ".yml",
        pure EmptyConfigFileNameSuffix
      ]
  return $ ConfigFileName version suffix

pathToName :: String -> Maybe ConfigFileName
pathToName path =
  path & to @Text & parse configFileNameParser
    & either (const Nothing) Just

selectName :: [String] -> Maybe ConfigFileName
selectName =
  Safe.maximumMay . mapMaybe pathToName

chooseAndReadConfigHappily :: IO (Word, Path, Text)
chooseAndReadConfigHappily = do
  paths <- Directory.listDirectory "."
  name <- case selectName paths of
    Just a -> return a
    Nothing -> die "No pgenie config found"
  let stringPath = printCompactAs @String name
  contents <- TextIO.readFile stringPath
  return (configFileNameVersion name, fromString stringPath, contents)
