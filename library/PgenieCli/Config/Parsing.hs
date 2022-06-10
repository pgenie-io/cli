module PgenieCli.Config.Parsing where

import qualified Coalmine.HappyPathIO as HappyPathIO
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString as ByteString
import qualified PgenieCli.Config.Defaults as Defaults
import PgenieCli.Config.Model
import PgenieCli.Prelude
import YamlUnscrambler

fileInDir :: Path -> IO Project
fileInDir dir =
  HappyPathIO.readOneOf paths
    >>= either (die . to) return . document
  where
    paths =
      [ ".pgenie1",
        ".pgenie1.yaml",
        ".pgenie1.yml"
      ]
        & fmap (mappend dir)

document :: ByteString -> Either Text Project
document =
  parseByteString project

project :: Value Project
project =
  mappingValue $
    byKeyMapping (CaseSensitive True) $
      Project
        <$> atByKey "org" name
        <*> atByKey "name" name
        <*> (atByKey "migrationsDir" path <|> pure Defaults.migrationsDir)
        <*> (atByKey "queriesDir" path <|> pure Defaults.queriesDir)
        <*> (atByKey "outputDir" path <|> pure Defaults.outputDir)

name :: Value Name
name =
  scalarsValue . pure . stringScalar $
    attoparsedString "Dash-separated name" $
      lenientParser <* Attoparsec.endOfInput

path :: Value Path
path =
  scalarsValue . pure . stringScalar $
    attoparsedString "File-path" $
      lenientParser <* Attoparsec.endOfInput
