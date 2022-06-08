module PgenieCli.Config.Parsing where

import qualified Coalmine.HappyPathIO as HappyPathIO
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString as ByteString
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
        <*> (atByKey "targets" targets <|> pure (Targets True True))

name :: Value Name
name =
  scalarsValue . pure . stringScalar $
    attoparsedString "Dash-separated name" $
      (lenientParser <* Attoparsec.endOfInput)

targets :: Value Targets
targets =
  mappingValue $
    byKeyMapping (CaseSensitive True) $ do
      javaJdbc <-
        fmap (fromMaybe False) . optional . atByKey "javaJdbc" $
          scalarsValue [boolScalar]
      haskellHasql <-
        fmap (fromMaybe False) . optional . atByKey "haskellHasql" $
          scalarsValue [boolScalar]
      return $ Targets javaJdbc haskellHasql
