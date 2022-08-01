module Pgenie.Cli.ServiceUrl where

import Coalmine.Prelude
import Data.Attoparsec.Text

data ServiceUrl = ServiceUrl
  { serviceUrlSecure :: !Bool,
    serviceUrlHost :: !Text,
    serviceUrlPort :: !(Maybe Int)
  }

instance LenientParser ServiceUrl where
  lenientParser = do
    string "http"
    secure <- char 's' $> True <|> pure False
    string "://"
    host <- takeWhile1 (\c -> c /= '/' && c /= ':')
    colon <- char ':' $> True <|> pure False
    port <- if colon then Just <$> lenientParser else pure Nothing

    return $ ServiceUrl secure host port

instance Default ServiceUrl where
  def = ServiceUrl True "pgenie.io" Nothing

instance Show ServiceUrl where
  show = to . toCompactBuilder

instance CompactPrinting ServiceUrl where
  toCompactBuilder ServiceUrl {..} =
    mconcat
      [ if serviceUrlSecure then "https" else "http",
        "://",
        to serviceUrlHost,
        foldMap (mappend ":" . showAs) serviceUrlPort
      ]
