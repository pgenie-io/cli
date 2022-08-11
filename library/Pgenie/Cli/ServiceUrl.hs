module Pgenie.Cli.ServiceUrl where

import Coalmine.Prelude
import qualified Data.Attoparsec.Text as Attoparsec

data ServiceUrl = ServiceUrl
  { serviceUrlSecure :: !Bool,
    serviceUrlHost :: !Text,
    serviceUrlPort :: !(Maybe Int)
  }

instance LenientParser ServiceUrl where
  lenientParser = do
    Attoparsec.string "http"
    secure <- Attoparsec.char 's' $> True <|> pure False
    Attoparsec.string "://"
    host <- Attoparsec.takeWhile1 (\c -> c /= '/' && c /= ':')
    colon <- Attoparsec.char ':' $> True <|> pure False
    port <- if colon then Just <$> lenientParser else pure Nothing

    return $ ServiceUrl secure host port

instance Default ServiceUrl where
  def = ServiceUrl True "api.pgenie.io" Nothing

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
