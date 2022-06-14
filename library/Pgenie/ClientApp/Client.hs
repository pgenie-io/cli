-- |
-- A thin wrapper over lean http client.
module Pgenie.ClientApp.Client
  ( Rsc,
    Op,
    Lhc.Err (..),

    -- * Resource management
    acquire,
    operate,
    operateGlobally,

    -- * Operations
    process,
  )
where

import qualified Coalmine.EvenSimplerPaths as Path
import qualified Data.Serialize as Cereal
import qualified Data.Text.IO as TextIO
import qualified LeanHttpClient as Lhc
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpClientTls
import qualified Pgenie.ClientApp.Config.Model as Config
import Pgenie.ClientApp.Prelude
import qualified Pgenie.Protocol.V1 as Protocol
import qualified System.Directory as Directory

acquire :: IO Rsc
acquire = error "TODO"

operate :: Rsc -> Op a -> IO (Either Lhc.Err a)
operate =
  error "TODO"

-- | Execute operation on global manager.
operateGlobally :: Op a -> IO (Either Lhc.Err a)
operateGlobally =
  Lhc.runSessionOnGlobalManager

type Rsc = HttpClient.Manager

type Op = Lhc.Session

-- * Operations

executeRequest :: Protocol.Request -> Op Protocol.Response
executeRequest req =
  Lhc.post url headers requestBody parser
  where
    url =
      Lhc.url https host port path query
      where
        https = True
        host = "pgenie.tech"
        port = Nothing
        path = "/api/v1"
        query = []
    headers =
      mempty
    requestBody =
      Cereal.encode req
    parser =
      Lhc.deserializeBodyWithCereal Cereal.get

process :: Name -> Name -> [(Path, Text)] -> [(Path, Text)] -> Op (Either Text [(Path, Text)])
process org name migrations queries = do
  fmap mapOut $
    executeRequest $
      Protocol.ProcessRequest $
        Protocol.RequestProcess org name migrations queries
  where
    mapOut = \case
      Protocol.FailedResponse err -> Left err
      Protocol.GeneratedResponse res -> Right res
