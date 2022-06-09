-- |
-- A thin wrapper over lean http client.
module PgenieCli.Client
  ( Rsc,
    Op,
    Lhc.Err (..),

    -- * Resource management
    acquire,
    operate,
    operateGlobally,

    -- * Operations
  )
where

import qualified Coalmine.EvenSimplerPaths as Path
import qualified Data.Text.IO as TextIO
import qualified LeanHttpClient as Lhc
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpClientTls
import qualified PgenieCli.Config.Model as Config
import PgenieCli.Prelude
import qualified PgenieProtocol.V1 as Protocol
import qualified System.Directory as Directory

acquire :: IO Rsc
acquire = error "TODO"

operate :: Rsc -> Op a -> IO (Either Lhc.Err a)
operate =
  error "TODO"

-- | Execute operation on global manager.
operateGlobally :: Op a -> IO (Either Lhc.Err a)
operateGlobally =
  error "TODO"

type Rsc = HttpClient.Manager

type Op = Lhc.Session

-- * Operations

executeRequest :: Protocol.Request -> Op Protocol.Response
executeRequest req =
  Lhc.performPost
    timeout
    maxRedirects
    secure
    host
    Nothing
    "/api/v1"
    []
    mempty
    (error "TODO: serialize")
    (error "TODO: parse")
  where
    timeout = 15
    maxRedirects = 3
    secure = True
    host = "pgenie.tech"

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
