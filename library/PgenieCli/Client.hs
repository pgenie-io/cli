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
import qualified PgenieProtocol.Model as PgenieProtocol
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
