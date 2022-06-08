module PgenieCli.App (run) where

import qualified Network.HTTP.Client as HttpClient
import qualified PgenieCli.Config.Model as Config
import PgenieCli.Prelude
import qualified PgenieService as Client

run :: IO a
run =
  error "TODO"

-- * Helpers

type Process =
  ReaderT Env (ExceptT Error IO)

type Env =
  (Config.Project, HttpClient.Manager, Client.PgenieServiceConfig)

data Error
  = MimeError !Client.MimeError

runClientRequest ::
  (Client.Produces req accept, Client.MimeUnrender accept res, Client.MimeType contentType) =>
  Client.PgenieServiceRequest req contentType res accept ->
  Process res
runClientRequest req =
  ReaderT $ \(_, manager, clientConfig) ->
    ExceptT $ fmap (first MimeError) $ Client.dispatchMime' manager clientConfig req

readMigrations :: Process [(Text, Text)]
readMigrations =
  error "TODO"

publish :: Client.CodegenPostRequest -> Process [Client.CodegenPost200ResponseInner]
publish req =
  runClientRequest $ Client.codegenPost req
