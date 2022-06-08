module PgenieCli.App (main) where

import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpClientTls
import Optics
import Optics.State.Operators
import qualified PgenieCli.Config.Model as Config
import qualified PgenieCli.Config.Parsing as Parsing
import PgenieCli.Prelude
import qualified PgenieService as Client

main :: IO ()
main = runProcess $ do
  migrations <- readMigrations
  queries <- readQueries
  response <- generate migrations queries
  handleResponse response

runProcess :: Process a -> IO a
runProcess process = do
  config <- Parsing.fileInDir mempty
  clientConfig <- Client.newConfig
  manager <- HttpClientTls.newTlsManager
  res <- runExceptT $ runReaderT process (config, manager, clientConfig)
  either (die . printBroadAs) return res

-- * Helpers

type Process =
  ReaderT Env (ExceptT Error IO)

type Env =
  (Config.Project, HttpClient.Manager, Client.PgenieServiceConfig)

data Error
  = MimeError !Client.MimeError

instance BroadPrinting Error where
  toBroadBuilder =
    error "TODO"

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

readQueries :: Process [(Text, Text)]
readQueries =
  error "TODO"

generate :: [(Text, Text)] -> [(Text, Text)] -> Process [Client.CodegenPost200ResponseInner]
generate migrations queries = do
  org <- gview (_1 % #org)
  name <- gview (_1 % #name)
  runClientRequest $ Client.codegenPost (req org name)
  where
    req org name =
      Client.CodegenPostRequest org' name' migrations' queries'
      where
        org' =
          fromNameIn #spinal org
        name' =
          fromNameIn #spinal name
        migrations' =
          migrations <&> uncurry Client.CodegenPostRequestMigrationsInner
        queries' =
          queries <&> uncurry Client.CodegenPostRequestQueriesInner

handleResponse :: [Client.CodegenPost200ResponseInner] -> Process ()
handleResponse results = do
  outputDir <- gview (_1 % #outputDir)
  forM_ results $ \(Client.CodegenPost200ResponseInner pathText contents) -> do
    path <- parsePath pathText
    liftIO $ TextIO.writeFile (printCompactAs (outputDir <> path)) contents

parsePath :: Text -> Process Path
parsePath =
  error "TODO"
