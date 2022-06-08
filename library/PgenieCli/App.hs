module PgenieCli.App (main) where

import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpClientTls
import qualified PgenieCli.Config.Model as Config
import qualified PgenieCli.Config.Parsing as Parsing
import PgenieCli.Prelude
import qualified PgenieService as Client

main :: IO ()
main = do
  config <- Parsing.fileInDir mempty
  clientConfig <- Client.newConfig
  manager <- HttpClientTls.newTlsManager
  migrations <- readMigrations (#migrationsDir config)
  queries <- readQueries (#queriesDir config)
  generate manager clientConfig (#outputDir config) (#org config) (#name config) migrations queries

readMigrations :: Path -> IO [(Text, Text)]
readMigrations =
  error "TODO"

readQueries :: Path -> IO [(Text, Text)]
readQueries =
  error "TODO"

generate ::
  HttpClient.Manager ->
  Client.PgenieServiceConfig ->
  Path ->
  Name ->
  Name ->
  [(Text, Text)] ->
  [(Text, Text)] ->
  IO ()
generate manager clientConfig outputDir org name migrations queries = do
  res <-
    Client.dispatchMime' manager clientConfig $
      Client.codegenPost $
        Client.CodegenPostRequest
          (fromNameIn #spinal org)
          (fromNameIn #spinal name)
          (migrations <&> uncurry Client.CodegenPostRequestMigrationsInner)
          (queries <&> uncurry Client.CodegenPostRequestQueriesInner)
  results <- case res of
    Left err -> die (to (show err))
    Right res -> return res
  forM_ results $ \(Client.CodegenPost200ResponseInner pathText contents) -> do
    path <- parsePath pathText
    liftIO $ TextIO.writeFile (printCompactAs (outputDir <> path)) contents

parsePath :: Text -> IO Path
parsePath text =
  case parseTextLeniently text of
    Right path -> return path
    Left err -> die (to err)
