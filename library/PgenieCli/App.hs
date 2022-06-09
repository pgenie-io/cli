module PgenieCli.App (main) where

import qualified Coalmine.EvenSimplerPaths as Path
import qualified Data.Text.IO as TextIO
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpClientTls
import qualified PgenieCli.Config.Model as Config
import qualified PgenieCli.Config.Parsing as Parsing
import PgenieCli.Prelude
import qualified PgenieService as Client
import qualified System.Directory as Directory

main :: IO ()
main = do
  config <- Parsing.fileInDir mempty
  clientConfig <- Client.newConfig
  manager <- HttpClientTls.newTlsManager
  migrations <- loadSqlFiles (#migrationsDir config)
  queries <- loadSqlFiles (#queriesDir config)
  generate config manager clientConfig migrations queries

loadSqlFiles :: Path -> IO [(Text, Text)]
loadSqlFiles dir =
  Path.listDirectory dir >>= traverse load . sort . filter pred
  where
    pred path =
      case Path.extensions path of
        ["sql"] -> True
        ["psql"] -> True
        _ -> False
    load path =
      TextIO.readFile (printCompactAs path)
        <&> (printCompactAs path,)

generate ::
  Config.Project ->
  HttpClient.Manager ->
  Client.PgenieServiceConfig ->
  [(Text, Text)] ->
  [(Text, Text)] ->
  IO ()
generate config manager clientConfig migrations queries = do
  res <-
    Client.dispatchMime' manager clientConfig $
      Client.codegenPost $
        Client.CodegenPostRequest
          (fromNameIn #spinal (#org config))
          (fromNameIn #spinal (#name config))
          (migrations <&> uncurry Client.CodegenPostRequestMigrationsInner)
          (queries <&> uncurry Client.CodegenPostRequestQueriesInner)
  results <- case res of
    Left err -> die (to (show err))
    Right res -> return res
  forM_ results $ \(Client.CodegenPost200ResponseInner pathText contents) -> do
    path <- parsePath pathText
    liftIO $ TextIO.writeFile (printCompactAs (#outputDir config <> path)) contents

parsePath :: Text -> IO Path
parsePath text =
  case parseTextLeniently text of
    Right path -> return path
    Left err -> die (to err)
