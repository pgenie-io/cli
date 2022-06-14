module Pgenie.ClientApp (main) where

import qualified Coalmine.EvenSimplerPaths as Path
import qualified Data.Text.IO as TextIO
import qualified Pgenie.Client as Client
import Pgenie.ClientApp.Prelude
import qualified Pgenie.Config.Model as Config
import qualified Pgenie.Config.Parsing as Parsing
import qualified System.Directory as Directory

main ::
  Bool ->
  Text ->
  Maybe Int ->
  IO ()
main secure host port = do
  config <- Parsing.fileInDir mempty
  migrations <- loadSqlFiles (#migrationsDir config)
  queries <- loadSqlFiles (#queriesDir config)
  generate secure host port config migrations queries

loadSqlFiles :: Path -> IO [(Path, Text)]
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
        <&> (path,)

generate ::
  Bool ->
  Text ->
  Maybe Int ->
  Config.Project ->
  [(Path, Text)] ->
  [(Path, Text)] ->
  IO ()
generate secure host port config migrations queries = do
  res <- Client.operateGlobally op secure host port
  results <- case res of
    Left err -> die (to err)
    Right res -> case res of
      Left err -> die (to err)
      Right res -> return res
  forM_ results $ \(path, contents) -> do
    liftIO $ TextIO.writeFile (printCompactAs (#outputDir config <> path)) contents
  where
    op =
      Client.process (#org config) (#name config) migrations queries
