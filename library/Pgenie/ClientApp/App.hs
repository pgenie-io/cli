module Pgenie.ClientApp.App (main) where

import qualified Coalmine.EvenSimplerPaths as Path
import qualified Data.Text.IO as TextIO
import qualified Pgenie.ClientApp.Client as Client
import qualified Pgenie.ClientApp.Config.Model as Config
import qualified Pgenie.ClientApp.Config.Parsing as Parsing
import Pgenie.ClientApp.Prelude
import qualified System.Directory as Directory

main :: IO ()
main = do
  config <- Parsing.fileInDir mempty
  migrations <- loadSqlFiles (#migrationsDir config)
  queries <- loadSqlFiles (#queriesDir config)
  generate config migrations queries

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
  Config.Project ->
  [(Path, Text)] ->
  [(Path, Text)] ->
  IO ()
generate config migrations queries = do
  res <-
    Client.operateGlobally $
      Client.process (#org config) (#name config) migrations queries
  results <- case res of
    Left err -> die (to (show err))
    Right res -> case res of
      Left err -> die (to err)
      Right res -> return res
  forM_ results $ \(path, contents) -> do
    liftIO $ TextIO.writeFile (printCompactAs (#outputDir config <> path)) contents
