module Pgenie.ClientApp (main) where

import qualified Coalmine.BaseExtras.List as List
import qualified Coalmine.EvenSimplerPaths as Path
import qualified Coalmine.MultilineTextBuilder as Mb
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
  traceM [i|Requesting|]
  res <- Client.operateGlobally op secure host port
  results <- case res of
    Left err -> die (to err)
    Right res -> case res of
      Left err -> die (to err)
      Right res -> return res
  traceM $
    let results' =
          results
            & List.mapIntercalate (mappend "- " . Mb.indent 2 . toBroadBuilder) "\n"
     in [i|
          Results:
            $results'
        |]
  forM_ results $ \(path, contents) -> do
    let path' = #outputDir config <> path
    Path.createDirsTo path'
    liftIO $ TextIO.writeFile (printCompactAs path') contents
  where
    op =
      Client.process (#org config) (#name config) migrations queries
