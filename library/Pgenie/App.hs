module Pgenie.App (main) where

import qualified Coalmine.EvenSimplerPaths as Path
import Coalmine.Prelude
import qualified Data.Text.IO as TextIO
import qualified Optima
import qualified Pgenie.App.ConfigToProtocolMapping as ConfigToProtocolMapping
import qualified Pgenie.Client as Client
import qualified Pgenie.Config.Model as Config
import qualified Pgenie.Config.Parsing as Parsing
import qualified System.Directory as Directory

main :: IO ()
main = do
  (host, port, secure) <- readArgs
  config <- Parsing.fileInDir mempty
  migrations <- loadSqlFiles (#migrationsDir config)
  queries <- loadSqlFiles (#queriesDir config)
  generate secure host port config migrations queries

readArgs :: IO (Text, Maybe Int, Bool)
readArgs =
  Optima.params "pgenie.io CLI app" $
    (,,)
      <$> Optima.param
        Nothing
        "service-host"
        ( Optima.value
            "Service host"
            (Optima.explicitlyRepresented id "pgenie.io")
            Optima.unformatted
            Optima.implicitlyParsed
        )
      <*> optional
        ( Optima.param
            Nothing
            "service-port"
            ( Optima.value
                "Service port"
                Optima.defaultless
                Optima.unformatted
                Optima.implicitlyParsed
            )
        )
      <*> ( Optima.param
              Nothing
              "service-insecure"
              (Optima.flag "HTTP if present, HTTPS otherwise")
              $> True
              <|> pure False
          )

loadSqlFiles :: Path -> IO (BVec (Path, Text))
loadSqlFiles dir =
  Path.listDirectory dir
    >>= traverse load . sort . filter pred
    <&> fromList
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
  (BVec (Path, Text)) ->
  (BVec (Path, Text)) ->
  IO ()
generate secure host port config migrations queries = do
  res <- Client.runHappily op secure host port
  results <- case res of
    Left err -> die (to err)
    Right res -> return res
  forM_ results $ \(nestPath -> path, contents) -> do
    Path.createDirsTo path
    liftIO $ TextIO.writeFile (printCompactAs path) contents
  where
    op =
      Client.process (#space config) (#name config) migrations queries artifacts
      where
        artifacts =
          ConfigToProtocolMapping.artifacts (#artifacts config)
    nestPath path =
      #artifactsDir config <> path
