module Pgenie.Cli (main) where

import qualified Coalmine.EvenSimplerPaths as Path
import Coalmine.Prelude
import qualified Data.Text.IO as TextIO
import qualified Data.Version as Version
import qualified Optima
import qualified Paths_pgenie_cli as CabalMetadata
import qualified Pgenie.Api.Client as Client
import qualified Pgenie.Cli.ConfigFilesListing as ConfigFilesListing
import qualified Pgenie.Cli.ServiceUrl as ServiceUrl
import qualified System.Directory as Directory

main :: IO ()
main = do
  ServiceUrl.ServiceUrl {..} <- readArgs
  (configVersion, configPath, configContents) <- ConfigFilesListing.chooseAndReadConfigHappily
  migrations <- loadSqlFiles "migrations"
  queries <- loadSqlFiles "queries"
  generate serviceUrlSecure serviceUrlHost serviceUrlPort configVersion configContents migrations queries

readArgs :: IO ServiceUrl.ServiceUrl
readArgs =
  Optima.params "pgenie.io CLI app" $
    Optima.param
      Nothing
      "service-url"
      ( Optima.value
          "Service URL (for development purposes)"
          (Optima.explicitlyRepresented showAs def)
          Optima.unformatted
          Optima.implicitlyParsed
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
  Word ->
  Text ->
  (BVec (Path, Text)) ->
  (BVec (Path, Text)) ->
  IO ()
generate secure host port configVersion configContents migrations queries = do
  res <- Client.run op secure host port
  res <- case res of
    Left err -> case err of
      Client.TimeoutErr -> die [i|Failure connecting to $host|]
      Client.NetworkErr _ -> die [i|Failure connecting to $host|]
      Client.ResponseParsingErr _ ->
        die
          [i|
            Unexpected response from $host.
            You probably need to update this app.
            Visit https://github.com/pgenie-io/cli for installation instructions.
          |]
    Right res -> return res
  results <- case res of
    Left err -> die (to err)
    Right res -> return res
  forM_ results $ \(nestPath -> path, contents) -> do
    Path.createDirsTo path
    liftIO $ TextIO.writeFile (printCompactAs path) contents
  where
    op =
      Client.process
        clientVersion
        configVersion
        configContents
        migrations
        queries
    nestPath path =
      "artifacts" <> path

clientVersion :: Text
clientVersion =
  CabalMetadata.version
    & Version.showVersion
    & to @Text