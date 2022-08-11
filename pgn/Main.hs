module Main where

import Coalmine.Prelude
import qualified Pgenie.Api.Client as Client
import qualified Pgenie.Api.Protocol as Protocol
import qualified Pgenie.Cli as App

main =
  let op = do
        Client.process (Protocol.Version 1 0 0) 1 config mempty mempty
      config =
        [i|
          space: my-space
          name: music-catalogue
          version: 1.0.0
        |]
   in traceShowM =<< Client.run op True "api.pgenie.io" Nothing
