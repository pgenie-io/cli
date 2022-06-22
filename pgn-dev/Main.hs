module Main where

import Coalmine.Prelude
import qualified Pgenie.App as App

main = App.main False "localhost" (Just 27400)
