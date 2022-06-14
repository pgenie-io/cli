module Main where

import Coalmine.Prelude
import qualified Pgenie.ClientApp as App

main = App.main False "localhost" (Just 27400)
