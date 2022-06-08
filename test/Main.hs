module Main where

import Coalmine.Prelude
import Coalmine.Tasty

main =
  defaultMain . testGroup "All"
    =<< sequence
      []
