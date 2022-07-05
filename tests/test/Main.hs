module Main (main) where

import Prelude

import Spec (spec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec spec
