module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit (suite)
import Test.Unit.Main (runTest)

import Test.Network.HTTP.Types.Header as HT

main :: Effect Unit
main = runTest $ suite "Node Wai" do
  HT.main
