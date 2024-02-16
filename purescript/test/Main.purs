module Test.Main where

import Prelude (Unit, ($))

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.CliSpec as Test.CliSpec


main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  Test.CliSpec.tests
