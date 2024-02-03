{-# LANGUAGE OverloadedRecordDot #-}

import Protolude (IO, ($))
import Test.Hspec (describe, hspec, it, shouldBe)

import Oclis


main :: IO ()
main = hspec $ do
  describe "Oclis" $ do
    it "has a default app" $ do
      defaultApp.name `shouldBe` "oclis"
