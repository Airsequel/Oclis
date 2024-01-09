import Protolude (IO, ($))
import Test.Hspec (hspec, shouldBe, describe, it)

import Lib (sayHello)

main :: IO ()
main = hspec $ do
  describe "Haskell Template" $ do
    it "prints 'Hello World!'" $ do
      sayHello "World" `shouldBe` "Hello World!"
