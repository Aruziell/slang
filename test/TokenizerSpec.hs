module TokenizerSpec (spec) where

import Test.Hspec

import Tokenizer
import qualified Token as T


spec :: Spec
spec = do
    it "single-digit integer" $ do
        tokenize "1" `shouldBe` T.IntegerToken 1
        
    it "multiple-digit integer" $ do
        tokenize "1000" `shouldBe` T.IntegerToken 1000
