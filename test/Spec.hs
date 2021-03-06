import Test.Hspec
import Lib 
import Data

main :: IO ()
main = hspec $ do
 describe "formatGrid" $ do
   it "Should concatenate every line with a newline" $ do
     (formatGrid ["abc", "def", "data"])  `shouldBe` "abc\ndef\ndata\n"

 describe "findWord" $ do
   it "Should find words that exist on the Grid" $ do
     findWord grid "HASKELL" `shouldBe` Just "HASKELL"
     findWord grid "PERL" `shouldBe` Just "PERL"
   it "Should not find words that do not exists on the Grid" $ do
     findWord grid "HAMSTER" `shouldBe` Nothing

 describe "findWord" $ do
   it "Should find all the words that exist on the Grid" $ do
     findWords grid languages `shouldBe` languages
   it "Should not find words that do not exist on the Grid" $ do
     findWords grid ["FRENCH", "SPANISH"] `shouldBe` []