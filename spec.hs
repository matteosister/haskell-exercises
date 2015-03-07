import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Exercises

main :: IO ()
main = hspec $ do
  describe "lists" $ do
  	it "1. Find the last element of a list" $ do
  		myLast [1,2,3,4] `shouldBe` (4 :: Int)
  		myLast ['x','y','z'] `shouldBe` ('z' :: Char)

  	it "2. Find the last but one element of a list" $ do
  		myButLast [1,2,3,4] `shouldBe` (3 :: Int)
  		myButLast ['a'..'z'] `shouldBe` ('y' :: Char)

 	it "8. Eliminate consecutive duplicates of list elements" $ do
 		compress "aaaabccaadeeee" `shouldBe` ("abcade" :: String)
 		compress "abc" `shouldBe` ("abc" :: String)
 		compress [1,2,3,4,4,5] `shouldBe` ([1,2,3,4,5] :: [Int])