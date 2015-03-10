import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import List1
import List2

main :: IO ()
main = hspec $ do
	describe "lists 1" $ do
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

		it "9. Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists." $ do
			pack ['a','a','a','a','b','c','c','a','a','d','e','e','e','e'] `shouldBe` (["aaaa","b","cc","aa","d","eeee"])

		it "10. Run-length encoding of a list" $ do
			encode "aaaabccaadeeee" `shouldBe` ([(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] :: [(Int,Char)])
			encode "aabbcde" `shouldBe` ([(2,'a'),(2,'b'),(1,'c'),(1,'d'),(1,'e')] :: [(Int,Char)])

	describe "lists 2" $ do
		it "run Run-length with single/multiple elements" $ do
			encodeModified "aaaabccdeeee" `shouldBe` ([Multiple 4 'a',Single 'b',Multiple 2 'c',Single 'd',Multiple 4 'e'] :: [Element Char])
			encodeModified "a" `shouldBe` ([Single 'a'] :: [Element Char])