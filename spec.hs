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
		it "11. run Run-length with single/multiple elements" $ do
			encodeModified "aaaabccdeeee" `shouldBe` ([Multiple 4 'a',Single 'b',Multiple 2 'c',Single 'd',Multiple 4 'e'] :: [Element Char])
			encodeModified "a" `shouldBe` ([Single 'a'] :: [Element Char])

		it "12. Decode a run-length encoded list." $ do
			decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'] `shouldBe` "aaaabccaadeeee"
			decodeModified [Multiple 2 'a',Single 'b'] `shouldBe` "aab"

		it "14. Duplicate the elements of a list." $ do
			dupli [1, 2, 3] `shouldBe` [1,1,2,2,3,3]
			dupli' [1, 2, 3] `shouldBe` [1,1,2,2,3,3]

		it "15. Replicate the elements of a list a given number of times" $ do
			repli "abc" 3 `shouldBe` "aaabbbccc"
			repli [1,2] 2 `shouldBe` [1,1,2,2]

		it "16. Drop every N'th element from a list." $ do
			dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
			dropEvery "" 3 `shouldBe` ""
			--dropEvery [] 2 `shouldBe` ([])

		it "17. Split a list into two parts; the length of the first part is given." $ do
			split "abcdefghik" 3 `shouldBe` ("abc", "defghik")
			split "abc" 3 `shouldBe` ("abc", "")
			split "abc" 5 `shouldBe` ("abc", "")

		it "18. Extract a slice from a list." $ do
			slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"
			slice ['a','b','c','d','e','f','g','h','i','k'] 3 100 `shouldBe` "cdefghik"
			slice ['a','b','c','d','e','f','g','h','i','k'] 5 6 `shouldBe` "ef"
			slice ['a','b'] 3 100 `shouldBe` ""