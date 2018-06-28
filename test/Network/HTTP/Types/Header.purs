module Test.Network.HTTP.Types.Header where

import Prelude

import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe)
import Data.String (toLower, toUpper) as S
import Data.String.CodeUnits (fromCharArray, toCharArray) as S
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))

import Effect.Class (liftEffect)

import Test.Unit (TestSuite, describe, it)
import Test.Unit.Assert (shouldEqual)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Gen (Gen, arrayOf, oneOf)
import Test.QuickCheck.Arbitrary (class Coarbitrary, class Arbitrary)

import Network.HTTP.Types.Header as HD


testHeadersData :: HD.RequestHeaders
testHeadersData =
  [ HD.contentType "text/json"
  , HD.accept "text/json"
  , HD.acceptEncoding "gzip, deflate, sdch"
  , HD.userAgent "Mozilla/5.0"
  ]

newtype HeaderFieldString = HeaderFieldString String

derive instance newtypeHeaderFieldString :: Newtype HeaderFieldString _
derive newtype instance eqHeaderFieldString :: Eq HeaderFieldString
derive newtype instance ordHeaderFieldString :: Ord HeaderFieldString

instance arbHeaderFieldString :: Arbitrary HeaderFieldString where
  arbitrary = HeaderFieldString <<< S.fromCharArray <$> arrayOf anyChar
    where
    rest :: Array Char
    rest = S.toCharArray "-bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

    anyChar :: Gen Char
    anyChar = oneOf $ (pure 'a') :| (map pure rest)

derive newtype instance coarbHeaderFieldString :: Coarbitrary HeaderFieldString

testCaseInsensitive :: HeaderFieldString -> Boolean
testCaseInsensitive (HeaderFieldString field) =
  HD.ci2Head (S.toLower field) == HD.ci2Head (S.toUpper field)

main :: TestSuite
main =
  describe "Network.HTTP.Types.Header" do
    it "case insensitive" do
      liftEffect $ quickCheck testCaseInsensitive

    it "can get the existsting key" do
      let ctype = HD.lookupHeader HD.ContentType testHeadersData
      isJust ctype `shouldEqual` true
      fromMaybe false (map ((==) "text/json") ctype) `shouldEqual` true

    it "return Nothing if try to get unexisting key" do
      let k = HD.lookupHeader HD.IfRange testHeadersData
      isNothing k `shouldEqual` true

    -- Byte Range
    describe "parseByteRanges can correctly parse byte range header value" do
      it "Should return Nothing when passed invalid input" do
        isNothing (HD.parseByteRanges "error") `shouldEqual` true

      it "Should return Just with parsed ranges (input from-to)" do
        let p  = HD.parseByteRanges "bytes=0-499"
            p' = HD.parseByteRanges "bytes=500-999"
        isJust p `shouldEqual` true
        isJust p' `shouldEqual` true
        p `shouldEqual` Just (HD.ByteRangeFromTo 0 499 : Nil)
        p' `shouldEqual` Just (HD.ByteRangeFromTo 500 999 : Nil)

      it "Can parsed suffix" do
        HD.parseByteRanges "bytes=-500" `shouldEqual` Just (HD.ByteRangeSuffix 500 : Nil)

      it "ByteRange only from" do
        HD.parseByteRanges "bytes=9500-" `shouldEqual` Just (HD.ByteRangeFrom 9500 : Nil)

      it "Combined range parsed correctly" do
        let comb = HD.parseByteRanges "bytes=0-0,-1"
        comb `shouldEqual` Just (HD.ByteRangeFromTo 0 0 : HD.ByteRangeSuffix 1 : Nil)
