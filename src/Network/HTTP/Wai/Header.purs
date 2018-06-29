module Network.HTTP.Wai.Header where

import Prelude

import Data.Array as A
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Enum (class Enum, class BoundedEnum, toEnum, fromEnum, Cardinality(..))

import Network.HTTP.Types.Header (Header(..), HeaderName(..), RequestHeaders)

type HeaderMap = M.Map Int String

data RequestHeaderKey
  = ReqContentLength
  | ReqTransferEncoding
  | ReqExpect
  | ReqConnection
  | ReqRange
  | ReqHost
  | ReqIfModifiedSince
  | ReqIfUnmodifiedSince
  | ReqIfRange
  | ReqReferer
  | ReqUserAgent

derive instance eqRequestHeaderKey :: Eq RequestHeaderKey
derive instance ordRequestHeaderKey :: Ord RequestHeaderKey

instance boundedRequestHeaderKey :: Bounded RequestHeaderKey where
  bottom = ReqContentLength
  top = ReqUserAgent

instance enumRequestHeaderKey :: Enum RequestHeaderKey where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumRequestHeaderKey :: BoundedEnum RequestHeaderKey where
  cardinality = Cardinality 11
  toEnum = case _ of
    1 -> Just ReqContentLength
    2 -> Just ReqTransferEncoding
    3 -> Just ReqExpect
    4 -> Just ReqConnection
    5 -> Just ReqRange
    6 -> Just ReqHost
    7 -> Just ReqIfModifiedSince
    8 -> Just ReqIfUnmodifiedSince
    9 -> Just ReqIfRange
    10 -> Just ReqReferer
    11 -> Just ReqUserAgent
    _ -> Nothing
  fromEnum = case _ of
    ReqContentLength -> 1
    ReqTransferEncoding -> 2
    ReqExpect -> 3
    ReqConnection -> 4
    ReqRange -> 5
    ReqHost -> 6
    ReqIfModifiedSince -> 7
    ReqIfUnmodifiedSince -> 8
    ReqIfRange -> 9
    ReqReferer -> 10
    ReqUserAgent -> 11

getReqKey :: HeaderName -> Maybe Int
getReqKey hd = fromEnum <$> key
  where
    key :: Maybe RequestHeaderKey
    key = case hd of
      ContentLength -> Just ReqContentLength
      TransferEncoding -> Just ReqTransferEncoding
      Expect -> Just ReqExpect
      Connection -> Just ReqConnection
      Range -> Just ReqRange
      Host -> Just ReqHost
      IfModifiedSince -> Just ReqIfModifiedSince
      IfUnmodifiedSince -> Just ReqIfUnmodifiedSince
      IfRange ->  Just ReqIfRange
      Referer -> Just ReqReferer
      UserAgent -> Just ReqUserAgent
      _ -> Nothing

keyedRequestHeader :: RequestHeaders -> HeaderMap
keyedRequestHeader = traverseHeader getReqKey

traverseHeader :: (HeaderName -> Maybe Int) -> Array Header -> HeaderMap
traverseHeader f = M.fromFoldable <<< A.mapMaybe (\(Header k v) -> flip Tuple v <$> f k)
