module Network.HTTP.Types.Header where

import Prelude

import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldMap, intercalate)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.String as S
import Data.Tuple (Tuple(..))

data HeaderName
  = Accept
  | AcceptCharset
  | AcceptEncoding
  | AcceptLanguage
  | Allow
  | Authorization
  | CacheControl
  | Cookie
  | Connection
  | ContentEncoding
  | ContentLanguage
  | ContentLength
  | ContentLocation
  | ContentMD5
  | ContentRange
  | ContentType
  | Date
  | Expect
  | Expires
  | From
  | Host
  | IfMatch
  | IfModifiedSince
  | IfNoneMatch
  | IfRange
  | IfUnmodifiedSince
  | LastModified
  | MaxForwards
  | Pragma
  | ProxyAuthorization
  | Range
  | Referer
  | SetCookie
  | TE
  | Trailer
  | TransferEncoding
  | Upgrade
  | UserAgent
  | Via
  | Warning
  | Custom String

data Header = Header HeaderName String

-- | Response Headers
type ResponseHeaders = Array Header

-- | Request Headers.
type RequestHeaders = Array Header

derive instance eqHeaderName :: Eq HeaderName

derive instance ordHeaderName :: Ord HeaderName

derive instance eqHeader :: Eq Header

derive instance ordHeader :: Ord Header

instance showHeader :: Show Header where
  show (Header name val) = show name <> ": " <> val

instance showHeaderName :: Show HeaderName where
  show Accept             = "Accept"
  show AcceptCharset      = "Accept-Charset"
  show AcceptEncoding     = "Accept-Encoding"
  show AcceptLanguage     = "Accept-Language"
  show Allow              = "Allow"
  show Authorization      = "Authorization"
  show CacheControl       = "Cache-Control"
  show Cookie             = "Cookie"
  show Connection         = "Connection"
  show ContentEncoding    = "Content-Encoding"
  show ContentLanguage    = "Content-Language"
  show ContentLength      = "Content-Length"
  show ContentLocation    = "Content-Location"
  show ContentMD5         = "Content-MD5"
  show ContentRange       = "Content-Range"
  show ContentType        = "Content-Type"
  show Date               = "Date"
  show Expect             = "Expect"
  show Expires            = "Expires"
  show From               = "From"
  show Host               = "Host"
  show IfMatch            = "If-Match"
  show IfModifiedSince    = "If-Modified-Since"
  show IfNoneMatch        = "If-None-Match"
  show IfRange            = "If-Range"
  show IfUnmodifiedSince  = "If-Unmodified-Since"
  show LastModified       = "Last-Modified"
  show MaxForwards        = "Max-Forwards"
  show Pragma             = "Pragma"
  show ProxyAuthorization = "Proxy-Authorization"
  show Range              = "Range"
  show Referer            = "Referer"
  show SetCookie          = "Set-Cookie"
  show TE                 = "TE"
  show Trailer            = "Trailer"
  show TransferEncoding   = "Transfer-Encoding"
  show Upgrade            = "Upgrade"
  show UserAgent          = "User-Agent"
  show Via                = "Via"
  show Warning            = "Warning"
  show (Custom header)    = header

lookupHeader :: forall f. Foldable f => HeaderName -> f Header -> Maybe String
lookupHeader a = unwrap <<< foldMap \(Header a' b) -> First (if a == a' then Just b else Nothing)

getHeaderName :: Header -> HeaderName
getHeaderName (Header h _) = h

getHeaderValue :: Header -> String
getHeaderValue (Header _ s) = s

-- | Build HeaderName by case insensivetive string, this format used by Node js
ci2Head :: String -> HeaderName
ci2Head s
  | S.toLower s == "te" = TE
  | otherwise           =
      let toTitleCase st = (S.toUpper (S.take 1 st)) <> (S.toLower (S.drop 1 st))
      in string2Head $ intercalate "-" (map toTitleCase (S.split (S.Pattern "-") s))

tuple2Header :: Tuple HeaderName String -> Header
tuple2Header (Tuple n v) = Header n v

header2Tuple :: Header -> Tuple HeaderName String
header2Tuple (Header n v) = Tuple n v

fromHeaders :: Array (Tuple String String) -> Array Header
fromHeaders = map (tuple2Header <<< lmap ci2Head)

string2Head :: String -> HeaderName
string2Head "Accept"              = Accept
string2Head "Accept-Charset"      = AcceptCharset
string2Head "Accept-Encoding"     = AcceptEncoding
string2Head "Accept-Language"     = AcceptLanguage
string2Head "Allow"               = Allow
string2Head "Authorization"       = Authorization
string2Head "Cache-Control"       = CacheControl
string2Head "Cookie"              = Cookie
string2Head "Connection"          = Connection
string2Head "Content-Encoding"    = ContentEncoding
string2Head "Content-Language"    = ContentLanguage
string2Head "Content-Length"      = ContentLength
string2Head "Content-Location"    = ContentLocation
string2Head "Content-MD5"         = ContentMD5
string2Head "Content-Range"       = ContentRange
string2Head "Content-Type"        = ContentType
string2Head "Date"                = Date
string2Head "Expect"              = Expect
string2Head "Expires"             = Expires
string2Head "From"                = From
string2Head "Host"                = Host
string2Head "If-Match"            = IfMatch
string2Head "If-Modified-Since"   = IfModifiedSince
string2Head "If-None-Match"       = IfNoneMatch
string2Head "If-Range"            = IfRange
string2Head "If-Unmodified-Since" = IfUnmodifiedSince
string2Head "Last-Modified"       = LastModified
string2Head "Max-Forwards"        = MaxForwards
string2Head "Pragma"              = Pragma
string2Head "Proxy-Authorization" = ProxyAuthorization
string2Head "Range"               = Range
string2Head "Referer"             = Referer
string2Head "Set-Cookie"          = SetCookie
string2Head "TE"                  = TE
string2Head "Trailer"             = Trailer
string2Head "Transfer-Encoding"   = TransferEncoding
string2Head "Upgrade"             = Upgrade
string2Head "User-Agent"          = UserAgent
string2Head "Via"                 = Via
string2Head "Warning"             = Warning
string2Head header                = (Custom header)

accept :: String -> Header
accept = Header Accept

acceptCharset :: String -> Header
acceptCharset = Header AcceptCharset

acceptEncoding :: String -> Header
acceptEncoding = Header AcceptEncoding

acceptLanguage :: String -> Header
acceptLanguage = Header AcceptLanguage

allow :: String -> Header
allow = Header Allow

authorization :: String -> Header
authorization = Header Authorization

cacheControl :: String -> Header
cacheControl = Header CacheControl

cookie :: String -> Header
cookie = Header Cookie

connection :: String -> Header
connection = Header Connection

contentEncoding :: String -> Header
contentEncoding = Header ContentEncoding

contentLanguage :: String -> Header
contentLanguage = Header ContentLanguage

contentLength :: String -> Header
contentLength = Header ContentLength

contentLocation :: String -> Header
contentLocation = Header ContentLocation

contentMD5 :: String -> Header
contentMD5 = Header ContentMD5

contentRange :: String -> Header
contentRange = Header ContentRange

contentType :: String -> Header
contentType = Header ContentType

date :: String -> Header
date = Header Date

expect :: String -> Header
expect = Header Expect

expires :: String -> Header
expires = Header Expires

from :: String -> Header
from = Header From

host :: String -> Header
host = Header Host

ifMatch :: String -> Header
ifMatch = Header IfMatch

ifModifiedSince :: String -> Header
ifModifiedSince = Header IfModifiedSince

ifNoneMatch :: String -> Header
ifNoneMatch = Header IfNoneMatch

ifRange :: String -> Header
ifRange = Header IfRange

ifUnmodifiedSince :: String -> Header
ifUnmodifiedSince = Header IfUnmodifiedSince

lastModified :: String -> Header
lastModified = Header LastModified

maxForwards :: String -> Header
maxForwards = Header MaxForwards

pragma :: String -> Header
pragma = Header Pragma

proxyAuthorization :: String -> Header
proxyAuthorization = Header ProxyAuthorization

range :: String -> Header
range = Header Range

referer :: String -> Header
referer = Header Referer

setCookie :: String -> Header
setCookie = Header SetCookie

tE :: String -> Header
tE = Header TE

trailer :: String -> Header
trailer = Header Trailer

transferEncoding :: String -> Header
transferEncoding = Header TransferEncoding

upgrade :: String -> Header
upgrade = Header Upgrade

userAgent :: String -> Header
userAgent = Header UserAgent

via :: String -> Header
via = Header Via

warning :: String -> Header
warning = Header Warning

customString :: String -> String -> Header
customString s = Header (Custom s)

data ByteRange
  = ByteRangeFrom Int
  | ByteRangeFromTo Int Int
  | ByteRangeSuffix Int

derive instance eqByteRange :: Eq ByteRange

instance showByteRange :: Show ByteRange where
  show (ByteRangeFrom b) = "(ByteRangeFrom " <> show b <> " )"
  show (ByteRangeFromTo a b) = "(ByteRangeFromTo " <> show a <> " " <> show b <> " )"
  show (ByteRangeSuffix co) = "(ByteRangeSuffix " <> show co <> " )"

type ByteRanges = Array ByteRange

renderByteRange :: ByteRange -> String
renderByteRange (ByteRangeFrom beg)      = show beg <> "-"
renderByteRange (ByteRangeFromTo beg to) = show beg <> "-" <> show to
renderByteRange (ByteRangeSuffix suffix) = "-" <> show suffix

renderByteRanges :: ByteRanges -> String
renderByteRanges xs = "bytes="
  <> intercalate "," (map renderByteRange xs)

parseByteRanges :: String -> Maybe (List ByteRange)
parseByteRanges bs1 = do
  bs2 <- S.stripPrefix (S.Pattern "bytes=") bs1
  Tuple r bs3 <- _range bs2
  ranges ((:) r) bs3
  where
    _range bs2 = do
      Tuple i bs3 <- readInteger bs2
      if i < 0
        then Just $ Tuple (ByteRangeSuffix (negate i)) bs3
        else do
          bs4 <- S.stripPrefix (S.Pattern "-") bs3
          case readInteger bs4 of
            Just (Tuple j bs5) | j >= i -> Just $ Tuple (ByteRangeFromTo i j) bs5
            _ -> Just $ Tuple (ByteRangeFrom i) bs4
    ranges front bs3
      | bs3 == "" = Just (front Nil)
      | otherwise = do
          bs4 <- S.stripPrefix (S.Pattern ",") bs3
          Tuple r bs5 <- _range bs4
          ranges (front <<< ((:) r)) bs5

readInteger :: String -> Maybe (Tuple Int String)
readInteger st = runFn4 readIntegerImpl Nothing Just Tuple st

foreign import readIntegerImpl
  :: Fn4
      (forall a. Maybe a)
      (forall a. a -> Maybe a)
      (forall a b. a -> b -> Tuple a b)
      String
      (Maybe (Tuple Int String))
