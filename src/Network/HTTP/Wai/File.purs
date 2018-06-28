module Network.HTTP.Wai.File
  ( FileInfo(..)
  , getFileInfo
  , RspFileInfo(..)
  , conditionalRequest
  , addContentHeadersForFilePart
  , createReadStreamRange
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)

import Data.Enum (fromEnum)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Int (fromNumber)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Partial.Unsafe (unsafePartial)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (error)

import Node.FS.Aff as F
import Node.FS.Stats (Stats(..), isFile, modifiedTime)
import Node.Path (FilePath)
import Node.Stream (Readable())

import Network.HTTP.Types as H
import Network.HTTP.Types.Header (parseByteRanges)
import Network.HTTP.Types.Date (HTTPDate, parseHTTPDate, fromDateTime, formatHTTPDate)
import Network.HTTP.Wai.Header (RequestHeaderKey(..), HeaderMap)
import Network.HTTP.Wai.Internal (FilePart(..))


newtype FileInfo = FileInfo
  { name :: FilePath
  , size :: Int
  , time :: HTTPDate -- ^ Modification time
  , date :: String -- ^ Modification time in the GMT format
  }

derive instance eqFileInfo :: Eq FileInfo

instance showFileInfo :: Show FileInfo where
  show (FileInfo s) = "(FileInfo { name: "
    <> show s.name
    <> "\n, size: "
    <> show s.size
    <> "\n, time: "
    <> show s.time
    <> "\n, date: "
    <> show s.date
    <> "\n})"

getFileInfo :: FilePath -> Aff (Tuple Stats FileInfo)
getFileInfo fp = do
  fs <- F.stat fp
  when (not $ isFile fs) do
    throwError (error "Not a file")
  pure $ Tuple fs $ FileInfo
    { name: fp
    , size: fileSizeInfo fs
    -- todo dry the following up
    , time: fromDateTime $ modifiedTime fs
    , date: formatHTTPDate (fromDateTime $ modifiedTime fs)
    }

fileSizeInfo :: Stats -> Int
fileSizeInfo (Stats s) = unsafePartial $ fromJust (fromNumber s.size)

data RspFileInfo
  = WithoutBody H.Status
  | WithBody H.Status H.ResponseHeaders Int Int

derive instance eqRspFileInfo :: Eq RspFileInfo

conditionalRequest :: FileInfo -> H.ResponseHeaders -> HeaderMap -> RspFileInfo
conditionalRequest (FileInfo finfo) hs0 hm = case condition of
  nobody@(WithoutBody _) -> nobody
  WithBody s _ off len ->
    let hs = [H.lastModified finfo.date] <> addContentHeaders hs0 off len finfo.size
    in WithBody s hs off len
  where
    mcondition = ifmodified hm finfo.size finfo.time
      <|> ifunmodified hm finfo.size finfo.time
      <|> ifrange hm finfo.size finfo.time
    condition = fromMaybe (unconditional hm finfo.size) mcondition

ifModifiedSince :: HeaderMap -> Maybe HTTPDate
ifModifiedSince hm = M.lookup (fromEnum ReqIfModifiedSince) hm >>= parseHTTPDate

ifUnmodifiedSince :: HeaderMap -> Maybe HTTPDate
ifUnmodifiedSince hm = M.lookup (fromEnum ReqIfUnmodifiedSince) hm >>= parseHTTPDate

ifRange :: HeaderMap -> Maybe HTTPDate
ifRange hm = M.lookup (fromEnum ReqIfRange) hm >>= parseHTTPDate

ifmodified :: HeaderMap -> Int -> HTTPDate -> Maybe RspFileInfo
ifmodified hm size mtime = do
  date <- ifModifiedSince hm
  pure $ if date /= mtime then unconditional hm size else WithoutBody H.status304

ifunmodified :: HeaderMap -> Int -> HTTPDate -> Maybe RspFileInfo
ifunmodified hm size mtime = do
  date <- ifUnmodifiedSince hm
  pure $ if date == mtime then unconditional hm size else WithoutBody H.status412

ifrange :: HeaderMap -> Int -> HTTPDate -> Maybe RspFileInfo
ifrange hm size mtime = do
  date <- ifRange hm
  rng <- M.lookup (fromEnum ReqRange) hm
  pure $ if date == mtime then parseRange rng size else WithBody H.status200 [] 0 size

unconditional :: HeaderMap -> Int -> RspFileInfo
unconditional hm size = case M.lookup (fromEnum ReqRange) hm of
  Nothing -> WithBody H.status200 [] 0 size
  Just rng -> parseRange rng size

parseRange :: String -> Int -> RspFileInfo
parseRange rn size = case parseByteRanges rn of
  Nothing     -> WithoutBody H.status416
  Just Nil    -> WithoutBody H.status416
  Just (r:_)  ->
    let Tuple beg end = checkRange r size
        len           = end - beg + 1
        s             = if beg == 0 && end == size - 1 then H.status200 else H.status206
    in WithBody s [] beg len

checkRange :: H.ByteRange -> Int -> Tuple Int Int
checkRange (H.ByteRangeFrom   beg)     size = Tuple beg (size - 1)
checkRange (H.ByteRangeFromTo beg end) size = Tuple beg (min (size - 1) end)
checkRange (H.ByteRangeSuffix count)   size = Tuple (max 0 (size - count)) (size - 1)

contentRangeHeader :: Int -> Int -> Int -> H.Header
contentRangeHeader beg end total = H.contentRange byterange
  where
    byterange = "bytes " <> (if beg > end then "*" else show beg <> "-" <> show end) <> "/" <> show total

addContentHeaders :: H.ResponseHeaders -> Int -> Int -> Int -> H.ResponseHeaders
addContentHeaders hs off len size =
  if len == size
    then hs'
    else
      let cr = contentRangeHeader off (off + len - 1) size
      in [cr] <> hs'
  where
    hs' = [H.contentLength (show len), H.customString "Accept-Range" "bytes"] <> hs

addContentHeadersForFilePart :: H.ResponseHeaders -> FilePart -> H.ResponseHeaders
addContentHeadersForFilePart hs (FilePart fp) = addContentHeaders hs off len size
  where
    off = fp.offset
    len = fp.byteCount
    size = fp.size

createReadStreamRange :: FilePath -> Int -> Int -> Effect (Readable ())
createReadStreamRange path start end = runFn3 createReadStreamRangeImpl path start end

foreign import createReadStreamRangeImpl :: Fn3 FilePath Int Int (Effect (Readable ()))
