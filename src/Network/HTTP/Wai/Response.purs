module Network.HTTP.Wai.Response where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Data.Array (deleteBy)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)

import Node.HTTP as NH
import Node.Buffer as B
import Node.Path (FilePath)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (Readable, end, pipe, write) as ST
import Node.FS.Aff as FSA
import Node.FS.Stats (Stats, isDirectory)

import Network.HTTP.Wai (responseStatus, responseHeaders)
import Network.HTTP.Wai.Effects (WaiEffects)
import Network.HTTP.Wai.File (RspFileInfo(..), getFileInfo, addContentHeadersForFilePart,
  conditionalRequest, createReadStreamRange)
import Network.HTTP.Types as H
import Network.HTTP.Wai.Header (HeaderMap)
import Network.HTTP.Wai.Internal (Request(..), Response(..), FilePart(..))

-- | Sending a HTTP response to Node.js's Response according to 'Response'.
-- Applications/middlewares MUST provide a proper 'H.ResponseHeaders'.
-- so that inconsistency does not happen. No header is deleted by this function.
sendResponse
  :: forall eff
   . NH.Response
  -> Request
  -> HeaderMap
  -> Response (WaiEffects eff)
  -> Aff (WaiEffects eff) Unit
sendResponse nresp (Request req) reqkydhdr response = do
  if hasBody status
    then do
      Tuple ms len <- sendRsp nresp ver status hs0 rsp
      case ms of
        Nothing -> pure unit
        Just realStatus ->
          -- to do log response here
          pure unit
    else do
      _ <- sendRsp nresp ver status hs0 RspNoBody
      pure unit
  where
    ver       = req.httpVersion
    status    = responseStatus response
    hs0       = responseHeaders response
    isHead    = req.method == H.HEAD
    rsp       = case response of
      ResponseFile _ _ fp mpart -> RspFile fp mpart reqkydhdr isHead
      ResponseBuffer _ _ buf
        | isHead     -> RspNoBody
        | otherwise  -> RspBuffer buf
      ResponseString _ _ enc str
        | isHead     -> RspNoBody
        | otherwise  -> RspString enc str
      ResponseStream _ _ stream
        | isHead     -> RspNoBody
        | otherwise  -> RspStream stream

data Rsp eff
  = RspNoBody
  | RspFile FilePath (Maybe FilePart) HeaderMap Boolean
  | RspBuffer B.Buffer
  | RspString Encoding String
  | RspStream (forall a. ST.Readable a eff)

sendRsp
  :: forall e
   . NH.Response
  -> H.HttpVersion
  -> H.Status
  -> H.ResponseHeaders
  -> Rsp (WaiEffects e)
  -> Aff (WaiEffects e) (Tuple (Maybe H.Status) (Maybe Int))
sendRsp nresp ver s hs RspNoBody = do
  let endCon = NH.responseAsStream nresp
  liftEff do
    NH.setStatusCode nresp (H.status2Number s)
    sendHeaders nresp hs
    ST.end endCon (pure unit)
    pure unit
  pure $ Tuple (Just s) Nothing
sendRsp nresp ver s hs (RspStream streaRead) = do
  let pipedTo = NH.responseAsStream nresp
  liftEff do
    NH.setStatusCode nresp (H.status2Number s)
    sendHeaders nresp hs
    _ <- ST.pipe streaRead pipedTo
    pure unit
  pure $ Tuple (Just s) Nothing
sendRsp nresp ver s hs (RspBuffer buff) = do
  let conn = NH.responseAsStream nresp
  le <- liftEff do
    NH.setStatusCode nresp (H.status2Number s)
    sendHeaders nresp hs
    _ <- ST.write conn buff (pure unit)
    ST.end conn (pure unit)
    bufferByteLength buff
  pure $ Tuple (Just s) (Just le)
sendRsp nresp ver s hs (RspString enc str) = do
  strBuff <- liftEff $ B.fromString str enc
  sendRsp nresp ver s hs (RspBuffer strBuff)
sendRsp nresp ver s hs (RspFile path (Just fpart@(FilePart part)) hm isHead) =
  let
    hs' = addContentHeadersForFilePart hs fpart
    beg = part.offset
    len = part.byteCount
  in
    sendRspFile2XX nresp ver s hs' path beg len isHead Nothing
sendRsp nresp ver _ hs (RspFile path Nothing hm isHead) = do
  efinfo <- attempt $ getFileInfo path
  case efinfo of
    Left _ ->
      sendRspFile404 nresp ver hs
    Right (Tuple stats finfo) ->
      case conditionalRequest finfo hs hm of
        WithoutBody s -> sendRsp nresp ver s hs RspNoBody
        WithBody s hs' beg len -> sendRspFile2XX nresp ver s hs' path beg len isHead (Just stats)

sendRspFile2XX
  :: forall e
   . NH.Response
  -> H.HttpVersion
  -> H.Status
  -> H.ResponseHeaders
  -> FilePath
  -> Int
  -> Int
  -> Boolean
  -> Maybe Stats
  -> Aff (WaiEffects e) (Tuple (Maybe H.Status) (Maybe Int))
sendRspFile2XX nresp ver s hs path beg len true _ = sendRsp nresp ver s hs RspNoBody
sendRspFile2XX nresp ver s hs path beg len _ (Just stats) = do
  if isDirectory stats
    then sendRspFile404 nresp ver hs
    else do
      liftEff do
        NH.setStatusCode nresp (H.status2Number s)
        sendHeaders nresp hs
      let conn = NH.responseAsStream nresp
          end  = max beg (beg + len - 1)
      rstream <- liftEff $ createReadStreamRange path beg end
      _ <- liftEff $ ST.pipe rstream conn
      pure $ Tuple (Just s) (Just len)
sendRspFile2XX nresp ver s hs path beg len _ Nothing = do
  fseither <- attempt $ FSA.stat path
  case fseither of
    Left _      -> sendRspFile404 nresp ver hs
    Right st    -> sendRspFile2XX nresp ver s hs path beg len false (Just st)

sendRspFile404
  :: forall e
   . NH.Response
  -> H.HttpVersion
  -> H.ResponseHeaders
  -> Aff (WaiEffects e) (Tuple (Maybe H.Status) (Maybe Int))
sendRspFile404 nresp ver hd =
  sendRsp nresp ver H.status404 hd' (RspString UTF8 body)
  where
    body = "File not found\n"
    hd'  = replaceHeader H.ContentType "text/plain; charset=utf-8" hd

hasBody :: H.Status -> Boolean
hasBody s = sc /= 204
         && sc /= 304
         && sc >= 200
  where
    sc = H.status2Number s

-- | Send headers
sendHeaders :: forall e. NH.Response -> H.ResponseHeaders -> Eff (http :: NH.HTTP | e) Unit
sendHeaders nresp = traverse_ $ uncurry (NH.setHeader nresp) <<< lmap show <<< H.header2Tuple

replaceHeader :: H.HeaderName -> String -> H.ResponseHeaders -> H.ResponseHeaders
replaceHeader k v hdrs = [H.Header k v] <> deleteBy ((==) `on` H.getHeaderName) (H.Header k v) hdrs

foreign import bufferByteLength :: forall e. B.Buffer -> Eff (buffer :: B.BUFFER | e) Int
