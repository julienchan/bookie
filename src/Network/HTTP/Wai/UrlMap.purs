module Network.HTTP.Wai.UrlMap where

import Prelude

import Data.Foldable (foldl, intercalate)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

import Network.HTTP.Types (status404, contentType)
import Network.HTTP.Wai (Application, Request(..), reqPathInfo, responseStrUtf8)

import URI.Query (print) as Query

type Path = List String

newtype UrlMap = UrlMap (List (Tuple Path Application))

instance semigroupUrlMap :: Semigroup UrlMap where
  append (UrlMap a) (UrlMap b) = UrlMap (a <> b)

instance monoidUrlMap :: Monoid UrlMap where
  mempty = UrlMap Nil

mount' :: Path -> Application -> UrlMap
mount' prefix thing = UrlMap $ (Tuple prefix thing : Nil)

mount :: String -> Application -> UrlMap
mount prefix = mount' (prefix:Nil)

mountRoot :: Application -> UrlMap
mountRoot = mount' Nil

match :: forall a. Path -> List (Tuple Path a) -> Maybe (Tuple Path a)
match xs tuples = foldl go Nothing tuples
  where
    go (Just x) _ = Just x
    go _ (Tuple prefix y) = stripPrefix prefix xs >>= \xs' -> pure (Tuple xs' y)

toApplication :: UrlMap -> Application
toApplication (UrlMap urlmap) req sendResponse =
  case match (reqPathInfo req) urlmap of
    Just (Tuple newPath app) ->
      app (modifyReqPath newPath req) sendResponse
    Nothing ->
      sendResponse $ responseStrUtf8 status404 ([contentType "text/plain"]) "Not found\n"

modifyReqPath :: Path -> Request -> Request
modifyReqPath pa (Request oreq) =
  let
    newRaw = "/" <> intercalate "/" pa <> fromMaybe mempty (Query.print <$> oreq.query)
  in
    Request $
      oreq { rawPathInfo = newRaw
           , pathInfo = pa }

stripPrefix :: forall a. Eq a => List a -> List a -> Maybe (List a)
stripPrefix Nil ys = Just ys
stripPrefix (x:xs) (y:ys)
  | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing
