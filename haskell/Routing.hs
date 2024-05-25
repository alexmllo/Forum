
module Routing
where
import Develop.DatFw

import Network.Wai

import           Network.HTTP.Types as H
import           Develop.DatFw.Content
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


dispatchSite :: (WebApp site, ParseRoute site, ToTypedContent r)
             => (Route site -> HandlerFor site r) -> DispatchEnv site -> Application
dispatchSite handle env req = case parseRoute (pathInfo req, decodedQuery) of
        Just r  -> dispatchHandler (handle r) env (Just r) req
        Nothing -> dispatchHandler (notFound :: HandlerFor site ()) env Nothing req
    where
        decodedQuery :: [(T.Text, T.Text)]
        decodedQuery =
            let query = queryString req
                decode (n, mbv) = (T.decodeUtf8 n, maybe T.empty T.decodeUtf8 mbv)
            in decode <$> query

onMethods :: MonadHandler m => [(Method, m r)] -> m r
onMethods hlist = do
    let mmap = M.fromList hlist
        methmbf m = case M.lookup m mmap of
            Nothing -> M.lookup anyMethod mmap
            Just h  -> Just h
    meth <- requestMethod <$> getRequest
    case methmbf meth of
        Just h  -> h
        Nothing -> badMethod

whenMethod :: (Functor f, ToTypedContent o) => Method -> f o -> (Method, f TypedContent)
whenMethod m h = (m, fmap toTypedContent h)

whenAnyMethod :: (Functor f, ToTypedContent o) => f o -> (Method, f TypedContent)
whenAnyMethod h = (anyMethod, fmap toTypedContent h)

anyMethod :: Method
anyMethod = B8.pack ""

