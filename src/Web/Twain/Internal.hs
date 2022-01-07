module Web.Twain.Internal where

import Control.Exception (throwIO)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.List as L
import Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Types (Method, hCookie, status204)
import Network.Wai (Application, Middleware, Request, lazyRequestBody, queryString, requestHeaders, requestMethod, responseLBS)
import Network.Wai.Parse (File, ParseRequestBodyOptions, lbsBackEnd, parseRequestBodyEx)
import Web.Cookie (SetCookie, parseCookiesText, renderSetCookie)
import Web.Twain.Types

type MaxRequestSizeBytes = Int64

modifyTwainState :: (TwainState e -> TwainState e) -> TwainM e ()
modifyTwainState f = TwainM (\s -> ((), f s))

execTwain :: TwainM e a -> e -> TwainState e
execTwain (TwainM f) e = snd (f (TwainState [] e defaultOnExceptionResponse))

routeState :: RouteM e (RouteState e)
routeState = RouteM $ \s -> return (Right (s, s))

setRouteState :: RouteState e -> RouteM e ()
setRouteState s = RouteM $ \_ -> return (Right ((), s))

concatParams :: RouteState e -> [Param]
concatParams p =
  reqBodyParams p
    <> reqCookieParams p
    <> reqPathParams p
    <> reqQueryParams p

composeMiddleware :: [Middleware] -> Application
composeMiddleware = L.foldl' (\a m -> m a) emptyApp

routeMiddleware ::
  Maybe Method ->
  PathPattern ->
  RouteM e a ->
  e ->
  Middleware
routeMiddleware method pat (RouteM route) env app req respond =
  case match method pat req of
    Nothing -> app req respond
    Just pathParams -> do
      let st =
            RouteState
              { reqBodyParams = [],
                reqBodyFiles = [],
                reqPathParams = pathParams,
                reqQueryParams = decodeQueryParam <$> queryString req,
                reqCookieParams = cookieParams req,
                reqBodyJson = Left "missing JSON body",
                reqBodyParsed = False,
                reqEnv = env,
                reqWai = req
              }
      action <- route st
      case action of
        Left (Respond res) -> respond res
        _ -> app req respond

match :: Maybe Method -> PathPattern -> Request -> Maybe [Param]
match method (MatchPath f) req
  | maybe True (requestMethod req ==) method = f req
  | otherwise = Nothing

-- | Parse request body.
--
-- Use to customize `ParseRequestBodyOptions` before calls to `param`
-- and `file`.
parseBody :: ParseRequestBodyOptions -> RouteM e ([Param], [File BL.ByteString])
parseBody opts = do
  s <- routeState
  if reqBodyParsed s
    then return (concatParams s, reqBodyFiles s)
    else do
      (ps, fs) <- liftIO $ parseRequestBodyEx opts lbsBackEnd (reqWai s)
      let sb =
            s
              { reqBodyParams = decodeBsParam <$> ps,
                reqBodyFiles = fs,
                reqBodyParsed = True
              }
      setRouteState sb
      return (concatParams sb, reqBodyFiles sb)

parseBodyJson :: RouteM e (Either String JSON.Value)
parseBodyJson = do
  s <- routeState
  if reqBodyParsed s
    then return (reqBodyJson s)
    else do
      jsonE <- liftIO $ JSON.eitherDecode <$> lazyRequestBody (reqWai s)
      setRouteState $ s {reqBodyJson = jsonE, reqBodyParsed = True}
      return jsonE

cookieParams :: Request -> [Param]
cookieParams req =
  let headers = snd <$> L.filter ((==) hCookie . fst) (requestHeaders req)
   in join $ parseCookiesText <$> headers

setCookieByteString :: SetCookie -> B.ByteString
setCookieByteString setCookie =
  BL.toStrict (toLazyByteString (renderSetCookie setCookie))

decodeQueryParam :: (B.ByteString, Maybe B.ByteString) -> Param
decodeQueryParam (a, b) = (decodeUtf8 a, maybe "" decodeUtf8 b)

decodeBsParam :: (B.ByteString, B.ByteString) -> Param
decodeBsParam (a, b) = (decodeUtf8 a, decodeUtf8 b)

emptyApp :: Application
emptyApp req respond = respond $ responseLBS status204 [] ""
