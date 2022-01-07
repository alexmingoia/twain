module Web.Twain
  ( -- * Twain to WAI
    twain,
    twain',
    twainApp,

    -- * Middleware and Routes.
    middleware,
    get,
    put,
    patch,
    post,
    delete,
    notFound,
    onException,
    addRoute,

    -- * Request and Parameters.
    env,
    param,
    param',
    paramMaybe,
    params,
    parseBody,
    bodyJson,
    file,
    files,
    header,
    headers,
    request,

    -- * Responses.
    send,
    next,
    redirect301,
    redirect302,
    redirect303,
    text,
    html,
    json,
    xml,
    raw,
    status,
    withHeader,
    withCookie,
    withCookie',
    expireCookie,
    module Web.Twain.Types,
    module Network.HTTP.Types,
    module Network.Wai.Parse,
  )
where

import Control.Exception (SomeException)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON
import Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Either.Combinators (rightToMaybe)
import qualified Data.List as L
import Data.Text as T
import Data.Text.Encoding
import Data.Time
import Network.HTTP.Types
import Network.Wai (Application, Middleware, Request, Response, mapResponseHeaders, mapResponseStatus, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp (Port, Settings, defaultSettings, runEnv, runSettings, setOnExceptionResponse, setPort)
import Network.Wai.Parse (File (..), FileInfo (..), defaultParseRequestBodyOptions, setMaxRequestFilesSize, setMaxRequestParmsSize)
import System.Environment (lookupEnv)
import Web.Cookie
import Web.Twain.Internal
import Web.Twain.Types

-- | Run a Twain app on `Port` using the given environment.
--
-- If a PORT environment variable is set, it will take precendence.
--
-- > twain 8080 "My App" $ do
-- >   middleware logger
-- >   get "/" $ do
-- >     appTitle <- env
-- >     send $ text ("Hello from " <> appTitle)
-- >   get "/greetings/:name"
-- >     name <- param "name"
-- >     send $ text ("Hello, " <> name)
-- >   notFound $ do
-- >     send $ status status404 $ text "Not Found"
twain :: Port -> e -> TwainM e () -> IO ()
twain port env m = do
  mp <- lookupEnv "PORT"
  let p = maybe port read mp
      st = exec m env
      app = composeMiddleware $ middlewares st
      handler = onExceptionResponse st
      settings' = setOnExceptionResponse handler $ setPort p defaultSettings
  runSettings settings' app

-- | Run a Twain app passing Warp `Settings`.
twain' :: Settings -> e -> TwainM e () -> IO ()
twain' settings env m = do
  let st = exec m env
      app = composeMiddleware $ middlewares st
      settings' = setOnExceptionResponse (onExceptionResponse st) settings
  runSettings settings' app

-- | Create a WAI `Application` from a Twain app and environment.
twainApp :: e -> TwainM e () -> Application
twainApp env m = composeMiddleware $ middlewares $ exec m env

-- | Use the given middleware. The first declared is the outermost middleware
-- (it has first access to request and last action on response).
middleware :: Middleware -> TwainM e ()
middleware m = modify (\st -> st {middlewares = m : middlewares st})

get :: PathPattern -> RouteM e a -> TwainM e ()
get = addRoute (Just "GET")

put :: PathPattern -> RouteM e a -> TwainM e ()
put = addRoute (Just "PUT")

patch :: PathPattern -> RouteM e a -> TwainM e ()
patch = addRoute (Just "PATCH")

post :: PathPattern -> RouteM e a -> TwainM e ()
post = addRoute (Just "POST")

delete :: PathPattern -> RouteM e a -> TwainM e ()
delete = addRoute (Just "DELETE")

-- | Add a route if nothing else is found. This matches any request, so it
-- should go last.
notFound :: RouteM e a -> TwainM e ()
notFound = addRoute Nothing (MatchPath (const (Just [])))

-- | Render a `Response` on exceptions.
onException :: (SomeException -> Response) -> TwainM e ()
onException handler = modify $ \st -> st {onExceptionResponse = handler}

-- | Add a route matching `Method` (optional) and `PathPattern`.
addRoute :: Maybe Method -> PathPattern -> RouteM e a -> TwainM e ()
addRoute method pat route =
  modify $ \st ->
    let m = routeMiddleware method pat route (environment st)
     in st {middlewares = m : middlewares st}

-- | Get the app environment.
env :: RouteM e e
env = RouteM $ \st -> return $ Right (reqEnv st, st)

-- | Get a parameter. Looks in query, path, cookie, and body (in that order).
--
-- If no parameter is found, or parameter fails to parse, `next` is called
-- which passes control to subsequent routes and middleware.
param :: ParsableParam a => Text -> RouteM e a
param name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> params
  maybe next (either (const next) pure . parseParam) pM

-- | Get a parameter or error if missing or parse failure.
param' :: ParsableParam a => Text -> RouteM e (Either Text a)
param' name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> params
  return $ maybe (Left ("missing parameter: " <> name)) parseParam pM

-- | Get an optional parameter. `Nothing` is returned for missing parameter or
-- parse failure.
paramMaybe :: ParsableParam a => Text -> RouteM e (Maybe a)
paramMaybe name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> params
  return $ maybe Nothing (rightToMaybe . parseParam) pM

-- | Get all parameters from query, path, cookie, and body (in that order).
params :: RouteM e [Param]
params = fst <$> parseBody defaultParseRequestBodyOptions

-- | Get uploaded `FileInfo`.
file :: Text -> RouteM e (Maybe (FileInfo BL.ByteString))
file name = fmap snd . L.find ((==) (encodeUtf8 name) . fst) <$> files

-- | Get all uploaded files.
files :: RouteM e [File BL.ByteString]
files = snd <$> parseBody defaultParseRequestBodyOptions

-- | Get the value of a request `Header`. Header names are case-insensitive.
header :: Text -> RouteM e (Maybe Text)
header name = do
  let ciname = CI.mk (encodeUtf8 name)
  fmap (decodeUtf8 . snd) . L.find ((==) ciname . fst) <$> headers

-- | Get the request headers.
headers :: RouteM e [Header]
headers = requestHeaders <$> request

-- | Get the JSON value from request body.
bodyJson :: JSON.FromJSON a => RouteM e (Either String a)
bodyJson = do
  jsonE <- parseBodyJson
  case jsonE of
    Left e -> return (Left e)
    Right v -> case JSON.fromJSON v of
      JSON.Error e -> return (Left e)
      JSON.Success a -> return (Right a)

-- | Get the WAI `Request`.
request :: RouteM e Request
request = reqWai <$> routeState

-- | Send a `Response`.
--
-- > send $ text "Hello, World!"
--
-- Send an `html` response:
--
-- > send $ html "<h1>Hello, World!</h1>"
--
-- Modify the `status`:
--
-- > send $ status status404 $ text "Not Found"
--
-- Send a response `withHeader`:
--
-- > send $ withHeader (hServer, "Twain + Warp") $ text "Hello"
--
-- Send a response `withCookie`:
--
-- > send $ withCookie "key" "val" $ text "Hello"
send :: Response -> RouteM e a
send res = RouteM $ \_ -> return $ Left (Respond res)

-- | Pass control to the next route or middleware.
next :: RouteM e a
next = RouteM $ \_ -> return (Left Next)

-- | Construct a `Text` response.
--
-- Sets the Content-Type and Content-Length headers.
text :: Text -> Response
text body =
  let lbs = BL.fromStrict (encodeUtf8 body)
      typ = (hContentType, "text/plain; charset=utf-8")
      len = (hContentLength, Char8.pack (show (BL.length lbs)))
   in raw status200 [typ, len] lbs

-- | Construct an HTML response.
--
-- Sets the Content-Type and Content-Length headers.
html :: BL.ByteString -> Response
html body =
  let lbs = body
      typ = (hContentType, "text/html; charset=utf-8")
      len = (hContentLength, Char8.pack (show (BL.length lbs)))
   in raw status200 [typ, len] lbs

-- | Construct a JSON response using `ToJSON`.
--
-- Sets the Content-Type and Content-Length headers.
json :: ToJSON a => a -> Response
json val =
  let lbs = JSON.encode val
      typ = (hContentType, "application/json; charset=utf-8")
      len = (hContentLength, Char8.pack (show (BL.length lbs)))
   in raw status200 [typ, len] lbs

-- | Construct an XML response.
--
-- Sets the Content-Type and Content-Length headers.
xml :: BL.ByteString -> Response
xml body =
  let lbs = body
      typ = (hContentType, "application/xml; charset=utf-8")
      len = (hContentLength, Char8.pack (show (BL.length lbs)))
   in raw status200 [typ, len] lbs

-- | Construct a raw response from a lazy `ByteString`.
raw :: Status -> [Header] -> BL.ByteString -> Response
raw status headers body = responseLBS status headers body

-- | Set the `Status` for a `Response`.
status :: Status -> Response -> Response
status s = mapResponseStatus (const s)

-- | Add a `Header` to response.
withHeader :: Header -> Response -> Response
withHeader header = mapResponseHeaders (header :)

-- | Add a cookie to the response with the given key and value.
--
-- Note: This uses `defaultSetCookie`.
withCookie :: Text -> Text -> Response -> Response
withCookie key val res =
  let setCookie =
        defaultSetCookie
          { setCookieName = encodeUtf8 key,
            setCookieValue = encodeUtf8 val
          }
      header = (CI.mk "Set-Cookie", setCookieByteString setCookie)
   in mapResponseHeaders (header :) res

-- | Add a `SetCookie` to the response.
withCookie' :: SetCookie -> Response -> Response
withCookie' setCookie res =
  let header = (CI.mk "Set-Cookie", setCookieByteString setCookie)
   in mapResponseHeaders (header :) res

-- | Add a header to expire (unset) a cookie with the given key.
expireCookie :: Text -> Response -> Response
expireCookie key res = do
  let zeroTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
      setCookie =
        defaultSetCookie
          { setCookieName = encodeUtf8 key,
            setCookieExpires = Just zeroTime
          }
      header = (CI.mk "Set-Cookie", setCookieByteString setCookie)
   in mapResponseHeaders (header :) res

-- | Create a redirect response with 301 status (Moved Permanently).
redirect301 :: Text -> Response
redirect301 url = raw status301 [(hLocation, encodeUtf8 url)] ""

-- | Create a redirect response with 302 status (Found).
redirect302 :: Text -> Response
redirect302 url = raw status302 [(hLocation, encodeUtf8 url)] ""

-- | Create a redirect response 303 status (See Other).
redirect303 :: Text -> Response
redirect303 url = raw status303 [(hLocation, encodeUtf8 url)] ""
