-- | Twain is a tiny web application framework for WAI
--
-- - `ResponderM` for composing responses with do notation.
-- - Routing with path captures that decompose `ResponderM` into middleware.
-- - Parameter parsing for cookies, path, query, and body.
-- - Helpers for redirects, headers, status codes, and errors.
--
-- @
-- import Network.Wai.Handler.Warp (run)
-- import Web.Twain
--
-- main :: IO ()
-- main = do
--   run 8080
--     $ get "/" index
--     $ post "/echo/:name" echo
--     $ notFound missing
--
-- index :: ResponderM a
-- index = send $ html "Hello World!"
--
-- echo :: ResponderM a
-- echo = do
--   name <- param "name"
--   send $ html $ "Hello, " <> name
--
-- missing :: ResponderM a
-- missing = send $ html "Not found..."
-- @
module Web.Twain
  ( ResponderM,

    -- * Routing
    get,
    put,
    patch,
    post,
    delete,
    route,
    notFound,

    -- * Request and Parameters
    param,
    paramEither,
    paramMaybe,
    params,
    file,
    fileMaybe,
    files,
    fromBody,
    header,
    headers,
    request,

    -- * Responses
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

    -- * Errors
    HttpError (..),
    onException,

    -- * Middleware
    withParseBodyOpts,
    withMaxBodySize,

    -- * Re-exports
    module Network.HTTP.Types,
    module Network.Wai,
    FileInfo (..),
  )
where

import Control.Exception (SomeException, handle)
import Control.Monad.Catch (throwM)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as JSON
import Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Either.Combinators (rightToMaybe)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Text.Encoding
import Data.Time
import qualified Data.Vault.Lazy as V
import Data.Word (Word64)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp hiding (FileInfo)
import Network.Wai.Parse hiding (Param)
import Network.Wai.Request
import System.Environment (lookupEnv)
import Web.Cookie
import Web.Twain.Internal
import Web.Twain.Types

get :: PathPattern -> ResponderM a -> Middleware
get = route (Just "GET")

put :: PathPattern -> ResponderM a -> Middleware
put = route (Just "PUT")

patch :: PathPattern -> ResponderM a -> Middleware
patch = route (Just "PATCH")

post :: PathPattern -> ResponderM a -> Middleware
post = route (Just "POST")

delete :: PathPattern -> ResponderM a -> Middleware
delete = route (Just "DELETE")

-- | Route request matching optional `Method` and `PathPattern` to `ResponderM`.
route :: Maybe Method -> PathPattern -> ResponderM a -> Middleware
route method pat (ResponderM responder) app req respond = do
  let maxM = optsMaxBodySize <$> V.lookup responderOptsKey (vault req)
  req' <- maybe (pure req) (flip requestSizeCheck req) maxM
  case match method pat req' of
    Nothing -> app req' respond
    Just pathParams -> do
      let preq = parseRequest req'
          preq' = preq {preqPathParams = pathParams}
          req'' = req' {vault = V.insert parsedReqKey preq' (vault req')}
      eres <- responder req''
      case eres of
        Left (Respond res) -> respond res
        _ -> app req'' respond

-- | Respond if no other route responds.
--
-- Sets the status to 404.
notFound :: ResponderM a -> Application
notFound (ResponderM responder) req respond = do
  let preq = parseRequest req
      req' = req {vault = V.insert parsedReqKey preq (vault req)}
  eres <- responder req'
  case eres of
    Left (Respond res) -> respond $ mapResponseStatus (const status404) res
    _ -> respond $ status status404 $ text "Not found."

onException :: (SomeException -> ResponderM a) -> Middleware
onException h app req respond = do
  handle handler $ app req respond
  where
    handler err = do
      let preq = parseRequest req
          req' = req {vault = V.insert parsedReqKey preq (vault req)}
      let (ResponderM responder) = h err
      eres <- responder req'
      case eres of
        Left (Respond res) -> respond res
        _ -> app req' respond

-- | Specify maximum request body size in bytes.
--
-- Defaults to 64KB.
withMaxBodySize :: Word64 -> Middleware
withMaxBodySize max app req respond = do
  let optsM = V.lookup responderOptsKey (vault req)
      opts = fromMaybe defaultResponderOpts optsM
      opts' = opts {optsMaxBodySize = max}
  let req' = req {vault = V.insert responderOptsKey opts' (vault req)}
  app req' respond

-- | Specify `ParseRequestBodyOptions` to use when parsing request body.
withParseBodyOpts :: ParseRequestBodyOptions -> Middleware
withParseBodyOpts parseBodyOpts app req respond = do
  let optsM = V.lookup responderOptsKey (vault req)
      opts = fromMaybe defaultResponderOpts optsM
      opts' = opts {optsParseBody = parseBodyOpts}
  let req' = req {vault = V.insert responderOptsKey opts' (vault req)}
  app req' respond

-- | Get a parameter. Looks in query, path, cookie, and body (in that order).
--
-- If no parameter is found, or parameter fails to parse, `next` is called
-- which passes control to subsequent routes and middleware.
param :: ParsableParam a => Text -> ResponderM a
param name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> params
  maybe next (either (const next) pure . parseParam) pM

-- | Get a parameter or error if missing or parse failure.
paramEither :: ParsableParam a => Text -> ResponderM (Either HttpError a)
paramEither name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> params
  return $ case pM of
    Nothing ->
      Left $ HttpError status400 ("missing parameter: " <> T.unpack name)
    Just p -> parseParam p

-- | Get an optional parameter.
--
-- Returns `Nothing` for missing parameter.
-- Throws `HttpError` on parse failure.
paramMaybe :: ParsableParam a => Text -> ResponderM (Maybe a)
paramMaybe name = do
  pM <- fmap snd . L.find ((==) name . fst) <$> params
  return $ maybe Nothing (rightToMaybe . parseParam) pM

-- | Get all parameters from query, path, cookie, and body (in that order).
params :: ResponderM [Param]
params = concatParams <$> parseBodyForm

-- | Get uploaded `FileInfo`.
--
-- If missing parameter or empty file, pass control to subsequent routes and
-- middleware.
file :: Text -> ResponderM (FileInfo BL.ByteString)
file name = maybe next pure =<< fileMaybe name

-- | Get optional uploaded `FileInfo`.
--
-- `Nothing` is returned for missing parameter or empty file content.
fileMaybe :: Text -> ResponderM (Maybe (FileInfo BL.ByteString))
fileMaybe name = do
  fM <- fmap snd . L.find ((==) (encodeUtf8 name) . fst) <$> files
  case fileContent <$> fM of
    Nothing -> return Nothing
    Just "" -> return Nothing
    Just _ -> return fM

-- | Get all uploaded files.
files :: ResponderM [File BL.ByteString]
files = fs . preqBody <$> parseBodyForm
  where
    fs bodyM = case bodyM of
      Just (FormBody (_, fs)) -> fs
      _ -> []

-- | Get the JSON value from request body.
fromBody :: JSON.FromJSON a => ResponderM a
fromBody = do
  json <- parseBodyJson
  case JSON.fromJSON json of
    JSON.Error msg -> throwM $ HttpError status400 msg
    JSON.Success a -> return a

-- | Get the value of a request `Header`. Header names are case-insensitive.
header :: Text -> ResponderM (Maybe Text)
header name = do
  let ciname = CI.mk (encodeUtf8 name)
  fmap (decodeUtf8 . snd) . L.find ((==) ciname . fst) <$> headers

-- | Get the request headers.
headers :: ResponderM [Header]
headers = requestHeaders <$> request

-- | Get the WAI `Request`.
request :: ResponderM Request
request = getRequest

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
send :: Response -> ResponderM a
send res = ResponderM $ \_ -> return $ Left (Respond res)

-- | Pass control to the next route or middleware.
next :: ResponderM a
next = ResponderM $ \_ -> return (Left Next)

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
--
-- Sets the Content-Length header if missing.
raw :: Status -> [Header] -> BL.ByteString -> Response
raw status headers body =
  if L.any ((hContentLength ==) . fst) headers
    then responseLBS status headers body
    else
      let len = (hContentLength, Char8.pack (show (BL.length body)))
       in responseLBS status (len : headers) body

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
            setCookieValue = encodeUtf8 val,
            setCookiePath = Just "/",
            setCookieHttpOnly = True
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
            setCookiePath = Just "/",
            setCookieHttpOnly = True,
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
