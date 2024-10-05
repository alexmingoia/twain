module Web.Twain.Internal where

import Control.Exception (Exception, handle, throwIO)
import Control.Monad (join)
import Control.Monad.Catch (throwM, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text as T
import Data.Text.Encoding
import qualified Data.Vault.Lazy as V
import Data.Word (Word64)
import Network.HTTP.Types (Method, hCookie, mkStatus, status204, status400, status413, status500)
import Network.HTTP2.Frame (ErrorCodeId (..))
import Network.Wai (Application, Middleware, Request (..), lazyRequestBody, queryString, requestHeaders, requestMethod, responseLBS)
import Network.Wai.Parse (File, ParseRequestBodyOptions, lbsBackEnd, noLimitParseRequestBodyOptions, parseRequestBodyEx)
import Network.Wai.Request (RequestSizeException (..), requestSizeCheck)
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie (SetCookie, parseCookiesText, renderSetCookie)
import Web.Twain.Types
import Network.HTTP2.Client (HTTP2Error (..), ErrorCode(..))
import Data.Word (Word32)
import qualified Data.ByteString.Char8 as BC

parsedReqKey :: V.Key ParsedRequest
parsedReqKey = unsafePerformIO V.newKey
{-# NOINLINE parsedReqKey #-}

responderOptsKey :: V.Key ResponderOptions
responderOptsKey = unsafePerformIO V.newKey
{-# NOINLINE responderOptsKey #-}

defaultResponderOpts :: ResponderOptions
defaultResponderOpts =
  ResponderOptions
    { optsMaxBodySize = 64000,
      optsParseBody = noLimitParseRequestBodyOptions
    }

getRequest :: ResponderM Request
getRequest = ResponderM $ \r -> return (Right (r, r))

setRequest :: Request -> ResponderM ()
setRequest r = ResponderM $ \_ -> return (Right ((), r))

concatParams :: ParsedRequest -> [Param]
concatParams
  ParsedRequest
    { preqBody = Just (FormBody (fps, _)),
      preqCookieParams = cps,
      preqPathParams = pps,
      preqQueryParams = qps
    } = qps <> pps <> cps <> fps
concatParams preq =
  preqQueryParams preq <> preqPathParams preq <> preqCookieParams preq

parseRequest :: Request -> ParsedRequest
parseRequest req =
  case V.lookup parsedReqKey (vault req) of
    Just preq -> preq
    Nothing ->
      ParsedRequest
        { preqPathParams = [],
          preqQueryParams = decodeQueryParam <$> queryString req,
          preqCookieParams = parseCookieParams req,
          preqBody = Nothing
        }

match :: Maybe Method -> PathPattern -> Request -> Maybe [Param]
match method (MatchPath f) req
  | maybe True (requestMethod req ==) method = f req
  | otherwise = Nothing

-- | Parse form request body.
parseBodyForm :: ResponderM ParsedRequest
parseBodyForm = do
  req <- getRequest
  let preq = fromMaybe (parseRequest req) $ V.lookup parsedReqKey (vault req)
  case preqBody preq of
    Just (FormBody _) -> return preq
    _ -> do
      let optsM = optsParseBody <$> V.lookup responderOptsKey (vault req)
          opts = fromMaybe noLimitParseRequestBodyOptions optsM
      (ps, fs) <- liftIO $ wrapErr $ parseRequestBodyEx opts lbsBackEnd req
      let parsedBody = FormBody (decodeBsParam <$> ps, fs)
          preq' = preq {preqBody = Just parsedBody}
      setRequest $ req {vault = V.insert parsedReqKey preq' (vault req)}
      return preq'

-- | Parse JSON request body.
parseBodyJson :: ResponderM JSON.Value
parseBodyJson = do
  req <- getRequest
  let preq = fromMaybe (parseRequest req) $ V.lookup parsedReqKey (vault req)
  case preqBody preq of
    Just (JSONBody json) -> return json
    _ -> do
      jsonE <- liftIO $ wrapErr $ JSON.eitherDecode <$> lazyRequestBody req
      case jsonE of
        Left msg -> throwM $ HttpError status400 msg
        Right json -> do
          let preq' = preq {preqBody = Just (JSONBody json)}
          setRequest $ req {vault = V.insert parsedReqKey preq' (vault req)}
          return json

wrapErr :: IO a -> IO a
wrapErr = handle wrapMaxReqErr . handle wrapParseErr

wrapMaxReqErr :: RequestSizeException -> IO a
wrapMaxReqErr (RequestSizeException max) =
  throwIO $ HttpError status413 $
    "Request body size larger than " <> show max <> " bytes."


wrapParseErr :: HTTP2Exception -> IO a
wrapParseErr (HTTP2Exception (ErrorCode code)) = do
  let statusCode = fromIntegral code
      statusMsg = BC.pack $ "HTTP/2 error: " ++ show code
      status = mkStatus statusCode statusMsg
      errorMsg = unpack $ decodeUtf8 statusMsg
  throwIO $ HttpError status errorMsg


parseCookieParams :: Request -> [Param]
parseCookieParams req =
  let headers = snd <$> L.filter ((==) hCookie . fst) (requestHeaders req)
   in join $ parseCookiesText <$> headers

setCookieByteString :: SetCookie -> B.ByteString
setCookieByteString setCookie =
  BL.toStrict (toLazyByteString (renderSetCookie setCookie))

decodeQueryParam :: (B.ByteString, Maybe B.ByteString) -> Param
decodeQueryParam (a, b) = (decodeUtf8 a, maybe "" decodeUtf8 b)

decodeBsParam :: (B.ByteString, B.ByteString) -> Param
decodeBsParam (a, b) = (decodeUtf8 a, decodeUtf8 b)
