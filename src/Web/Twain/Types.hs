module Web.Twain.Types where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (ap)
import Control.Monad.Catch hiding (throw, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Either.Combinators (mapRight)
import Data.Int
import Data.List as L
import Data.String (IsString, fromString)
import Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import Data.Word
import Network.HTTP.Types (Status, status400)
import Network.Wai (Middleware, Request, Response, pathInfo)
import Network.Wai.Parse (File, ParseRequestBodyOptions)
import Numeric.Natural

-- | `ResponderM` is an Either-like monad that can "short-circuit" and return a
-- response, or pass control to the next middleware. This provides convenient
-- branching with do notation for redirects, error responses, etc.
data ResponderM a
  = ResponderM (Request -> IO (Either RouteAction (a, Request)))

data RouteAction
  = Respond Response
  | Next

data ParsedRequest
  = ParsedRequest
      { preqBody :: Maybe ParsedBody,
        preqCookieParams :: [Param],
        preqPathParams :: [Param],
        preqQueryParams :: [Param]
      }

data ResponderOptions
  = ResponderOptions
      { optsMaxBodySize :: Word64,
        optsParseBody :: ParseRequestBodyOptions
      }

data ParsedBody
  = FormBody ([Param], [File BL.ByteString])
  | JSONBody JSON.Value

instance Functor ResponderM where
  fmap f (ResponderM g) = ResponderM $ \r -> mapRight (\(a, b) -> (f a, b)) `fmap` g r

instance Applicative ResponderM where
  pure = return
  (<*>) = ap

instance Monad ResponderM where
  return a = ResponderM $ \r -> return (Right (a, r))
  (ResponderM act) >>= fn = ResponderM $ \r -> do
    eres <- act r
    case eres of
      Left ract -> return (Left ract)
      Right (a, r') -> do
        let (ResponderM fres) = fn a
        fres r'

instance MonadIO ResponderM where
  liftIO act = ResponderM $ \r -> act >>= \a -> return (Right (a, r))

instance MonadThrow ResponderM where
  throwM = liftIO . throwIO

instance MonadCatch ResponderM where
  catch (ResponderM act) f = ResponderM $ \r -> do
    ea <- try (act r)
    case ea of
      Left e ->
        let (ResponderM h) = f e
         in h r
      Right a -> pure a

data HttpError = HttpError Status String
  deriving (Eq, Show)

instance Exception HttpError

type Param = (Text, Text)

data PathPattern = MatchPath (Request -> Maybe [Param])

instance IsString PathPattern where
  fromString s = MatchPath (matchPath (T.pack s))

matchPath :: Text -> Request -> Maybe [Param]
matchPath path req =
  go (splitPath path) (pathInfo req) (Just [])
  where
    splitPath = L.filter (not . T.null) . T.split (== '/')
    go (p : ps) (r : rs) m@(Just pms) =
      if not (T.null p) && T.head p == ':'
        then go ps rs (Just ((T.drop 1 p, r) : pms))
        else if p == r then go ps rs m else Nothing
    go [] [] pms = pms
    go _ _ _ = Nothing

-- | Parse values from request parameters.
class ParsableParam a where
  parseParam :: Text -> Either HttpError a

  -- | Default implementation parses comma-delimited lists.
  parseParamList :: Text -> Either HttpError [a]
  parseParamList t = mapM parseParam (T.split (== ',') t)

-- ParsableParam class and instance code is from Andrew Farmer and Scotty
-- framework, with slight modifications.

instance ParsableParam TL.Text where parseParam = Right . TL.fromStrict

instance ParsableParam T.Text where parseParam = Right

instance ParsableParam B.ByteString where parseParam = Right . encodeUtf8

instance ParsableParam BL.ByteString where parseParam = Right . BL.fromStrict . encodeUtf8

instance ParsableParam Char where
  parseParam t = case T.unpack t of
    [c] -> Right c
    _ -> Left $ HttpError status400 "parseParam Char: no parse"
  parseParamList = Right . T.unpack -- String

instance ParsableParam () where
  parseParam t =
    if T.null t
      then Right ()
      else Left $ HttpError status400 "parseParam Unit: no parse"

instance (ParsableParam a) => ParsableParam [a] where parseParam = parseParamList

instance ParsableParam Bool where
  parseParam t =
    if t' == T.toCaseFold "true"
      then Right True
      else
        if t' == T.toCaseFold "false"
          then Right False
          else Left $ HttpError status400 "parseParam Bool: no parse"
    where
      t' = T.toCaseFold t

instance ParsableParam Double where parseParam = readEither

instance ParsableParam Float where parseParam = readEither

instance ParsableParam Int where parseParam = readEither

instance ParsableParam Int8 where parseParam = readEither

instance ParsableParam Int16 where parseParam = readEither

instance ParsableParam Int32 where parseParam = readEither

instance ParsableParam Int64 where parseParam = readEither

instance ParsableParam Integer where parseParam = readEither

instance ParsableParam Word where parseParam = readEither

instance ParsableParam Word8 where parseParam = readEither

instance ParsableParam Word16 where parseParam = readEither

instance ParsableParam Word32 where parseParam = readEither

instance ParsableParam Word64 where parseParam = readEither

instance ParsableParam Natural where parseParam = readEither

-- | Useful for creating 'ParsableParam' instances for things that already implement 'Read'.
readEither :: Read a => Text -> Either HttpError a
readEither t = case [x | (x, "") <- reads (T.unpack t)] of
  [x] -> Right x
  [] -> Left $ HttpError status400 "readEither: no parse"
  _ -> Left $ HttpError status400 "readEither: ambiguous parse"
