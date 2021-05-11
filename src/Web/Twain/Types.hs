module Web.Twain.Types where

import Control.Exception (SomeException)
import Control.Monad (ap)
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
import Network.Wai (Middleware, Request, Response, pathInfo)
import Network.Wai.Handler.Warp (defaultOnExceptionResponse)
import Network.Wai.Parse (File, ParseRequestBodyOptions)
import Numeric.Natural

-- | TwainM provides a monad interface for composing routes and middleware.
newtype TwainM e a = TwainM (TwainState e -> (a, TwainState e))

data TwainState e
  = TwainState
      { middlewares :: [Middleware],
        environment :: e,
        onExceptionResponse :: (SomeException -> Response)
      }

instance Functor (TwainM e) where
  fmap f (TwainM g) = TwainM $ \s ->
    let (a, sb) = g s
     in (f a, sb)

instance Applicative (TwainM e) where
  pure = return
  (<*>) = ap

instance Monad (TwainM e) where
  return a = TwainM (\s -> (a, s))
  (TwainM m) >>= fn = TwainM $ \s ->
    let (a, sb) = m s
        (TwainM mb) = fn a
     in mb sb

modify :: (TwainState e -> TwainState e) -> TwainM e ()
modify f = TwainM (\s -> ((), f s))

exec :: TwainM e a -> e -> TwainState e
exec (TwainM f) e = snd (f (TwainState [] e defaultOnExceptionResponse))

-- | `RouteM` is a Reader-like monad that can "short-circuit" and return a WAI
-- response using a given environment. This provides convenient branching with
-- do notation for redirects, error responses, etc.
data RouteM e a
  = RouteM (RouteState e -> IO (Either RouteAction (a, RouteState e)))

data RouteAction
  = Respond Response
  | Next

data RouteState e
  = RouteState
      { reqBodyParams :: [Param],
        reqBodyFiles :: [File BL.ByteString],
        reqPathParams :: [Param],
        reqQueryParams :: [Param],
        reqCookieParams :: [Param],
        reqBodyJson :: Either String JSON.Value,
        reqBodyParsed :: Bool,
        reqEnv :: e,
        reqWai :: Request
      }

instance Functor (RouteM e) where
  fmap f (RouteM g) = RouteM $ \s -> mapRight (\(a, b) -> (f a, b)) `fmap` g s

instance Applicative (RouteM e) where
  pure = return
  (<*>) = ap

instance Monad (RouteM e) where
  return a = RouteM $ \s -> return (Right (a, s))
  (RouteM act) >>= fn = RouteM $ \s -> do
    eres <- act s
    case eres of
      Left ract -> return (Left ract)
      Right (a, sb) -> do
        let (RouteM fres) = fn a
        fres sb

instance MonadIO (RouteM e) where
  liftIO act = RouteM $ \s -> act >>= \a -> return (Right (a, s))

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
  parseParam :: Text -> Either Text a

  -- | Default implementation parses comma-delimited lists.
  parseParamList :: Text -> Either Text [a]
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
    _ -> Left "parseParam Char: no parse"
  parseParamList = Right . T.unpack -- String

instance ParsableParam () where
  parseParam t = if T.null t then Right () else Left "parseParam Unit: no parse"

instance (ParsableParam a) => ParsableParam [a] where parseParam = parseParamList

instance ParsableParam Bool where
  parseParam t =
    if t' == T.toCaseFold "true"
      then Right True
      else
        if t' == T.toCaseFold "false"
          then Right False
          else Left "parseParam Bool: no parse"
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
readEither :: Read a => Text -> Either Text a
readEither t = case [x | (x, "") <- reads (T.unpack t)] of
  [x] -> Right x
  [] -> Left "readEither: no parse"
  _ -> Left "readEither: ambiguous parse"
