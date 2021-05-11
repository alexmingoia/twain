# Twain

Twain is a tiny web application framework for WAI.

Twain provides routing, parameter parsing, and a reader-like monad for
composing responses from an environment. Also provided are helpers for
redirects, cookies, IO actions, and more.

```haskell
import Web.Twain

main :: IO ()
main = do
  twain 8080 "my app" $ do
    get "/" $ do
      send $ html "Hello, World!"
```

## Prior Work

Twain is heavily inspired by Scotty. Unlike Scotty, Twain does not use monad
transformers. Instead of providing a monad transformer, the `TwainM` and
`RouteM` monads are parametized by an environment, which can be accessed within
routes. This results in a simpler API with better performance.
