# Twain

[![Hackage](https://img.shields.io/hackage/v/twain.svg?style=flat)](http://hackage.haskell.org/package/twain)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)

Twain is a tiny web application framework for [WAI](http://hackage.haskell.org/package/wai).

- Simple routing with path captures.
- Parameter parsing of cookies, path, query, and body.
- Compose responses from an app environment using a reader-like monad.
- Helpers for redirects, headers, status codes.
- Routes decompose into WAI middleware.

```haskell
import Web.Twain

main :: IO ()
main = do
  twain 8080 "my app" $ do
    get "/" $ do
      send $ html "Hello, World!"
```
