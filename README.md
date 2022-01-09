# Twain

[![Hackage](https://img.shields.io/hackage/v/twain.svg?style=flat)](http://hackage.haskell.org/package/twain)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)

Twain is a tiny web application framework for
[WAI](http://hackage.haskell.org/package/wai) that provides the `ResponderM`
monad for composing responses, and helpers for routing and parameter parsing.

`ResponderM` is an Either-like monad that can "short-circuit" and return a
response, or pass control to the next middleware. This provides convenient
branching with do notation for redirects, error responses, etc.

Twain also includes:

- Routing with path captures that decompose into WAI middleare.
- Parameter parsing from cookies, path, query, and body.
- Helpers for redirects, headers, status codes, and errors.

```haskell
import Network.Wai.Handler.Warp (run)
import Web.Twain

main :: IO ()
main = do
  run 8080
    $ get "/" (send $ html "Hello World!")
    $ notFound (send $ html "Not found...")
```
