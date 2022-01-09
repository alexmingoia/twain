# Twain

[![Hackage](https://img.shields.io/hackage/v/twain.svg?style=flat)](http://hackage.haskell.org/package/twain)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)

Twain is a tiny web application framework for
[WAI](http://hackage.haskell.org/package/wai).

- `ResponderM` for composing responses with do notation.
- Routing with path captures that decompose `ResponderM` into middleware.
- Parameter parsing from cookies, path, query, and body.
- Helpers for redirects, headers, status codes, and errors.

```haskell
import Network.Wai.Handler.Warp (run)
import Web.Twain
  
index :: ResponderM a
index = send $ html "Hello World!"
  
echoName :: ResponderM a
echoName = do
  name <- param "name"
  send $ html $ "Hello, " <> name
  
missing :: ResponderM a
missing = send $ html "Not found..."
 
main :: IO ()
main = do
  run 8080
    $ get "/" index
    $ post "/echo/:name" echoName
    $ notFound missing
```
