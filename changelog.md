# Change Log

## 2.2.0.1 [2024-11-05]

- Fix failing compilation: Use qualified import of Data.Text.

## 2.2.0.0 [2024-10-12]

- Bump version bounds for http2 to >=5.0 && <5.4

## 2.1.2.0 [2023-04-10]

- Bump version bounds for transformers so Twain can be built with GHC 9.6.1

## 2.1.1.0 [2022-08-14]

- Add getters for query, path, and cookie params.

## 2.1.0.0 [2022-04-22]

- Remove ghc flag and add default-language.
- Add tests
- Add CSS response

## 2.0.0.0 [2022-01-09]

Simplify API to decompose routes into WAI middleware.

### Breaking changes

- Removed `TwainM` monad in favor of composing WAI middleware.
- Replace `RouteM` with `ResponderM`, no longer parametized by environment in
  preference for middleware utilizing request vault.
- Replace string errors with new `HttpError`.
- Rename param and file functions to be consistent.
  - Renamed `param'` to `paramEither`.
  - Renamed `file` to `fileMaybe`.
  - Added `file` which matches `param` functionality.
  - Changed behavior of `paramMaybe` to throw `HttpError`.
- Set default cookie path and http-only.

### Added

- Add additional helpers `withMaxBodySize` and `withParseBodyOpts` for limiting
  request body parsing.

## 1.0.1.0 [2021-09-15]

- Export `parseBody` to allow custom `ParseRequestBodyOptions`.

## 1.0.0.0 [2021-05-11]

- Initial release.
