# Change Log

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
