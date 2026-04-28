# yesod-middleware-csp

A middleware for building CSP headers on the fly

Deals with CSP without disabling it.
This is done by overriding the default yesod
provided addScript functionalities and adding
a nonce to the tag, and the right headers to the request.

## Usage

Because there is no good way of enforcing CSP
at typelevel in yesod,
it's best to hide the addScript functions from
yesod with the ones provided by this library:

```haskell
import Yesod hiding (addScript, addScriptRemote)
import Yesod.Middleware.CSP (addScript, addScriptRemote, addCSPMiddleware)
```

Then wire up the middleware in your `Yesod` instance:

```haskell
instance Yesod App where
  yesodMiddleware = addCSPMiddleware
```

## How to run tests

```
nix build
```

## Contributing

PR's are welcome.
