# yesod-middleware-csp

Deals with CSP without disabling it.
This is done by overriding the default yesod
provided addScript functionalities and adding
a nonce to the tag, and the right headers to the request.

## Usage

Because there is no good way of enforcing CSP
at typelevel in yesod,
It's best to override classy prelude with your
own custom prelude.
This allows hiding the addScript functions from
there with the ones provided by this library:

```haskell

-- | Mirrors classy prelude yesod but with our supercede patches
module Supercede.Prelude.Yesod
  ( -- * rexport
    module X
  -- ** use CSP variant instead of yesod's
  , addScriptEither
  , addScript
  , addScriptRemote
  ) where

import Supercede.Prelude as X hiding (delete, deleteBy, Handler (..))
import Yesod as X hiding (addScriptEither, addScript, addScriptRemote, addScriptAttrs, addScriptRemoteAttrs)

import Yesod.Middleware.CSP (addScriptEither, addScript, addScriptRemote)

```

Then in hlint you can simply dis-recommend usage of classy prelude:

```haskell
- modules:
  - {name: [ClassyPrelude], message: "Use Supercede.Prelude instead"}
  - {name: [ClassyPrelude.Yesod], message: "Use Supercede.Prelude.Yesod instead"}
```

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

PR's are welcome.
