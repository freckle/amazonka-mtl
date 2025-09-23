# amazonka-mtl

[![Hackage](https://img.shields.io/hackage/v/amazonka-mtl.svg?style=flat)](https://hackage.haskell.org/package/amazonka-mtl)
[![Stackage Nightly](http://stackage.org/package/amazonka-mtl/badge/nightly)](http://stackage.org/nightly/package/amazonka-mtl)
[![Stackage LTS](http://stackage.org/package/amazonka-mtl/badge/lts)](http://stackage.org/lts/package/amazonka-mtl)
[![CI](https://github.com/freckle/amazonka-mtl/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/amazonka-mtl/actions/workflows/ci.yml)

MTL-style type-class and deriving-via newtypes for Amazonka.

## Example

This package allows incorporation of AWS actions into any MTL-style function,

<!--
```haskell
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Prelude

import Amazonka.S3 (BucketName)
import Control.Monad.Reader (MonadReader)
```
-->

```haskell
import Amazonka.Data.Text (ToText(..))
import Amazonka.S3.ListObjects
import Amazonka.S3.Types.Object
import Blammo.Logging
import Conduit
import Control.Lens hiding ((.=))
import Control.Monad.AWS as AWS
```

<!--
```haskell
data Settings = Settings
  { settingsBucketName :: BucketName
  }

class HasSettings env where
  settingsL :: Lens' env Settings

instance HasSettings Settings where
  settingsL = id
```
-->

```haskell
someAction
  :: ( MonadIO m
     , MonadLogger m
     , MonadAWS m
     , MonadReader env m
     , HasSettings env
     )
  => m ()
someAction = do
  Settings {..} <- view settingsL

  keys <-
    runConduit
      $ paginate (newListObjects settingsBucketName)
      .| concatMapC (^. listObjectsResponse_contents)
      .| concatC
      .| mapC (^. object_key . to toText)
      .| iterMC (\k -> logDebug $ k :# [])
      .| sinkList

  logInfo $ "Bucket contents" :# ["keys" .= keys]
```

<!--
```haskell
main :: IO ()
main = pure ()
```
-->

This package also provides a number of options for execution:

- Through a concrete transformer: `Control.Monad.AWS.EnvT`
- Through your own reader env and deriving-via: `Control.Monad.AWS.ViaReader`

This package also provides mechanisms for mocking AWS in tests:

- Through a concrete transformer: `Control.Monad.AWS.MockT`
- Through your own reader env and deriving-via: `Control.Monad.AWS.ViaMock`

Please see the [documentation on hackage][hackage] for all the details.

[hackage]: https://hackage.haskell.org/package/amazonka-mtl

## Development & Tests

```console
stack build --fast --pedantic --test --file-watch
```

## Release

To trigger a release, merge a commit to `main` that follows [Conventional
Commits][]. In short,

- `fix:` to trigger a patch release,
- `feat:` for minor, and
- `<type>!:` or add a `BREAKING CHANGE:` footer to trigger major

We don't enforce conventional commits generally (though you are free do so),
it's only required if you want to trigger release.

[conventional commits]: https://www.conventionalcommits.org/en/v1.0.0/#summary

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
