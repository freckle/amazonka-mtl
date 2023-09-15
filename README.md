# amazonka-mtl

Haskell library template used at Freckle.

## Create your repo

```sh
gh repo create --template freckle/amazonka-mtl --public freckle/<name>
```

## Rename your package

```sh
sed -i s/amazonka-mtl/my-name/ ./**/*
```

## Enable release

When you are ready to release your library, simply remove the conditional from
the release workflow.

```diff
-      - if: false # Remove when ready to release
```

## Open repo up to [hacktoberfest][hacktoberfest] contributions

Add the `hacktoberfest` topic to your repo if

- you're planning on releasing it as open source, and
- you think it would benefit from and be amenable to public contributions

[hacktoberfest]: https://hacktoberfest.digitalocean.com/

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
