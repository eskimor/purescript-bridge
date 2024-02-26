# PureScript Bridge example

This project demonstrates:
- PureScript Bridge
- [`purescript-argonaut-aeson-generic`](https://pursuit.purescript.org/packages/purescript-argonaut-aeson-generic) ([GitHub](https://github.com/coot/purescript-argonaut-aeson-generic))
- [`input-output-hk/purescript-bridge-json-helpers`](https://github.com/input-output-hk/purescript-bridge-json-helpers.git)

The Haskell type `Foo`, in `src/Types.hs`, is generated for PureScript by PureScript Bridge. Some of values in `Foo` are randomly generated every time the page is loaded.

On page load, the client:
- requests a `Foo` from the server
- decodes this value using `purescript-argonaut-aeson-generic`
- modifies some of the values
- encodes this with `purescript-argonaut-aeson-generic`, and sends it back to the server
- repeats these steps, encoding and decoding instead with `json-helpers`

# Dependencies
## Nix
The `nix develop` shell will provide PureScript 0.15 and Spago.
## Without Nix
You must install PureScript 0.15 and [spago-legacy](https://github.com/purescript/spago-legacy).

# Running the example
- Enter the `example` directory

- With Nix:
```
nix run
```

- Or without Nix:
```
cabal run example
```

- Open [http://localhost:8080/index.html](http://localhost:8080/index.html)

- Open the browser's developer console and look for the message received:
```
Foo message: Hello	 Foo number: 123    Foo list length: 11
```

- Look at the server's logs for the message sent from the browser:

```
Foo message: Hola        Foo number: 124        Foo list length: 22
```

# Updating the PureScript Bridge
- Enter the `example` directory

- Regenerate the PureScript Bridge types:
```
cabal run generate-purescript
```

- Generate the Javascript bundle:
```
spago bundle-app --to static/index.js
```
- or:
```
spago bundle-app --watch --to static/index.js
```

- Restart the server


More discussion: https://discourse.purescript.org/t/latest-and-greatest-haskell-purescript-serialization/1640
