# Purescript Bridge example

This project demonstrates the libraries Purescript Bridge and [`input-output-hk/purescript-bridge-json-helpers`](https://github.com/input-output-hk/purescript-bridge-json-helpers.git).

It does not use [`purescript-argonaut-aeson-generic`](https://pursuit.purescript.org/packages/purescript-argonaut-aeson-generic) ([GitHub](https://github.com/coot/purescript-argonaut-aeson-generic)).

The Haskell type `Foo`, in `src/Types.hs`, is generated for Purescript by Purescript Bridge.  `purescript-bridge-json-helpers` is used to decode and encode this type, client-side.

# Dependencies
## Nix
The `nix develop` shell will provide Purescipt 0.15 and Spago.
## Without Nix
You must install Purescript 0.15 and Spago.

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

# Updating the Purescript Bridge
- Enter the `example` directory

- Regenerate the Purescript Bridge types:
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
