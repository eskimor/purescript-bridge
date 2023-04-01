# Purescript Bridge example

This project demonstrates the libraries Purescript Bridge and [`purescript-argonaut-aeson-generic`](https://pursuit.purescript.org/packages/purescript-argonaut-aeson-generic) ([GitHub](https://github.com/coot/purescript-argonaut-aeson-generic)).

It needs Purescript 0.15.

The Haskell type `Foo`, in `src/Types.hs`, is generated for Purescript by Purescript Bridge.  Purescript Argonaut Aeson Generic is used to decode and encode this type, client-side.

In this directory:

- Generate the Javascript bundle:

```spago bundle-app --to static/index.js```

- `cabal run example`

- Open [http://localhost:8080/index.html](http://localhost:8080/index.html)

- Open the browser's developer console and look for the message received:

```
Foo message: Hello	 Foo number: 123    Foo list length: 11     Foo map size: 3
```

- Look at the server's logs for the message sent from the browser:

```
Foo message: Hola        Foo number: 124        Foo list length: 22      Foo Map length: 4
```

----------------

Regenerate the Purescript for the bridge type `Foo` with `cabal run generate-purescript`.

----------------

This Purescript Discourse thread assisted me: https://discourse.purescript.org/t/latest-and-greatest-haskell-purescript-serialization/1640
