# Purescript Bridge example

This project demonstrates the libraries Purescript Bridge and [Purescript Argonaut Aeson Generic](https://github.com/coot/purescript-argonaut-aeson-generic).

The Haskell type `Foo`, in `src/Types.hs`, is generated for Purescript by Purescript Bridge.  Purescript Argonaut Aeson Generic is used to decode and encode this type, client-side.

In this directory:

- Generate the Javascript bundle:

```spago bundle-app --to static/index.js```

- `cabal run example`

- Open http://localhost:8080/index.html

- Open the browser's developer console and look for the message received:

```
Foo message: Hello	 Foo number: 123    Foo list length: 11
```

- Look at the server's logs for the message sent from the browser:

```
Foo message: Hola        Foo number: 124        Foo list length: 22
```

----------------

Regenerate the Purescript for the bridge type `Foo` with `cabal run generate-purescript`.

----------------

This Purescript Discourse thread assisted me: https://discourse.purescript.org/t/latest-and-greatest-haskell-purescript-serialization/1640
