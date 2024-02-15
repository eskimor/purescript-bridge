# Regenerate everything
reset && stack run generate-purescript && mv src/Argonaut/* example/src/Argonaut && mv src/JsonHelpers/* example/src/JsonHelpers;  cd example && spago bundle --bundle-type app --outfile static/index.js; cd ..

# Regenerate purescript files
reset && cd example && spago bundle --bundle-type app --outfile static/index.js; cd ..

# Roundtrip server example

## Tab 1
stack run example

## Tab 2
cd example/static
parcel serve index.html
