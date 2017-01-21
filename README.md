This is just a simple example of a counter application in thermite.

## Building

> only works with purescript 0.10.5

```bash
psc-package update
psc-package build
psc-bundle output/**/*.js -m Main --main Main > dist/Main.bundle.js
browserify dist/Main.bundle.js > dist/Main.browserify.js
```

index.html loads dist/Main.bundle.js relatively.
