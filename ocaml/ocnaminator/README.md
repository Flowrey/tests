# CNAMinator

## Getting started

First of all, create an opam switch:
```bash
opam switch create . 5.2.0 -y --deps-only
```

Install project's dependencies:
```bash
opam install -y . --deps-only --with-test
```

Build the JavaScript and bundle it with webpack:
```bash
dune build @melange
webpack --mode production --entry ./_build/default/src/output/src/main.js --output-path ./public/dist
```

