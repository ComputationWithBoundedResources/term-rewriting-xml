# term-rewriting-xml
XML parser for term-rewriting library

## Testing

The `.xml` and `.trs` parser can be tested by:

```
cabal configure --enable-tests
cabal install
cabal test testsuite --test-option="number-of-tests" --test-option="xml-folder" --test-option="trs-folder"

```

- `number-of-tests`: number of tests QuickCheck runs
- `xml-folder`: top-level folder with only `.xml` files. (searched recursively for all files)
- `trs-folder`: top level folder with same structure as `xml-folder` only containing `.trs` files


