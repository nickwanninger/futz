# Query Converter
This package converts a subset of SQL to an s-expression form of "relational algebra"

To build, run 
```bash
make
```

and to run:
```
cabal run exe:qconv q.sql
```

or, you can access the binary in `dist-newstyle` once it is built.
If it fails to build for any reason, make sure you have `bnfc` installed, and have run `cabal update`.