# inline-julia
A haskell library for writing inline julia code

## Examples

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Language.Julia.Inline

main = do
  let l = [1,2,4,5] :: [Int]
  ([julia| map(x->x+1, $l) |] >>= jlList) :: IO [Int] -- returns [2,3,4,5,6]
```

## TODO:
- [x] Tests
- [x] Automatic marshaling of data types
- [ ] Typechecking julia code
- [ ] Infer return type from julia code
- [x] `$varname` with typeclass for easy passing of variables
- [ ] rewrite rules optimizations
- [ ] Better error messages
- [ ] Use precompilation
- [ ] don't use libffi
