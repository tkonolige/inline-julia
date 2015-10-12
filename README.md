# inline-julia
A haskell library for writing inline julia code

## Examples

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Language.Julia.Inline

main = do
  let x = "Hello World"
  [julia| println($(hsString x)) |]

  l :: [Int] <- [julia| [1,2,3,4] |] >>= jlList
```

## TODO:
- [ ] Tests
- [ ] Automatic marshaling of data types
- [ ] Typechecking julia code
- [ ] Infer return type from julia code
