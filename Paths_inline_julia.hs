module Path_inline_julia where

getDataFileName :: FilePath -> IO FilePath
getDataFileName "HaskellGC.jl" = return "./julia/HaskellGC.jl"
