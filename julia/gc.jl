module HaskellGC

export retain, release

refs = Any[]
stack = Int64[]

# retain a reference
function retain(r)
  if length(stack) == 0
    push!(refs, r)
    length(refs) :: Int64
  else
    ind = pop!(stack)
    refs[ind] = r
    ind :: Int64
  end
end

# release a held reference
function release(i::Int64)
  refs[i] = None
  push!(stack, i)
end

# utility to show a julia value in haskell
function show(x)
  convert(Ptr{Uint8}, repr(x))
end

end
