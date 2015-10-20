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
  refs[i] = Union{}
  push!(stack, i)
end

# utility to show a julia value in haskell
function show(x)
  Base.unsafe_convert(Cstring, repr(x))
end

function finalize_hs(p::Ptr{Void})
  ccall(:hs_free_stable_ptr, Void, (Ptr{Void},), p)
end

end
