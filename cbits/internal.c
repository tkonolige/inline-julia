#include "julia/julia.h"
#include <dlfcn.h>
#include <stdio.h>

void* jl_array_data1(jl_value_t* a) {
  return jl_array_data(a);
}
