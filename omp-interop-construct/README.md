# OpenMP Interoperability #

Small example on how to use the OpenMP interoperability feature with foreign runtimes. Tested with latest `LLVM` and `CUDA 13.0`. `NVHPC 25.9` does not seem to support OpenMP interoperability constructs.
Querying from the OpenMP interop object is not possible in Fortran (no documented subroutines in Standard). Thus this example is written in C++.

Requires correctly setup compiler and CUDA environment. Compile with:
```
clang -fopenmp -fopenmp-targets=nvptx64-nvidia-cuda interop.cpp
```
If CUDA is available on your device, you can optionally add `-D_WITH_CUDA_ -lcuda` to query information about the CUDA context and device (used by OpenMP).

Run with:
```
OMP_TARGET_OFFLOAD=mandatory ./a.out
```
