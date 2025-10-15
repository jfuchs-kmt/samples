# Fortran OpenMP Cuda Interface #

Minimal example for an interface connecting Fortran OpenMP code with CUDA in C.
Verified to compile with latest `LLVM`, `NVHPC 25.9` and `CUDA 13.0`.

Requires correctly setup compiler and CUDA environment. Compile with:
```
mkdir build && cd build
cmake .. && cmake --build .
```

Run with:
```
OMP_TARGET_OFFLOAD=mandatory ./main
```

### Heterogeneous Memory Management (HMM)

Depending on the GPU architecture the code may still run with `OMP_TARGET_OFFLOAD=disabled`. In that case, host and device pointer will be equal as OpenMP does not allocate and associate additional variables on the GPU. However, CUDA is still able to write the array on system memory due to HMM. HMM emulates Unified Shared Memory (USM) hardware capabilities in software, which is also the reason as to why the kernel is slower by multiple orders of magnitude when run this way.

Querying for HMM is possible with e.g.
```
$ nvidia-smi -q | grep HMM
$     Addressing Mode                       : HMM
```
