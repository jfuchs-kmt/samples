# GPU Aware MPI with OpenMP #

Performs a GPU aware MPI call using device buffers. Using the OpenMP clause `use_device_addr` allows the code to still run on the CPU if offloading is not enabled via compiler flags.

Requires a compiler toolchain capable of GPU aware MPI. Compile with:
```
mkdir build && cd build
cmake .. && cmake --build .
```

Run with:
```
OMP_TARGET_OFFLOAD=mandatory mpirun -n 4 ./main
```
The expected result is a sendbuf of `6.0` values computed by the summation of all rank ids.

