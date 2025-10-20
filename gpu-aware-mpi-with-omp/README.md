# GPU Aware MPI with OpenMP #

Performs a GPU aware MPI call using device buffers. Using the OpenMP clause `use_device_addr` allows the code to still run on the CPU if offloading is not enabled via compiler flags.

Requires correctly setup compiler, CUDA and OpenMPI environment. Building ucx from source with CUDA explicitly enabled may be necessary. OpenMPI should be built with the same compiler toolchain used for ucx and the main file.

An environment with a sufficient compiler, CUDA and OpenMPI must be setup, then compile the example with:
```
mkdir build && cd build
cmake .. && cmake --build .
```

Run with:
```
OMP_TARGET_OFFLOAD=mandatory mpirun -n 4 ./main
```
The expected result is a sendbuf full of `10.0`. This comes from the summation of all process ids in each array element.

Query OpenMPI for CUDA support:
```
ompi_info --parsable -l 9 --all | grep mpi_built_with_cuda_support:value
```
