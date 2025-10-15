# GPU Aware MPI with OpenMP #

More notes on requirements tbd.

Requires correctly setup compiler, CUDA and OpenMPI environment. Compile with:
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
