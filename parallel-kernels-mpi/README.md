# Parallel Kernels on GPU using MPI #

Runs multiple GPU kernels in parallel invoked by OpenMP offloading through different MPI ranks.

Requires correctly setup llvm toolchain, CUDA and OpenMPI environment. Building ucx from source with CUDA explicitly enabled may be necessary. Compile with:
```
OMPI_FC=flang mpifort -O3 -fopenmp -fopenmp-targets=nvptx64-nvidia-cuda main.F90
```

Run with:
```
OMPI_MCA_pml=ucx OMP_TARGET_OFFLOAD=mandatory mpirun -n 4 ./a.out
```
The environment flag `OMPI_MCA_pml=ucx` was necessary to avoid a failing call to `cuMemHostRegister`.

