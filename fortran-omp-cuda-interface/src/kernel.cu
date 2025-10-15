#include "kernel.cuh"
#include <cuda_runtime.h>
#include <omp.h>
#include <stdio.h>

__global__ void fill_with_values(float* d_buf, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) {
        d_buf[i] = 42.0f;
    }
}

void fill_with_cuda(void* ptr, int n) {
    printf("========== kernel.cu ==========\n");
    printf("         N: %i\n", n);
    printf("  Host ptr: %p\n", ptr);
    
    void* d_ptr = omp_get_mapped_ptr(ptr, omp_get_default_device());
    float* d_buf = (float*) d_ptr;

    printf("Device ptr: %p\n", d_ptr);

    int threads = 256;
    int blocks = (n + threads - 1) / threads;
    printf("Running kernel...\n");
    fill_with_values<<<blocks, threads>>>(d_buf, n);
    printf("Synchronizing device...\n");
    cudaDeviceSynchronize();
    printf("============= end =============\n");
}
