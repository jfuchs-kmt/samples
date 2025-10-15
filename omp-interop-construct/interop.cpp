#include <stdio.h>
#include <omp.h>

#ifdef _WITH_CUDA_
#include <cuda.h>
#endif

// Detailed error code information
// https://www.openmp.org/spec-html/5.1/openmpse42.html#x236-2690032
static void print_if_error(const char* msg, int err) {
    if (err < 0) {
        printf("%s", msg);
        exit(1);
    }
}

int main() {
    omp_interop_t iobj = omp_interop_none;
#pragma omp interop init(target: iobj)
    
    int err;
    omp_intptr_t fr_id = omp_get_interop_int(iobj, omp_ipr_fr_id, &err);
    print_if_error("Error getting fr id", err);

    const char* rt_name = omp_get_interop_str(iobj, omp_ipr_fr_name, &err);
    print_if_error("Error getting rt name", err);

    omp_intptr_t vendor = omp_get_interop_int(iobj, omp_ipr_vendor, &err);
    print_if_error("Error getting vendor", err);

    const char* vendor_name = omp_get_interop_str(iobj, omp_ipr_vendor_name, &err);
    print_if_error("Error getting vendor name", err);

    omp_intptr_t device_num = omp_get_interop_int(iobj, omp_ipr_device_num, &err);
    print_if_error("Error getting device num", err);

    void* h_platform = omp_get_interop_ptr(iobj, omp_ipr_platform, &err);
    print_if_error("Error getting platform handle", err);

    void* h_device = omp_get_interop_ptr(iobj, omp_ipr_device, &err);
    print_if_error("Error getting device handle", err);

    void* h_device_context = omp_get_interop_ptr(iobj, omp_ipr_device_context, &err);
    print_if_error("Error getting device context handle", err);

    printf("======= Queried properties from interop handle =======\n");
    printf("         Foreign runtime id: %i\n", static_cast<int>(fr_id));
    printf("    Name of foreign runtime: %s\n", rt_name);
    printf("  Foreign runtime vendor id: %i\n", static_cast<int>(vendor));
    printf("Foreign runtime vendor name: %s\n", vendor_name);
    printf("        device number in rt: %i\n", static_cast<int>(device_num));
    printf("=====================================================\n");
    printf("      Handle to foreign rt platform: %p\n", h_platform);
    printf("        Handle to foreign rt device: %p\n", h_device);
    printf("Handle to foreign rt device context: %p\n", h_device_context);
    printf("=====================================================\n\n");


    int num_interop_properties = omp_get_num_interop_properties(iobj);

    printf("========== (Optional) Interop properties ============\n");
    printf("Num. properties available to interop handle: %i\n", num_interop_properties);
    printf("=====================================================\n");
    // If there are any properties, query them with
    // omp_get_interop_name, omp_get_interop_type_desc, omp_get_interop_rc_desc

#ifdef _WITH_CUDA_
    CUcontext cu_ctx = static_cast<CUcontext>(h_device_context);
    unsigned int context_version = 0;
    cuCtxGetApiVersion(cu_ctx, &context_version);
    unsigned int flags = 0;
    cuCtxGetFlags(&flags);
    CUfunc_cache cache_config;
    cuCtxGetCacheConfig(&cache_config);

    printf("\nCUDA context capability API version: %u\n\n", context_version);

    CUdevice dev = 0;
    cuCtxGetDevice(&dev);
    char name[256] = {0};
    cuDeviceGetName(name, sizeof(name), dev);
    size_t total_mem = 0;
    cuDeviceTotalMem(&total_mem, dev);
    char pci_bus_id[64] = {0};
    cuDeviceGetPCIBusId(pci_bus_id, sizeof(pci_bus_id), dev);

    int mpCount = 0, clockKHz = 0;
    int cc_major = 0, cc_minor = 0;
    int max_threads_per_block = 0, max_block_dim_x = 0, max_block_dim_y = 0, max_block_dim_z = 0;
    int max_grid_dim_x = 0, max_grid_dim_y = 0, max_grid_dim_z = 0;
    int max_shared_mem_per_block = 0;
    int total_const_memory = 0;
    int warp_size = 0;
    int max_threads_per_mp = 0;
    cuDeviceGetAttribute(&mpCount, CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT, dev);
    cuDeviceGetAttribute(&clockKHz, CU_DEVICE_ATTRIBUTE_CLOCK_RATE, dev);
    cuDeviceGetAttribute(&cc_major, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR, dev);
    cuDeviceGetAttribute(&cc_minor, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR, dev);
    cuDeviceGetAttribute(&max_threads_per_block, CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_BLOCK, dev);
    cuDeviceGetAttribute(&max_block_dim_x, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_X, dev);
    cuDeviceGetAttribute(&max_block_dim_y, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Y, dev);
    cuDeviceGetAttribute(&max_block_dim_z, CU_DEVICE_ATTRIBUTE_MAX_BLOCK_DIM_Z, dev);
    cuDeviceGetAttribute(&max_grid_dim_x, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_X, dev);
    cuDeviceGetAttribute(&max_grid_dim_y, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Y, dev);
    cuDeviceGetAttribute(&max_grid_dim_z, CU_DEVICE_ATTRIBUTE_MAX_GRID_DIM_Z, dev);
    cuDeviceGetAttribute(&max_shared_mem_per_block, CU_DEVICE_ATTRIBUTE_MAX_SHARED_MEMORY_PER_BLOCK, dev);
    cuDeviceGetAttribute(&total_const_memory, CU_DEVICE_ATTRIBUTE_TOTAL_CONSTANT_MEMORY, dev);
    cuDeviceGetAttribute(&warp_size, CU_DEVICE_ATTRIBUTE_WARP_SIZE, dev);
    cuDeviceGetAttribute(&max_threads_per_mp, CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_MULTIPROCESSOR, dev);

    float kb = 1024.0;
    float gb = kb * kb * kb * kb;
    printf("========== Device (owner of this context) ===========\n");
    printf("                       Name: %s\n", name);
    printf("         Compute capability: %i.%i\n", cc_major, cc_minor);
    printf("        Total global memory: %.2f GB\n", total_mem / gb);
    printf("                        SMs: %i\n", mpCount);
    printf("                 Core clock: %.2f MHz\n", clockKHz / 1000.0);
    printf("      Max threads per block: %i\n", max_threads_per_block);
    printf("         Max threads per MP: %i\n", max_threads_per_mp);
    printf("              Max block dim: (%i %i %i)\n", max_block_dim_x, max_block_dim_y, max_block_dim_z);
    printf("               Max grid dim: (%i %i %i)\n", max_grid_dim_x, max_grid_dim_y, max_grid_dim_z);
    printf("Max shared memory per block: %.1f KB\n", max_shared_mem_per_block / kb);
    printf("      Total constant memory: %.1f KB\n", total_const_memory / kb);
    printf("                  Warp size: %i\n", warp_size);
    printf("=====================================================\n");
#endif

#pragma omp interop destroy(iobj)
}
