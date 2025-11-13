cmake_minimum_required(VERSION 3.21)

project(gpu-mpi LANGUAGES C CXX Fortran)

include(FortranCInterface)
FortranCInterface_VERIFY(CXX)

find_package(OpenMP)
find_package(MPI COMPONENTS C Fortran REQUIRED)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(_OMP_OFFLOAD_FLAGS -fopenmp -foffload=nvptx-none -no-pie)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "LLVMFlang")
    set(_OMP_OFFLOAD_FLAGS -fopenmp -fopenmp-version=52 -fopenmp-targets=nvptx64-nvidia-cuda)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
    set(_OMP_OFFLOAD_FLAGS -mp=gpu)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
    set(_OMP_OFFLOAD_FLAGS -fiopenmp -fopenmp-targets=spir64)
endif()

add_compile_options(
    $<$<CONFIG:Release>:-O3>
)

add_executable(main main.F90)
target_link_libraries(main PRIVATE MPI::MPI_Fortran)
target_link_libraries(main PRIVATE OpenMP::OpenMP_Fortran)
target_compile_options(main PRIVATE ${_OMP_OFFLOAD_FLAGS})
target_link_options(main PRIVATE ${_OMP_OFFLOAD_FLAGS})
target_include_directories(main
    PRIVATE ${MPI_Fortran_INCLUDE_PATH}
)
