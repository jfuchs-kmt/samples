cmake_minimum_required(VERSION 3.21)

project(main LANGUAGES C CXX Fortran CUDA)

include(FortranCInterface)
FortranCInterface_VERIFY(CXX)

find_package(OpenMP REQUIRED COMPONENTS Fortran)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL "LLVMFlang")
    set(_OMP_OFFLOAD_FLAGS -fopenmp -fopenmp-version=52 -fopenmp-targets=nvptx64-nvidia-cuda)
    message("HIT LLVMFLANG")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
    set(_OMP_OFFLOAD_FLAGS -mp=gpu -static-nvidia)
    message("HIT NVHPC")
endif()

add_compile_options(
    $<$<CONFIG:Release>:-O3>
)

add_library(core STATIC
    src/kernel.cu
    src/core_mod.F90
)
include_directories(core "include")
target_compile_options(core PRIVATE 
    "$<$<COMPILE_LANGUAGE:Fortran>:${_OMP_OFFLOAD_FLAGS}>"
)

add_executable(main src/main.F90)
target_link_libraries(main PRIVATE core OpenMP::OpenMP_Fortran)
target_compile_options(main PRIVATE 
    "$<$<COMPILE_LANGUAGE:Fortran>:${_OMP_OFFLOAD_FLAGS}>"
)
target_link_options(main PRIVATE ${_OMP_OFFLOAD_FLAGS})
