MODULE core_mod
    USE omp_lib

    IMPLICIT NONE

    REAL, ALLOCATABLE, TARGET :: buf(:)
    INTEGER, PARAMETER :: size = 1000

    INTERFACE
        SUBROUTINE fill_with_cuda(buffer, n) BIND(C)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int, c_ptr
            TYPE(c_ptr), VALUE :: buffer
            INTEGER(c_int), VALUE :: n
        END SUBROUTINE
    END INTERFACE
CONTAINS
    SUBROUTINE omp_info()
        PRINT*, "OpenMP device info"
        PRINT*, "  Num devices:   ", omp_get_num_devices()
        PRINT*, "  Default device:", omp_get_default_device()
    END SUBROUTINE omp_info

    SUBROUTINE init()
        ALLOCATE(buf(size))
        buf = 0.0
        !$omp target enter data map(to: buf)        
    END SUBROUTINE init

    SUBROUTINE run()
        USE iso_c_binding, ONLY: c_loc

        CALL fill_with_cuda(c_loc(buf), size)

        PRINT*, "Updating values to host with OpenMP..."
        !$omp target update from(buf)
        PRINT*, "  Values:", buf
    END SUBROUTINE run

    SUBROUTINE finish()
        !$omp target exit data map(delete: buf)
        DEALLOCATE(buf)
    END SUBROUTINE finish
END MODULE core_mod
