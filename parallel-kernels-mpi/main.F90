PROGRAM main
    USE mpi_f08
    IMPLICIT none

    INTEGER :: ierr, rank, size
    CALL MPI_Init(ierr)
    CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

    CALL kernel(rank, size)
    CALL MPI_Finalize(ierr)
END PROGRAM main

SUBROUTINE kernel(rank, size)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: rank, size

    INTEGER :: i, n
    REAL :: sum

    n = 20
    sum = 0.0

    PRINT *, "Hello from rank", rank, "of", size

    !$omp target teams loop reduction(+:sum)
    DO i = 1, n
        sum = sum + 1.0 / REAL(i + rank + 1)
    END DO
    !$omp end target teams loop
END SUBROUTINE
