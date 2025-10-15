PROGRAM main
    USE MPI_f08
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: int32

    IMPLICIT NONE

    INTEGER(int32) :: myid
    REAL, ALLOCATABLE, DIMENSION(:) :: sendbuf, recvbuf
    INTEGER, PARAMETER :: N = 10

    CALL init_mpi()
    CALL init_data()

    !$omp target data use_device_addr(sendbuf, recvbuf)
    CALL MPI_Allreduce(sendbuf, recvbuf, N, MPI_REAL, MPI_SUM, MPI_COMM_WORLD)
    !$omp end target data

    !$omp target update from(recvbuf)
    IF (myid == 0) THEN
        PRINT*, "recvbuf (myid=0):"
        PRINT*, recvbuf
        PRINT*, "sendbuf (myid=0):"
        PRINT*, sendbuf
    ENDIF

    CALL finalize_all()
CONTAINS
    SUBROUTINE init_data()
        INTEGER :: i

        ALLOCATE(sendbuf(N), source=REAL(myid))
        ALLOCATE(recvbuf(N), source=0.0)
        !$omp target enter data map(alloc: sendbuf, recvbuf)
        !$omp target update to(sendbuf, recvbuf)

        ! Fill receive buffer with some initial value to check overrides in the end
        !$omp target teams loop
        DO i = 1, N
            recvbuf(i) = -1.0
        END DO
        !$omp end target teams loop
    END SUBROUTINE init_data

    SUBROUTINE init_mpi()
        CHARACTER(LEN=MPI_MAX_LIBRARY_VERSION_STRING) :: version    
        INTEGER(int32) :: numprocs
        INTEGER :: length


        CALL MPI_Init()
        CALL MPI_Comm_rank(MPI_COMM_WORLD, myid)
        CALL MPI_Comm_size(MPI_COMM_WORLD, numprocs)

        ! Taken straight from MGLET :)
        IF (myid == 0) THEN
            WRITE(*, '("MPI INFORMATION:")')
            WRITE(*, '("    Processes:     ", I0)') numprocs
            CALL MPI_Get_library_version(version, length)
            IF (version(length:length) == NEW_LINE('a')) THEN
                length = length - 1
            END IF
            WRITE(*, '("    MPI version:   ", A)') version(1:length)
            WRITE(*, '()')
        END IF
    END SUBROUTINE init_mpi

    SUBROUTINE finalize_all()
        !$omp target exit data map(delete: sendbuf, recvbuf)
        DEALLOCATE(sendbuf)
        DEALLOCATE(recvbuf)
        CALL MPI_Finalize()
    END SUBROUTINE finalize_all
END PROGRAM main
