PROGRAM main
    USE core_mod
    
    IMPLICIT NONE
    
    CALL omp_info()
    CALL init()
    CALL run()
    CALL finish()
END PROGRAM main
