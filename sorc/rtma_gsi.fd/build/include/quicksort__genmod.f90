        !COMPILER-GENERATED INTERFACE MODULE: Wed Dec  4 18:44:32 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE QUICKSORT__genmod
          INTERFACE 
            SUBROUTINE QUICKSORT(N,X,IND)
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: X(N)
              INTEGER(KIND=4), INTENT(INOUT) :: IND(N)
            END SUBROUTINE QUICKSORT
          END INTERFACE 
        END MODULE QUICKSORT__genmod
