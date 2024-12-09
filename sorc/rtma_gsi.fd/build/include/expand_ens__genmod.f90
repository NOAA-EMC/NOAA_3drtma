        !COMPILER-GENERATED INTERFACE MODULE: Wed Dec  4 18:44:03 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE EXPAND_ENS__genmod
          INTERFACE 
            SUBROUTINE EXPAND_ENS(NEIG,NANALS,ENS_ORIG,ENS_EXPANDED,    &
     &EVECTORS)
              INTEGER(KIND=4), INTENT(IN) :: NANALS
              INTEGER(KIND=4), INTENT(IN) :: NEIG
              REAL(KIND=4), INTENT(IN) :: ENS_ORIG(NANALS)
              REAL(KIND=4), INTENT(OUT) :: ENS_EXPANDED(NEIG*NANALS)
              REAL(KIND=8), INTENT(IN) :: EVECTORS(NEIG)
            END SUBROUTINE EXPAND_ENS
          END INTERFACE 
        END MODULE EXPAND_ENS__genmod
