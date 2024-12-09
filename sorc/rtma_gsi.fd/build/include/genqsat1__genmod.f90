        !COMPILER-GENERATED INTERFACE MODULE: Wed Dec  4 18:44:03 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GENQSAT1__genmod
          INTERFACE 
            SUBROUTINE GENQSAT1(SPH,QSAT,GES_PRSL,GES_TV,ICE,NPTS,NLEVS)
              INTEGER(KIND=4), INTENT(IN) :: NLEVS
              INTEGER(KIND=4), INTENT(IN) :: NPTS
              REAL(KIND=4), INTENT(IN) :: SPH(NPTS,NLEVS)
              REAL(KIND=8), INTENT(OUT) :: QSAT(NPTS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: GES_PRSL(NPTS,NLEVS)
              REAL(KIND=4), INTENT(IN) :: GES_TV(NPTS,NLEVS)
              LOGICAL(KIND=4), INTENT(IN) :: ICE
            END SUBROUTINE GENQSAT1
          END INTERFACE 
        END MODULE GENQSAT1__genmod
