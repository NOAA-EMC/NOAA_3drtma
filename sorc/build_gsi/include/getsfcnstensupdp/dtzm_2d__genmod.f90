        !COMPILER-GENERATED INTERFACE MODULE: Wed May 15 19:04:30 2019
        MODULE DTZM_2D__genmod
          INTERFACE 
            SUBROUTINE DTZM_2D(XT,XZ,DT_COOL,ZC,SLMSK,Z1,Z2,NX,NY,DTZM)
              INTEGER(KIND=4), INTENT(IN) :: NY
              INTEGER(KIND=4), INTENT(IN) :: NX
              REAL(KIND=4), INTENT(IN) :: XT(NX,NY)
              REAL(KIND=4), INTENT(IN) :: XZ(NX,NY)
              REAL(KIND=4), INTENT(IN) :: DT_COOL(NX,NY)
              REAL(KIND=4), INTENT(IN) :: ZC(NX,NY)
              REAL(KIND=4), INTENT(IN) :: SLMSK(NX,NY)
              REAL(KIND=4), INTENT(IN) :: Z1
              REAL(KIND=4), INTENT(IN) :: Z2
              REAL(KIND=4), INTENT(OUT) :: DTZM(NX,NY)
            END SUBROUTINE DTZM_2D
          END INTERFACE 
        END MODULE DTZM_2D__genmod
