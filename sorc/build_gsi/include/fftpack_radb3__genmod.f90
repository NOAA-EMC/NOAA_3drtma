        !COMPILER-GENERATED INTERFACE MODULE: Wed May 15 19:06:37 2019
        MODULE FFTPACK_RADB3__genmod
          INTERFACE 
            SUBROUTINE FFTPACK_RADB3(IDO,L1,CC,CH,WA1,WA2)
              INTEGER(KIND=4) :: L1
              INTEGER(KIND=4) :: IDO
              REAL(KIND=4) :: CC(IDO,3,L1)
              REAL(KIND=4) :: CH(IDO,L1,3)
              REAL(KIND=4) :: WA1(IDO)
              REAL(KIND=4) :: WA2(IDO)
            END SUBROUTINE FFTPACK_RADB3
          END INTERFACE 
        END MODULE FFTPACK_RADB3__genmod
