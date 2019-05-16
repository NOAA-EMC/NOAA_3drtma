        !COMPILER-GENERATED INTERFACE MODULE: Wed May 15 19:06:37 2019
        MODULE FFTPACK_RADF5__genmod
          INTERFACE 
            SUBROUTINE FFTPACK_RADF5(IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
              INTEGER(KIND=4) :: L1
              INTEGER(KIND=4) :: IDO
              REAL(KIND=4) :: CC(IDO,L1,5)
              REAL(KIND=4) :: CH(IDO,5,L1)
              REAL(KIND=4) :: WA1(IDO)
              REAL(KIND=4) :: WA2(IDO)
              REAL(KIND=4) :: WA3(IDO)
              REAL(KIND=4) :: WA4(IDO)
            END SUBROUTINE FFTPACK_RADF5
          END INTERFACE 
        END MODULE FFTPACK_RADF5__genmod
